//! Built-in native functions for the Lox VM
const std = @import("std");
const Value = @import("value.zig").Value;
const ObjClosure = @import("value.zig").ObjClosure;
const ProcessStatus = @import("value.zig").ProcessStatus;
const RestartPolicy = @import("value.zig").RestartPolicy;
const VM = @import("vm.zig").VM;

/// Register all built-in native functions in the VM's global scope.
pub fn register(vm: *VM) !void {
    try vm.defineNative("clock", clockNative);
    try vm.defineNative("spawn", spawnNative);
    try vm.defineNative("await", awaitNative);
    try vm.defineNative("receive", receiveNative);
    try vm.defineNative("tryReceive", tryReceiveNative);
    try vm.defineNative("send", sendNative);
    try vm.defineNative("self", selfNative);
    try vm.defineNative("yield", yieldNative);
    try vm.defineNative("abort", abortNative);
    try vm.defineNative("isError", isErrorNative);
    try vm.defineNative("process_flag", processFlagNative);
    try vm.defineNative("link", linkNative);
    try vm.defineNative("supervise_opt", superviseOptNative);
}

fn clockNative(vm: *VM, args: []const Value) Value {
    _ = args;
    const ts = std.Io.Clock.Timestamp.now(vm.io, .real);
    const secs = @as(f64, @floatFromInt(ts.raw.toSeconds()));
    const ns = @as(f64, @floatFromInt(ts.raw.toNanoseconds()));
    const subsec = @mod(ns, 1_000_000_000.0);
    return .{ .number = secs + subsec / 1_000_000_000.0 };
}

// ─────────────────────────────────────
// Helpers
// ─────────────────────────────────────

/// Find the current process's info.
/// If `vm` is a sub-VM, we search the parent VM's process list.
fn currentProcess(vm: *VM) ?VM.ProcessInfo {
    // Determine which VM owns the process tracking
    const owner = vm.parent_vm orelse vm;

    for (owner.process_objs.items, owner.sub_vms.items, 0..) |p, sub_vm, i| {
        if (sub_vm == vm) {
            return .{ .index = i, .vm = sub_vm, .obj = p };
        }
    }
    return null;
}

// ─────────────────────────────────────
// spawn & await
// ─────────────────────────────────────

/// spawn(fn, arg1, arg2, ...) — creates a new process.
/// Returns the process ID as a number.
fn spawnNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1) return .nil;
    const callable = args[0];

    const closure: *ObjClosure = if (callable == .obj) blk: {
        const obj = callable.obj;
        if (obj.obj_type == .closure) break :blk @ptrCast(obj);
        if (obj.obj_type == .function) {
            const cl = ObjClosure.init(vm.allocator, @ptrCast(obj)) catch return .nil;
            break :blk cl;
        }
        return .nil;
    } else return .nil;

    const spawn_args = if (args.len > 1) args[1..] else &.{};
    const result = vm.spawnProcess(closure, spawn_args) catch return .nil;
    return result;
}

/// await(pid) — waits for a process to finish.
/// Returns the process's result, or the error message if it crashed.
/// Drives the scheduler while waiting.
fn awaitNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1 or args[0] != .number) return .nil;
    const pid = @as(u64, @intFromFloat(args[0].number));

    while (true) {
        if (vm.findProcess(pid)) |info| {
            if (info.obj.status == .done) return info.obj.result;
            if (info.obj.status == .crashed) return .{ .string = info.obj.error_msg orelse "crashed" };
            vm.schedulerTick();
        } else {
            return .nil;
        }
    }
}

// ─────────────────────────────────────
// Message passing
// ─────────────────────────────────────

/// receive() — blocking receive.
/// If the mailbox has a message, returns it immediately.
/// If empty, the process blocks and yields control to the scheduler.
/// The process is woken up when send() delivers a message to it.
fn receiveNative(vm: *VM, args: []const Value) Value {
    _ = args;
    if (currentProcess(vm)) |info| {
        if (info.obj.mailbox.items.len > 0) {
            return info.obj.mailbox.orderedRemove(0);
        }
        // No messages — block this process
        info.obj.status = .blocked_receive;
        vm.yield_requested = true;
    }
    return .nil;
}

/// tryReceive() — non-blocking receive.
/// Returns the next message or nil if empty.
fn tryReceiveNative(vm: *VM, args: []const Value) Value {
    _ = args;
    if (currentProcess(vm)) |info| {
        if (info.obj.mailbox.items.len > 0) {
            return info.obj.mailbox.orderedRemove(0);
        }
    }
    return .nil;
}

/// send(pid, msg) — send a message to a process's mailbox.
/// If the target process was blocked on receive, it is woken up.
fn sendNative(vm: *VM, args: []const Value) Value {
    if (args.len < 2 or args[0] != .number) return .nil;
    const pid = @as(u64, @intFromFloat(args[0].number));
    const msg = args[1];

    if (vm.findProcess(pid)) |info| {
        info.obj.mailbox.append(info.obj.allocator, msg) catch return .{ .boolean = false };
        // Wake target if it was blocked on receive
        if (info.obj.status == .blocked_receive) {
            info.obj.status = .ready;
            vm.ready_queue.append(vm.allocator, info.index) catch {};
        }
        return .{ .boolean = true };
    }
    return .{ .boolean = false };
}

// ─────────────────────────────────────
// Process identity & control
// ─────────────────────────────────────

/// self() — returns the current process's PID.
fn selfNative(vm: *VM, args: []const Value) Value {
    _ = args;
    if (currentProcess(vm)) |info| {
        return .{ .number = @as(f64, @floatFromInt(info.obj.id)) };
    }
    return .nil;
}

/// yield() — voluntarily give up the remainder of the current timeslice.
fn yieldNative(vm: *VM, args: []const Value) Value {
    _ = args;
    _ = vm;
    return .nil;
}

/// abort(pid) — forcefully terminate a process.
fn abortNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1 or args[0] != .number) return .nil;
    const pid = @as(u64, @intFromFloat(args[0].number));

    if (vm.findProcess(pid)) |info| {
        info.obj.status = .crashed;
        info.obj.error_msg = "aborted";

        if (std.mem.indexOfScalar(usize, vm.ready_queue.items, info.index)) |qi| {
            _ = vm.ready_queue.orderedRemove(qi);
        }

        // Propagate exit signals
        vm.deliverExitSignals(info.index);
        return .{ .boolean = true };
    }
    return .{ .boolean = false };
}

/// isError(value) — returns true if the value is an error string.
fn isErrorNative(vm: *VM, args: []const Value) Value {
    _ = vm;
    if (args.len < 1) return .{ .boolean = false };
    return .{ .boolean = args[0] == .string };
}

// ─────────────────────────────────────
// Process flags & linking
// ─────────────────────────────────────

/// process_flag(flag, value) — set process flags.
/// Supported flags: "trap_exit" — when true, exit signals become messages instead of crashing.
fn processFlagNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1) return .nil;
    const flag = if (args[0] == .string) args[0].string else return .nil;

    if (currentProcess(vm)) |info| {
        if (std.mem.eql(u8, flag, "trap_exit")) {
            info.obj.trap_exit = if (args.len > 1) args[1].isTruthy() else true;
        }
    }
    return .nil;
}

/// link(pid) — link the current process to another process.
/// When a linked process crashes, an exit signal is delivered.
/// If trap_exit is true, the signal becomes a message; otherwise the linked process also crashes.
fn linkNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1 or args[0] != .number) return .nil;
    const pid = @as(u64, @intFromFloat(args[0].number));

    if (vm.findProcess(pid)) |target_info| {
        if (currentProcess(vm)) |info| {
            // Bidirectional link
            info.obj.links.append(info.obj.allocator, pid) catch return .{ .boolean = false };
            target_info.obj.links.append(target_info.obj.allocator, info.obj.id) catch return .{ .boolean = false };
            return .{ .boolean = true };
        }
    }
    return .{ .boolean = false };
}

// ─────────────────────────────────────
// Supervision
// ─────────────────────────────────────

/// supervise_opt(fn, options_string) — spawn with supervision.
/// The supervisor automatically restarts the process if it crashes,
/// up to max_restarts times.
///
/// Options (encoded as a string — future: proper record syntax):
///   "permanent"  — always restart (default)
///   "temporary"  — never restart
///   "transient"  — restart only on crash
///   "max=N"      — max restarts (default 3)
fn superviseOptNative(vm: *VM, args: []const Value) Value {
    if (args.len < 1) return .nil;
    const callable = args[0];

    const closure: *ObjClosure = if (callable == .obj) blk: {
        const obj = callable.obj;
        if (obj.obj_type == .closure) break :blk @ptrCast(obj);
        if (obj.obj_type == .function) {
            const cl = ObjClosure.init(vm.allocator, @ptrCast(obj)) catch return .nil;
            break :blk cl;
        }
        return .nil;
    } else return .nil;

    const spawn_args = if (args.len > 1) args[1..] else &.{};
    const pid_val = vm.spawnProcess(closure, spawn_args) catch return .nil;
    const pid = @as(u64, @intFromFloat(pid_val.number));

    // Set up supervision on the spawned process
    if (vm.findProcess(pid)) |info| {
        // Determine policy from the options string
        var policy = RestartPolicy.permanent;
        var max_restarts: u8 = 3;

        if (args.len > 1 and args[1] == .string) {
            const opts = args[1].string;
            if (std.mem.eql(u8, opts, "temporary")) policy = .temporary;
            if (std.mem.eql(u8, opts, "transient")) policy = .transient;

            // Parse max=N prefix
            if (std.mem.startsWith(u8, opts, "max=")) {
                const num_str = opts[4..];
                max_restarts = std.fmt.parseInt(u8, num_str, 10) catch 3;
            }
        }

        info.obj.restart_policy = policy;
        info.obj.max_restarts = max_restarts;
        info.obj.restarts_remaining = max_restarts;

        // Set supervisor: the current process
        if (currentProcess(vm)) |cur_info| {
            info.obj.supervisor_id = cur_info.obj.id;
        }
    }

    return pid_val;
}
