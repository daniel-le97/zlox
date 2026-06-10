//! Built-in native functions for the Lox VM
const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

/// Register all built-in native functions in the VM's global scope.
pub fn register(vm: *VM) !void {
    try vm.defineNative("clock", clockNative);
}

fn clockNative(vm: *VM, args: []const Value) Value {
    _ = args;
    const ts = std.Io.Clock.Timestamp.now(vm.io, .real);
    const secs = @as(f64, @floatFromInt(ts.raw.toSeconds()));
    const ns = @as(f64, @floatFromInt(ts.raw.toNanoseconds()));
    const subsec = @mod(ns, 1_000_000_000.0);
    return .{ .number = secs + subsec / 1_000_000_000.0 };
}
