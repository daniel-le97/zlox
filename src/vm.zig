//! Register-based virtual machine with local-variable caching for dispatch
const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Obj = @import("value.zig").Obj;
const ObjFunction = @import("value.zig").ObjFunction;
const ObjClosure = @import("value.zig").ObjClosure;
const ObjUpvalue = @import("value.zig").ObjUpvalue;
const ObjClass = @import("value.zig").ObjClass;
const ObjInstance = @import("value.zig").ObjInstance;
const ObjBoundMethod = @import("value.zig").ObjBoundMethod;
const ObjNative = @import("value.zig").ObjNative;
const ObjModule = @import("value.zig").ObjModule;
const ObjProcess = @import("value.zig").ObjProcess;
const ProcessStatus = @import("value.zig").ProcessStatus;
const NativeFn = @import("value.zig").NativeFn;
const ObjType = @import("value.zig").ObjType;

const MAX_REGISTERS = 4096;
const FRAMES_MAX = 64;

const CallFrame = struct {
    closure: *ObjClosure,
    ip: usize,
    reg_base: usize,
    num_regs: u8,
    return_dst: usize,
    is_initializer: bool = false,
    code: []const u8 = &.{},
    constants: []const Value = &.{},
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    globals: std.StringHashMap(Value),
    registers: [MAX_REGISTERS]Value = undefined,
    reg_top: usize = 0,
    frames: [FRAMES_MAX]CallFrame = undefined,
    frame_count: usize = 0,
    open_upvalues: ?*ObjUpvalue = null,
    objects: ?*Obj = null,

    // ── Scheduler / process system ──
    next_pid: u64 = 1,
    sub_vms: std.ArrayList(*VM),
    process_objs: std.ArrayList(*ObjProcess),
    ready_queue: std.ArrayList(usize),
    blocked_await: std.AutoHashMap(u64, usize), // pid -> parent index
    current_process_idx: usize = 0,
    instruction_budget: u32 = 0, // 0 = unlimited
    yield_requested: bool = false,
    parent_vm: ?*VM = null, // set for sub-VMs so they can find their process entry

    pub fn init(allocator: std.mem.Allocator, io: std.Io) VM {
        var vm = VM{
            .allocator = allocator,
            .io = io,
            .globals = std.StringHashMap(Value).init(allocator),
            .sub_vms = .{ .items = &.{}, .capacity = 0 },
            .process_objs = .{ .items = &.{}, .capacity = 0 },
            .ready_queue = .{ .items = &.{}, .capacity = 0 },
            .blocked_await = std.AutoHashMap(u64, usize).init(allocator),
        };
        builtin.register(&vm) catch {};
        return vm;
    }

    pub fn defineNative(self: *VM, name: []const u8, function: NativeFn) !void {
        const native = try self.allocObject(ObjNative);
        native.obj = .{ .obj_type = .native, .next = null };
        native.function = function;
        try self.globals.put(name, .{ .obj = &native.obj });
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        // Free sub-VMs and their process objects
        for (self.sub_vms.items, self.process_objs.items) |sub_vm, proc_obj| {
            if (sub_vm != self) {
                sub_vm.deinit();
                self.allocator.destroy(sub_vm);
            }
            self.allocator.destroy(proc_obj);
        }
        self.sub_vms.deinit(self.allocator);
        self.process_objs.deinit(self.allocator);
        self.ready_queue.deinit(self.allocator);
        self.blocked_await.deinit();
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            self.freeObject(o);
            obj = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        var lexer = Lexer.init(source, self.allocator);
        var compiler = try Compiler.init(self.allocator, &lexer, .script, null, null);
        const compiled = try compiler.compile();
        if (!compiled) return .compile_error;

        const func_obj: *ObjFunction = compiler.function;
        const closure = try ObjClosure.init(self.allocator, func_obj);
        self.registers[0] = .{ .obj = &closure.obj };
        self.reg_top = 1;

        // Register as main process (slot 0)
        const main_proc = try self.allocator.create(ObjProcess);
        main_proc.* = ObjProcess.init(self.allocator, 0, closure);
        main_proc.status = .running;
        try self.sub_vms.append(self.allocator, self);
        try self.process_objs.append(self.allocator, main_proc);

        _ = self.callValue(0, 0, 0);

        // Run with unlimited budget (main execution)
        self.instruction_budget = 0;
        return self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        var frame = &self.frames[self.frame_count - 1];
        var ip = frame.ip;
        var code = frame.code;
        var constants = frame.constants;
        var reg_base = frame.reg_base;

        while (true) {
            // Check instruction budget (0 = unlimited)
            if (self.instruction_budget > 0) {
                self.instruction_budget -= 1;
                if (self.instruction_budget == 0) {
                    frame.ip = ip;
                    return .yield;
                }
            }

            const instruction: OpCode = @enumFromInt(code[ip]);
            ip += 1;
            switch (instruction) {
                .load_const => {
                    const dst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    self.registers[reg_base + dst] = constants[ci];
                },
                .load_nil => {
                    const dst = code[ip];
                    ip += 1;
                    self.registers[reg_base + dst] = .nil;
                },
                .load_true => {
                    const dst = code[ip];
                    ip += 1;
                    self.registers[reg_base + dst] = .{ .boolean = true };
                },
                .load_false => {
                    const dst = code[ip];
                    ip += 1;
                    self.registers[reg_base + dst] = .{ .boolean = false };
                },
                .get_global => {
                    const dst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    self.registers[reg_base + dst] = self.globals.get(name.string) orelse return error.UndefinedVariable;
                },
                .set_global => {
                    const ci = code[ip];
                    ip += 1;
                    const src = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    if (!self.globals.contains(name.string)) return error.UndefinedVariable;
                    try self.globals.put(name.string, self.registers[reg_base + src]);
                },
                .define_global => {
                    const ci = code[ip];
                    ip += 1;
                    const src = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    try self.globals.put(name.string, self.registers[reg_base + src]);
                },
                .get_upvalue => {
                    const dst = code[ip];
                    ip += 1;
                    const slot = code[ip];
                    ip += 1;
                    const uv = frame.closure.upvalues[slot].?;
                    self.registers[reg_base + dst] = if (uv.location == &uv.closed) uv.closed else uv.location.*;
                },
                .set_upvalue => {
                    const slot = code[ip];
                    ip += 1;
                    const src = code[ip];
                    ip += 1;
                    const uv = frame.closure.upvalues[slot].?;
                    uv.location.* = self.registers[reg_base + src];
                },
                .close_upvalue => {
                    const src = code[ip];
                    ip += 1;
                    self.closeUpvalues(reg_base + src);
                },
                .add, .sub, .mul, .div => {
                    const op = instruction;
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = self.binaryOpResult(op, self.registers[reg_base + a], self.registers[reg_base + b]) catch |e| return e;
                },
                .add_number => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = self.registers[reg_base + a].number + self.registers[reg_base + b].number };
                },
                .sub_number => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = self.registers[reg_base + a].number - self.registers[reg_base + b].number };
                },
                .sub_const => {
                    const d = code[ip];
                    ip += 1;
                    const s = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = self.registers[reg_base + s].number - constants[ci].number };
                },
                .mul_number => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = self.registers[reg_base + a].number * self.registers[reg_base + b].number };
                },
                .div_number => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = self.registers[reg_base + a].number / self.registers[reg_base + b].number };
                },
                .negate, .negate_number => {
                    const d = code[ip];
                    ip += 1;
                    const s = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .number = -self.registers[reg_base + s].number };
                },
                .not_register => {
                    const d = code[ip];
                    ip += 1;
                    const s = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .boolean = !self.registers[reg_base + s].isTruthy() };
                },
                .equal => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = .{ .boolean = self.registers[reg_base + a].isEqual(self.registers[reg_base + b]) };
                },
                .greater, .greater_number, .less, .less_number => {
                    const d = code[ip];
                    ip += 1;
                    const a = code[ip];
                    ip += 1;
                    const b = code[ip];
                    ip += 1;
                    const lt = instruction == .less or instruction == .less_number;
                    const va = self.registers[reg_base + a];
                    const vb = self.registers[reg_base + b];
                    if (instruction == .greater or instruction == .less) {
                        if (va != .number or vb != .number) return error.OperandsMustBeNumbers;
                    }
                    self.registers[reg_base + d] = .{ .boolean = if (lt) va.number < vb.number else va.number > vb.number };
                },
                .jump_if_false => {
                    const src = code[ip];
                    ip += 1;
                    const offset = (@as(u16, code[ip]) << 8) | code[ip + 1];
                    ip += 2;
                    if (!self.registers[reg_base + src].isTruthy()) ip += offset;
                },
                .jump => {
                    const offset = (@as(u16, code[ip]) << 8) | code[ip + 1];
                    ip += 2;
                    ip += offset;
                },
                .loop => {
                    const offset = (@as(u16, code[ip]) << 8) | code[ip + 1];
                    ip += 2;
                    ip -= offset;
                },
                .move => {
                    const d = code[ip];
                    ip += 1;
                    const s = code[ip];
                    ip += 1;
                    self.registers[reg_base + d] = self.registers[reg_base + s];
                },
                .call => {
                    const r_dst = code[ip];
                    ip += 1;
                    const r_callee = code[ip];
                    ip += 1;
                    const arg_count = code[ip];
                    ip += 1;
                    frame.ip = ip;
                    if (!self.callValue(reg_base + r_dst, reg_base + r_callee, arg_count)) return .runtime_error;
                    if (self.yield_requested) {
                        self.yield_requested = false;
                        frame = &self.frames[self.frame_count - 1];
                        frame.ip = ip;
                        return .yield;
                    }
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                    code = frame.code;
                    constants = frame.constants;
                    reg_base = frame.reg_base;
                },
                .call_self => {
                    const r_dst = code[ip];
                    ip += 1;
                    const arg_count = code[ip];
                    ip += 1;
                    const callee = frame.closure;
                    if (arg_count != callee.func.arity) return error.RuntimeError;
                    if (self.frame_count >= FRAMES_MAX) return .runtime_error;
                    frame.ip = ip; // save return address
                    self.registers[reg_base + r_dst] = .nil;
                    const new_frame = &self.frames[self.frame_count];
                    self.frame_count += 1;
                    new_frame.closure = callee;
                    new_frame.ip = 0;
                    new_frame.reg_base = reg_base + r_dst;
                    new_frame.num_regs = callee.func.num_registers;
                    new_frame.return_dst = reg_base + r_dst;
                    new_frame.is_initializer = false;
                    new_frame.code = callee.func.chunk.code.items;
                    new_frame.constants = callee.func.chunk.constants.items;
                    frame = new_frame;
                    ip = 0;
                    code = frame.code;
                    constants = frame.constants;
                    reg_base = frame.reg_base;
                },
                .closure => {
                    const dst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const func_val = constants[ci];
                    if (func_val != .obj or func_val.obj.obj_type != .function) return error.RuntimeError;
                    var closure = try ObjClosure.init(self.allocator, @ptrCast(func_val.obj));
                    for (0..closure.func.upvalue_count) |i| {
                        const is_local = code[ip];
                        ip += 1;
                        const index = code[ip];
                        ip += 1;
                        if (is_local != 0) closure.upvalues[i] = try self.captureUpvalue(reg_base + index) else closure.upvalues[i] = frame.closure.upvalues[index];
                    }
                    self.registers[reg_base + dst] = .{ .obj = &closure.obj };
                },
                .class => {
                    const dst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const class_obj = try self.allocObject(ObjClass);
                    class_obj.* = ObjClass.init(self.allocator, name.string, null);
                    self.registers[reg_base + dst] = .{ .obj = &class_obj.obj };
                },
                .get_property => {
                    const dst = code[ip];
                    ip += 1;
                    const inst_reg = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const target = self.registers[reg_base + inst_reg];
                    if (target != .obj) return error.OnlyInstancesHaveProperties;
                    if (target.obj.obj_type == .instance) {
                        const inst: *ObjInstance = @ptrCast(target.obj);
                        if (inst.fields.get(name.string)) |value| {
                            self.registers[reg_base + dst] = value;
                        } else if (inst.class_def.findMethod(name.string)) |method| {
                            const bound = try self.allocObject(ObjBoundMethod);
                            bound.* = .{ .obj = .{ .obj_type = .bound_method, .next = null }, .receiver = target, .method = @ptrCast(method.obj) };
                            self.registers[reg_base + dst] = .{ .obj = &bound.obj };
                        } else return error.UndefinedProperty;
                    } else if (target.obj.obj_type == .module) {
                        const mod: *ObjModule = @ptrCast(target.obj);
                        if (mod.exports.get(name.string)) |value| self.registers[reg_base + dst] = value else return error.UndefinedProperty;
                    } else return error.OnlyInstancesHaveProperties;
                },
                .set_property => {
                    const inst_reg = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const src = code[ip];
                    ip += 1;
                    const target = self.registers[reg_base + inst_reg];
                    if (target != .obj or target.obj.obj_type != .instance) return error.OnlyInstancesHaveProperties;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const inst: *ObjInstance = @ptrCast(target.obj);
                    try inst.fields.put(name.string, self.registers[reg_base + src]);
                    self.registers[reg_base + inst_reg] = self.registers[reg_base + src];
                },
                .method => {
                    const class_reg = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const method_reg = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const method_val = self.registers[reg_base + method_reg];
                    if (method_val != .obj) return error.RuntimeError;
                    var closure = try ObjClosure.init(self.allocator, @ptrCast(method_val.obj));
                    const class_def: *ObjClass = @ptrCast(self.registers[reg_base + class_reg].obj);
                    try class_def.methods.put(name.string, .{ .obj = &closure.obj });
                },
                .invoke => {
                    const r_dst = code[ip];
                    ip += 1;
                    const r_inst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const arg_count = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const receiver = self.registers[reg_base + r_inst];
                    if (receiver != .obj or receiver.obj.obj_type != .instance) return error.OnlyInstancesHaveMethods;
                    const instance: *ObjInstance = @ptrCast(receiver.obj);
                    frame.ip = ip;
                    if (instance.fields.get(name.string)) |value| {
                        self.registers[reg_base + r_inst] = value;
                        if (!self.callValue(reg_base + r_dst, reg_base + r_inst, arg_count)) return .runtime_error;
                    } else {
                        if (!self.invokeFromClass(instance.class_def, name.string, reg_base + r_dst, reg_base + r_inst, arg_count)) return .runtime_error;
                    }
                    if (self.yield_requested) {
                        self.yield_requested = false;
                        frame = &self.frames[self.frame_count - 1];
                        frame.ip = ip;
                        return .yield;
                    }
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                    code = frame.code;
                    constants = frame.constants;
                    reg_base = frame.reg_base;
                },
                .inherit => {
                    const sub_reg = code[ip];
                    ip += 1;
                    const sup_reg = code[ip];
                    ip += 1;
                    const sv = self.registers[reg_base + sub_reg];
                    const spv = self.registers[reg_base + sup_reg];
                    if (sv != .obj or sv.obj.obj_type != .class) return error.RuntimeError;
                    if (spv != .obj or spv.obj.obj_type != .class) return error.SuperclassMustBeClass;
                    (@as(*ObjClass, @ptrCast(sv.obj))).superclass = @ptrCast(spv.obj);
                },
                .get_super => {
                    const dst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const instance = self.registers[reg_base];
                    if (instance != .obj or instance.obj.obj_type != .instance) return error.RuntimeError;
                    const inst: *ObjInstance = @ptrCast(instance.obj);
                    const superclass = inst.class_def.superclass orelse return error.RuntimeError;
                    if (!self.bindMethod(superclass, name.string, reg_base + dst)) return .runtime_error;
                },
                .super_invoke => {
                    const r_dst = code[ip];
                    ip += 1;
                    const r_inst = code[ip];
                    ip += 1;
                    const ci = code[ip];
                    ip += 1;
                    const arg_count = code[ip];
                    ip += 1;
                    const name = constants[ci];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const instance = self.registers[reg_base + r_inst];
                    if (instance != .obj or instance.obj.obj_type != .instance) return error.RuntimeError;
                    const inst: *ObjInstance = @ptrCast(instance.obj);
                    const superclass = inst.class_def.superclass orelse return error.RuntimeError;
                    frame.ip = ip;
                    if (!self.invokeFromClass(superclass, name.string, reg_base + r_dst, reg_base + r_inst, arg_count)) return .runtime_error;
                    if (self.yield_requested) {
                        self.yield_requested = false;
                        frame = &self.frames[self.frame_count - 1];
                        frame.ip = ip;
                        return .yield;
                    }
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                    code = frame.code;
                    constants = frame.constants;
                    reg_base = frame.reg_base;
                },
                .import_module => {
                    _ = code[ip];
                    ip += 1;
                    _ = code[ip];
                    ip += 1;
                },
                .print => {
                    const src = code[ip];
                    ip += 1;
                    self.printValue(self.registers[reg_base + src]);
                },
                .@"return" => {
                    const src = code[ip];
                    ip += 1;
                    const result = self.registers[reg_base + src];
                    const saved_return_dst = frame.return_dst;
                    const was_initializer = frame.is_initializer;
                    self.closeUpvalues(reg_base);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        self.registers[0] = if (was_initializer) self.registers[reg_base] else result;
                        return .ok;
                    }
                    frame = &self.frames[self.frame_count - 1];
                    if (was_initializer) self.registers[saved_return_dst] = self.registers[reg_base] else self.registers[saved_return_dst] = result;
                    ip = frame.ip;
                    code = frame.code;
                    constants = frame.constants;
                    reg_base = frame.reg_base;
                },
            }
        }
    }

    fn binaryOpResult(self: *VM, op: OpCode, a: Value, b: Value) !Value {
        if (a == .number and b == .number) {
            return switch (op) {
                .add => Value{ .number = a.number + b.number },
                .sub => Value{ .number = a.number - b.number },
                .mul => Value{ .number = a.number * b.number },
                .div => Value{ .number = a.number / b.number },
                else => unreachable,
            };
        }
        if (a == .string and b == .string and op == .add) {
            const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a.string, b.string });
            return Value{ .string = result };
        }
        return error.OperandsMustBeNumbers;
    }

    fn callValue(self: *VM, r_dst: usize, r_callee: usize, arg_count: u8) bool {
        const callee = self.registers[r_callee];
        if (callee == .obj) {
            switch (callee.obj.obj_type) {
                .function => {
                    const closure = ObjClosure.init(self.allocator, @ptrCast(callee.obj)) catch return false;
                    self.registers[r_callee] = .{ .obj = &closure.obj };
                    return self.call(closure, r_dst, r_callee, arg_count);
                },
                .closure => return self.call(@ptrCast(callee.obj), r_dst, r_callee, arg_count),
                .native => {
                    const native_fn: *ObjNative = @ptrCast(callee.obj);
                    const args = self.registers[r_callee + 1 .. r_callee + 1 + arg_count];
                    self.registers[r_dst] = native_fn.function(self, args);
                    return true;
                },
                .class => {
                    const class_def: *ObjClass = @ptrCast(callee.obj);
                    const instance = self.allocObject(ObjInstance) catch return false;
                    instance.* = ObjInstance.init(self.allocator, class_def);
                    self.registers[r_callee] = .{ .obj = &instance.obj };
                    if (class_def.methods.get("init")) |init_method| {
                        self.registers[r_callee] = init_method;
                        if (!self.callValue(r_dst, r_callee, arg_count)) return false;
                        self.frames[self.frame_count - 1].is_initializer = true;
                        return true;
                    } else {
                        if (arg_count != 0) return false;
                        self.registers[r_dst] = self.registers[r_callee];
                        return true;
                    }
                },
                .bound_method => {
                    const bound: *ObjBoundMethod = @ptrCast(callee.obj);
                    self.registers[r_callee] = bound.receiver;
                    return self.call(bound.method, r_dst, r_callee, arg_count);
                },
                else => return false,
            }
        }
        return false;
    }

    fn call(self: *VM, closure: *ObjClosure, r_dst: usize, r_callee: usize, arg_count: u8) bool {
        if (arg_count != closure.func.arity) return false;
        if (self.frame_count >= FRAMES_MAX) return false;
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = 0;
        frame.reg_base = r_callee;
        frame.num_regs = closure.func.num_registers;
        frame.return_dst = r_dst;
        frame.is_initializer = false;
        frame.code = closure.func.chunk.code.items;
        frame.constants = closure.func.chunk.constants.items;
        return true;
    }

    fn invokeFromClass(self: *VM, class_def: *ObjClass, name: []const u8, r_dst: usize, r_callee: usize, arg_count: u8) bool {
        const method_val = class_def.findMethod(name) orelse return false;
        self.registers[r_callee] = method_val;
        return self.callValue(r_dst, r_callee, arg_count);
    }

    fn bindMethod(self: *VM, class_def: *ObjClass, name: []const u8, r_dst: usize) bool {
        const method_value = class_def.findMethod(name) orelse return false;
        const bound = self.allocObject(ObjBoundMethod) catch return false;
        bound.* = .{ .obj = .{ .obj_type = .bound_method, .next = null }, .receiver = self.registers[r_dst], .method = @ptrCast(method_value.obj) };
        self.registers[r_dst] = .{ .obj = &bound.obj };
        return true;
    }

    fn captureUpvalue(self: *VM, reg_abs: usize) !*ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue) |uv| {
            if (@intFromPtr(uv.location) <= @intFromPtr(&self.registers[reg_abs])) break;
            prev_upvalue = uv;
            upvalue = uv.next;
        }
        if (upvalue != null and @intFromPtr(upvalue.?.location) == @intFromPtr(&self.registers[reg_abs])) return upvalue.?;
        const created = try self.allocObject(ObjUpvalue);
        created.* = ObjUpvalue{ .obj = .{ .obj_type = .upvalue, .next = null }, .location = &self.registers[reg_abs], .next = upvalue };
        if (prev_upvalue) |pv| pv.next = @ptrCast(&created.obj) else self.open_upvalues = @ptrCast(&created.obj);
        return created;
    }

    fn closeUpvalues(self: *VM, last_abs: usize) void {
        while (self.open_upvalues) |uv| {
            if (@intFromPtr(uv.location) < @intFromPtr(&self.registers[last_abs])) break;
            uv.closed = uv.location.*;
            uv.location = &uv.closed;
            self.open_upvalues = uv.next;
        }
    }

    fn allocObject(self: *VM, comptime T: type) !*T {
        const ptr = try self.allocator.create(T);
        ptr.obj.next = self.objects;
        self.objects = &ptr.obj;
        return ptr;
    }

    fn freeObject(self: *VM, obj: *Obj) void {
        switch (obj.obj_type) {
            .function => {
                const f: *ObjFunction = @fieldParentPtr("obj", obj);
                f.deinit();
                self.allocator.destroy(f);
            },
            .closure => {
                const c: *ObjClosure = @fieldParentPtr("obj", obj);
                c.deinit(self.allocator);
                self.allocator.destroy(c);
            },
            .upvalue => {
                self.allocator.destroy(@as(*ObjUpvalue, @ptrCast(obj)));
            },
            .class => {
                const c: *ObjClass = @fieldParentPtr("obj", obj);
                c.deinit();
                self.allocator.destroy(c);
            },
            .instance => {
                const i: *ObjInstance = @fieldParentPtr("obj", obj);
                i.deinit();
                self.allocator.destroy(i);
            },
            .bound_method => {
                self.allocator.destroy(@as(*ObjBoundMethod, @ptrCast(obj)));
            },
            .native => {
                self.allocator.destroy(@as(*ObjNative, @ptrCast(obj)));
            },
            .module => {
                const m: *ObjModule = @fieldParentPtr("obj", obj);
                m.deinit();
                self.allocator.destroy(m);
            },
            .process => {
                const p: *ObjProcess = @fieldParentPtr("obj", obj);
                p.deinit();
                self.allocator.destroy(p);
            },
        }
    }

    fn printValue(self: *VM, value: Value) void {
        _ = self;
        switch (value) {
            .number => |n| std.debug.print("{}\n", .{n}),
            .boolean => |b| std.debug.print("{}\n", .{b}),
            .string => |s| std.debug.print("{s}\n", .{s}),
            .obj => |obj| {
                switch (obj.obj_type) {
                    .function => std.debug.print("<fn {s}>\n", .{@as(*ObjFunction, @ptrCast(obj)).name}),
                    .closure => std.debug.print("<closure {s}>\n", .{@as(*ObjClosure, @ptrCast(obj)).func.name}),
                    .class => std.debug.print("<class {s}>\n", .{@as(*ObjClass, @ptrCast(obj)).name}),
                    .instance => std.debug.print("<instance {s}>\n", .{@as(*ObjInstance, @ptrCast(obj)).class_def.name}),
                    .bound_method => std.debug.print("<method {s}>\n", .{@as(*ObjBoundMethod, @ptrCast(obj)).method.func.name}),
                    .native => std.debug.print("<native fn>\n", .{}),
                    .module => std.debug.print("<module>\n", .{}),
                    .process => std.debug.print("<process {d}>\n", .{@as(*ObjProcess, @ptrCast(obj)).id}),
                    .upvalue => std.debug.print("<upvalue>\n", .{}),
                }
            },
            .nil => std.debug.print("nil\n", .{}),
        }
    }

    // ──────────────────────────────────────────────
    // Scheduler & Process Management
    // ──────────────────────────────────────────────

    /// Spawn a new process that executes the given closure.
    /// Returns the process ID (as a number Value).
    /// Always uses the root parent VM for process tracking.
    pub fn spawnProcess(self: *VM, closure: *ObjClosure, args: []const Value) !Value {
        // Use the root VM (top-level parent) for process tracking
        const root: *VM = if (self.parent_vm) |p| blk: {
            // Walk up to the actual root
            var r = p;
            while (r.parent_vm) |gp| r = gp;
            break :blk r;
        } else self;

        // Create a new sub-VM for this process
        const sub_vm = try root.allocator.create(VM);
        sub_vm.* = VM.init(root.allocator, root.io);
        sub_vm.parent_vm = root;

        // Create the process object
        const pid = root.next_pid;
        root.next_pid += 1;
        const proc_obj = try root.allocator.create(ObjProcess);
        proc_obj.* = ObjProcess.init(root.allocator, pid, closure);
        proc_obj.status = .ready;

        // Copy the closure into the sub-VM's registers
        sub_vm.registers[0] = .{ .obj = &closure.obj };
        sub_vm.reg_top = 1;

        // Copy arguments into registers 1..N
        for (args, 1..) |arg, i| {
            sub_vm.registers[i] = arg;
            sub_vm.reg_top = @max(sub_vm.reg_top, i + 1);
        }

        // Set up the call frame
        _ = sub_vm.callValue(0, 0, @intCast(args.len));

        // Save spawn args for supervision restart
        for (args) |arg| {
            proc_obj.spawn_args.append(proc_obj.allocator, arg) catch {};
        }

        // Register this process in the root VM
        const idx = root.sub_vms.items.len;
        try root.sub_vms.append(root.allocator, sub_vm);
        try root.process_objs.append(root.allocator, proc_obj);
        try root.ready_queue.append(root.allocator, idx);

        return .{ .number = @as(f64, @floatFromInt(pid)) };
    }

    /// Run the scheduler loop — processes all ready processes cooperatively.
    /// Call this after interpret() returns, or whenever you want to process
    /// pending spawned processes.
    pub fn runScheduler(self: *VM) void {
        while (self.ready_queue.items.len > 0) {
            self.schedulerTick();
        }
    }

    /// Run one tick of the scheduler: pick the next ready process and run it
    /// for a budget of instructions.
    pub fn schedulerTick(self: *VM) void {
        if (self.ready_queue.items.len == 0) return;

        const idx = self.ready_queue.orderedRemove(0);
        const sub_vm = self.sub_vms.items[idx];
        const proc_obj = self.process_objs.items[idx];

        if (proc_obj.status != .ready) return;

        proc_obj.status = .running;
        sub_vm.instruction_budget = 500; // timeslice budget

        const result = sub_vm.run() catch |err| {
            // Runtime error in the sub-process
            proc_obj.status = .crashed;
            // Convert error to string
            proc_obj.error_msg = switch (err) {
                error.RuntimeError => "runtime error",
                error.OperandsMustBeNumbers => "operands must be numbers",
                error.OperandsMustBeStrings => "operands must be strings",
                error.UndefinedVariable => "undefined variable",
                error.OnlyInstancesHaveProperties => "only instances have properties",
                error.OnlyInstancesHaveMethods => "only instances have methods",
                error.SuperclassMustBeClass => "superclass must be a class",
                error.UndefinedProperty => "undefined property",
                else => "unknown error",
            };
            self.handleProcessExit(idx);
            return;
        };

        switch (result) {
            .yield => {
                // Only re-queue if the process wasn't deliberately blocked
                if (proc_obj.status == .running) {
                    proc_obj.status = .ready;
                    self.ready_queue.append(self.allocator, idx) catch {};
                }
            },
            .ok => {
                // Process completed normally
                proc_obj.status = .done;
                // Return value is in register 0 (set by return opcode when frame_count reaches 0)
                proc_obj.result = sub_vm.registers[0];
                self.handleProcessExit(idx);
            },
            .runtime_error, .compile_error => {
                proc_obj.status = .crashed;
                proc_obj.error_msg = "runtime error";
                self.handleProcessExit(idx);
            },
        }
    }

    /// Deliver exit signals to all processes linked to the given process.
    /// If the linked process has trap_exit=true, the signal becomes a message.
    /// Otherwise, the linked process also crashes.
    pub fn deliverExitSignals(self: *VM, idx: usize) void {
        const proc_obj = self.process_objs.items[idx];
        const err_msg = proc_obj.error_msg orelse "crashed";

        for (proc_obj.links.items) |linked_pid| {
            if (self.findProcess(linked_pid)) |link_info| {
                if (link_info.obj.trap_exit) {
                    // Deliver exit signal as a message
                    const exit_msg = Value{ .string = err_msg };
                    link_info.obj.mailbox.append(link_info.obj.allocator, exit_msg) catch {};
                    // Wake the linked process if blocked on receive
                    if (link_info.obj.status == .blocked_receive) {
                        link_info.obj.status = .ready;
                        self.ready_queue.append(self.allocator, link_info.index) catch {};
                    }
                } else {
                    // Linked process crashes too
                    link_info.obj.status = .crashed;
                    link_info.obj.error_msg = err_msg;
                    // Remove from ready queue
                    if (std.mem.indexOfScalar(usize, self.ready_queue.items, link_info.index)) |qi| {
                        _ = self.ready_queue.orderedRemove(qi);
                    }
                }
            }
        }
    }

    /// Handle process exit (normal or crash): deliver exit signals to linked processes,
    /// and restart if supervision is configured.
    fn handleProcessExit(self: *VM, idx: usize) void {
        const proc_obj = self.process_objs.items[idx];

        // Deliver exit signals to linked processes
        self.deliverExitSignals(idx);

        // Handle supervision auto-restart
        if (proc_obj.supervisor_id) |_| {
            const should_restart = switch (proc_obj.restart_policy) {
                .permanent => true,
                .transient => proc_obj.status == .crashed,
                .temporary => false,
            };

            if (should_restart and proc_obj.restarts_remaining > 0) {
                proc_obj.restarts_remaining -= 1;

                // Re-spawn: reset the sub-VM and run the closure again
                const sub_vm = self.sub_vms.items[idx];
                sub_vm.frame_count = 0;
                sub_vm.reg_top = 1;
                sub_vm.open_upvalues = null;
                sub_vm.registers[0] = .{ .obj = &proc_obj.closure.obj };

                // Copy saved spawn arguments into registers 1..N
                for (proc_obj.spawn_args.items, 1..) |arg, i| {
                    sub_vm.registers[i] = arg;
                    sub_vm.reg_top = @max(sub_vm.reg_top, i + 1);
                }

                if (sub_vm.callValue(0, 0, @intCast(proc_obj.spawn_args.items.len))) {
                    proc_obj.status = .ready;
                    proc_obj.error_msg = null;
                    self.ready_queue.append(self.allocator, idx) catch {};
                }
            }
        }
    }

    pub const ProcessInfo = struct {
        index: usize,
        vm: *VM,
        obj: *ObjProcess,
    };

    /// Find a process by PID. Returns its index, sub-VM, and ObjProcess, or null.
    /// If called from a sub-VM, searches the parent VM's process list.
    pub fn findProcess(self: *VM, pid: u64) ?ProcessInfo {
        const owner = self.parent_vm orelse self;
        for (owner.process_objs.items, 0..) |p, i| {
            if (p.id == pid) {
                return .{ .index = i, .vm = owner.sub_vms.items[i], .obj = p };
            }
        }
        return null;
    }

    /// Check if a process is done and return its result.
    /// If the process crashed, returns the error message as a string.
    pub fn getProcessResult(self: *VM, pid: u64) Value {
        if (self.findProcess(pid)) |info| {
            if (info.obj.status == .done) return info.obj.result;
            if (info.obj.status == .crashed) return .{ .string = info.obj.error_msg orelse "crashed" };
        }
        return .nil;
    }

    /// Send a message to a process's mailbox.
    pub fn sendMessage(self: *VM, pid: u64, msg: Value) bool {
        if (self.findProcess(pid)) |info| {
            info.obj.mailbox.append(info.obj.allocator, msg) catch return false;
            return true;
        }
        return false;
    }

    /// Try to receive a message from the current process's mailbox.
    /// Returns the message, or nil if empty.
    pub fn tryReceiveMessage(self: *VM) Value {
        const owner = self.parent_vm orelse self;
        for (owner.process_objs.items, owner.sub_vms.items) |p, sub_vm| {
            if (sub_vm == self) {
                if (p.mailbox.items.len > 0) {
                    return p.mailbox.orderedRemove(0);
                }
                break;
            }
        }
        return .nil;
    }
};

pub const InterpretResult = enum { ok, compile_error, runtime_error, yield };

const Lexer = @import("lexer.zig").Lexer;
const Compiler = @import("compiler.zig").Compiler;
const builtin = @import("builtin.zig");
