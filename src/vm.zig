//! Stack-based virtual machine for executing bytecode
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
const NativeFn = @import("value.zig").NativeFn;
const ObjType = @import("value.zig").ObjType;

const STACK_MAX = 256;
const FRAMES_MAX = 64;

const CallFrame = struct {
    closure: *ObjClosure,
    ip: usize,
    stack_offset: usize,
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    globals: std.StringHashMap(Value),
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,
    frames: [FRAMES_MAX]CallFrame = undefined,
    frame_count: usize = 0,
    open_upvalues: ?*ObjUpvalue = null,
    objects: ?*Obj = null,

    pub fn init(allocator: std.mem.Allocator) VM {
        return VM{
            .allocator = allocator,
            .globals = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        // Free all objects
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            self.freeObject(o);
            obj = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        // Compile the source
        var lexer = Lexer.init(source, self.allocator);

        var compiler = try Compiler.init(self.allocator, &lexer, .script, null, null);
        const compiled = try compiler.compile();

        if (!compiled) {
            return .compile_error;
        }

        // Push the compiled function as a closure
        const func_obj: *ObjFunction = compiler.function;
        const closure = try ObjClosure.init(self.allocator, func_obj);
        try self.push(.{ .obj = &closure.obj });
        _ = self.callValue(.{ .obj = &closure.obj }, 0);

        return self.run();
    }

    pub fn run(self: *VM) !InterpretResult {
        var frame = &self.frames[self.frame_count - 1];

        while (true) {
            // Uncomment for bytecode tracing
            // try self.traceBytecode(frame);

            const instruction: OpCode = @enumFromInt(self.readByte(frame));
            switch (instruction) {
                .constant => {
                    const index = self.readByte(frame);
                    const value = frame.closure.func.chunk.readConstant(index);
                    try self.push(value);
                },
                .nil => try self.push(.nil),
                .true => try self.push(.{ .boolean = true }),
                .false => try self.push(.{ .boolean = false }),
                .pop => {
                    _ = self.pop();
                },
                .define_global => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const value = self.peek(0);
                    try self.globals.put(name.string, value);
                    _ = self.pop();
                },
                .get_global => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const value = self.globals.get(name.string) orelse return error.UndefinedVariable;
                    try self.push(value);
                },
                .set_global => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    if (!self.globals.contains(name.string)) return error.UndefinedVariable;
                    const value = self.peek(0);
                    try self.globals.put(name.string, value);
                },
                .get_local => {
                    const slot = self.readByte(frame);
                    try self.push(self.stack[frame.stack_offset + slot]);
                },
                .set_local => {
                    const slot = self.readByte(frame);
                    self.stack[frame.stack_offset + slot] = self.peek(0);
                },
                .add => try self.binaryOp(.add),
                .subtract => try self.binaryOp(.subtract),
                .multiply => try self.binaryOp(.multiply),
                .divide => try self.binaryOp(.divide),
                .negate => {
                    const value = self.pop();
                    if (value != .number) return error.OperandMustBeNumber;
                    try self.push(.{ .number = -value.number });
                },
                .not => {
                    const value = self.pop();
                    try self.push(.{ .boolean = !value.isTruthy() });
                },
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(.{ .boolean = a.isEqual(b) });
                },
                .greater => {
                    const b = self.pop();
                    const a = self.pop();
                    if (a != .number or b != .number) return error.OperandsMustBeNumbers;
                    try self.push(.{ .boolean = a.number > b.number });
                },
                .less => {
                    const b = self.pop();
                    const a = self.pop();
                    if (a != .number or b != .number) return error.OperandsMustBeNumbers;
                    try self.push(.{ .boolean = a.number < b.number });
                },
                .jump_if_false => {
                    const offset = self.readShort(frame);
                    if (!self.peek(0).isTruthy()) {
                        frame.ip += offset;
                    }
                },
                .jump => {
                    const offset = self.readShort(frame);
                    frame.ip += offset;
                },
                .loop => {
                    const offset = self.readShort(frame);
                    frame.ip -= offset;
                },
                .call => {
                    const arg_count = self.readByte(frame);
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .closure => {
                    const func = self.readConstant(frame);
                    if (func != .obj or func.obj.obj_type != .function) return error.RuntimeError;

                    var closure = try ObjClosure.init(self.allocator, @ptrCast(func.obj));
                    self.push(.{ .obj = &closure.obj }) catch unreachable;

                    // Initialize upvalues
                    for (0..closure.func.upvalue_count) |i| {
                        const is_local = self.readByte(frame) != 0;
                        const index = self.readByte(frame);

                        if (is_local) {
                            closure.upvalues[i] = try self.captureUpvalue(frame.stack_offset + index);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },
                .get_upvalue => {
                    const slot = self.readByte(frame);
                    const upvalue = frame.closure.upvalues[slot].?;
                    try self.push(if (upvalue.location == &upvalue.closed) upvalue.closed else upvalue.location.*);
                },
                .set_upvalue => {
                    const slot = self.readByte(frame);
                    const upvalue = frame.closure.upvalues[slot].?;
                    upvalue.location.* = self.peek(0);
                },
                .close_upvalue => {
                    self.closeUpvalues(self.stack_top - 1);
                    _ = self.pop();
                },
                .class => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const class_obj = try self.allocObject(ObjClass);
                    class_obj.* = ObjClass.init(self.allocator, name.string, null);
                    try self.push(.{ .obj = &class_obj.obj });
                },
                .get_property => {
                    const instance = self.peek(0);
                    if (instance != .obj or instance.obj.obj_type != .instance) return error.OnlyInstancesHaveProperties;
                    const inst: *ObjInstance = @ptrCast(instance.obj);
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;

                    if (inst.fields.get(name.string)) |value| {
                        _ = self.pop();
                        try self.push(value);
                    } else {
                        // Look up method
                        const method = inst.class_def.methods.get(name.string);
                        if (method == null) return error.UndefinedProperty;
                        _ = self.pop();
                        try self.push(method.?);
                    }
                },
                .set_property => {
                    const instance = self.peek(1);
                    if (instance != .obj or instance.obj.obj_type != .instance) return error.OnlyInstancesHaveProperties;
                    const inst: *ObjInstance = @ptrCast(instance.obj);
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;

                    const value = self.peek(0);
                    try inst.fields.put(name.string, value);
                    _ = self.pop();
                },
                .method => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const method = self.peek(0);
                    const class_def: *ObjClass = @ptrCast(self.stack[self.stack_top - 2].obj);
                    try class_def.methods.put(name.string, method);
                    _ = self.pop();
                },
                .invoke => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const arg_count = self.readByte(frame);

                    const receiver = self.peek(arg_count);
                    if (receiver != .obj or receiver.obj.obj_type != .instance) {
                        return error.OnlyInstancesHaveMethods;
                    }
                    const instance: *ObjInstance = @ptrCast(receiver.obj);

                    if (instance.fields.get(name.string)) |value| {
                        self.stack[self.stack_top - arg_count - 1] = value;
                        if (!self.callValue(value, arg_count)) return .runtime_error;
                    } else {
                        if (!self.invokeFromClass(instance.class_def, name.string, arg_count)) {
                            return .runtime_error;
                        }
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .inherit => {
                    const superclass = self.peek(1);
                    if (superclass != .obj or superclass.obj.obj_type != .class) return error.SuperclassMustBeClass;
                    const subclass = self.peek(0);
                    if (subclass != .obj or subclass.obj.obj_type != .class) return error.RuntimeError;
                    const sub: *ObjClass = @ptrCast(subclass.obj);
                    sub.superclass = @ptrCast(superclass.obj);
                    _ = self.pop();
                },
                .get_super => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const superclass = self.pop();
                    if (superclass != .obj or superclass.obj.obj_type != .class) return error.RuntimeError;
                    const sc: *ObjClass = @ptrCast(superclass.obj);

                    if (!self.bindMethod(sc, name.string)) {
                        return .runtime_error;
                    }
                },
                .super_invoke => {
                    const name = self.readConstant(frame);
                    if (name != .string) return error.OperandsMustBeStrings;
                    const arg_count = self.readByte(frame);
                    const superclass = self.pop();
                    if (superclass != .obj or superclass.obj.obj_type != .class) return error.RuntimeError;
                    const sc: *ObjClass = @ptrCast(superclass.obj);

                    if (!self.invokeFromClass(sc, name.string, arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .print => {
                    self.printValue(self.pop());
                },
                .@"return" => {
                    const result = self.pop();
                    // Close upvalues captured by this frame
                    self.closeUpvalues(frame.stack_offset);

                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        // Top-level script finished — nothing more to pop
                        return .ok;
                    }

                    self.stack_top = frame.stack_offset;
                    try self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
            }
        }
    }

    fn readByte(self: *VM, frame: *CallFrame) u8 {
        _ = self;
        const byte = frame.closure.func.chunk.readByte(frame.ip);
        frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM, frame: *CallFrame) u16 {
        _ = self;
        const short = frame.closure.func.chunk.readShort(frame.ip);
        frame.ip += 2;
        return short;
    }

    fn readConstant(self: *VM, frame: *CallFrame) Value {
        const index = self.readByte(frame);
        return frame.closure.func.chunk.readConstant(index);
    }

    fn binaryOp(self: *VM, op: enum { add, subtract, multiply, divide }) !void {
        const b = self.pop();
        const a = self.pop();

        if (a == .number and b == .number) {
            const result = switch (op) {
                .add => a.number + b.number,
                .subtract => a.number - b.number,
                .multiply => a.number * b.number,
                .divide => a.number / b.number,
            };
            try self.push(.{ .number = result });
            return;
        }

        if (a == .string and b == .string and op == .add) {
            const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a.string, b.string });
            try self.push(.{ .string = result });
            return;
        }

        return error.OperandsMustBeNumbers;
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) bool {
        if (callee == .obj) {
            switch (callee.obj.obj_type) {
                .closure => return self.call(@ptrCast(callee.obj), arg_count),
                .native => {
                    const native_fn: *ObjNative = @ptrCast(callee.obj);
                    const args = self.stack[self.stack_top - arg_count .. self.stack_top];
                    const result = native_fn.function(self, args);
                    self.stack_top -= arg_count + 1;
                    self.push(result) catch return false;
                    return true;
                },
                .class => {
                    // Constructor call: create instance
                    const class_def: *ObjClass = @ptrCast(callee.obj);
                    const instance = self.allocObject(ObjInstance) catch return false;
                    instance.* = ObjInstance.init(self.allocator, class_def);
                    self.stack[self.stack_top - arg_count - 1] = .{ .obj = &instance.obj };

                    // Call init if it exists
                    if (class_def.methods.get("init")) |init_method| {
                        return self.callValue(init_method, arg_count);
                    } else if (arg_count != 0) {
                        // No init but arguments provided — error
                        return false;
                    }
                    return true;
                },
                .bound_method => {
                    const bound: *ObjBoundMethod = @ptrCast(callee.obj);
                    self.stack[self.stack_top - arg_count - 1] = bound.receiver;
                    return self.call(bound.method, arg_count);
                },
                else => return false,
            }
        }
        return false;
    }

    fn call(self: *VM, closure: *ObjClosure, arg_count: u8) bool {
        if (arg_count != closure.func.arity) {
            return false;
        }

        if (self.frame_count >= FRAMES_MAX) return false;

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = 0;
        frame.stack_offset = self.stack_top - arg_count - 1;

        return true;
    }

    fn invokeFromClass(self: *VM, class_def: *ObjClass, name: []const u8, arg_count: u8) bool {
        const method = class_def.methods.get(name) orelse return false;
        return self.callValue(method, arg_count);
    }

    fn bindMethod(self: *VM, class_def: *ObjClass, name: []const u8) bool {
        const method_value = class_def.methods.get(name) orelse return false;
        const bound = self.allocObject(ObjBoundMethod) catch return false;
        bound.obj = .{ .obj_type = .bound_method, .next = null };
        bound.receiver = self.peek(0);
        bound.method = @ptrCast(method_value.obj);
        _ = self.pop();
        self.push(.{ .obj = &bound.obj }) catch return false;
        return true;
    }

    fn captureUpvalue(self: *VM, local: usize) !*ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;

        while (upvalue) |uv| {
            if (@intFromPtr(uv.location) <= @intFromPtr(&self.stack[local])) break;
            prev_upvalue = uv;
            upvalue = uv.next;
        }

        if (upvalue != null and @intFromPtr(upvalue.?.location) == @intFromPtr(&self.stack[local])) {
            return upvalue.?;
        }

        const created = try self.allocObject(ObjUpvalue);
        created.* = ObjUpvalue{
            .obj = .{ .obj_type = .upvalue, .next = null },
            .location = &self.stack[local],
            .next = upvalue,
        };

        if (prev_upvalue) |pv| {
            pv.next = @ptrCast(&created.obj);
        } else {
            self.open_upvalues = @ptrCast(&created.obj);
        }

        return created;
    }

    fn closeUpvalues(self: *VM, last: usize) void {
        while (self.open_upvalues) |uv| {
            if (@intFromPtr(uv.location) < @intFromPtr(&self.stack[last])) break;
            uv.closed = uv.location.*;
            uv.location = &uv.closed;
            self.open_upvalues = uv.next;
        }
    }

    fn push(self: *VM, value: Value) !void {
        if (self.stack_top >= STACK_MAX) return error.StackOverflow;
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
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
                const func: *ObjFunction = @fieldParentPtr("obj", obj);
                func.deinit();
                self.allocator.destroy(func);
            },
            .closure => {
                const closure: *ObjClosure = @fieldParentPtr("obj", obj);
                closure.deinit(self.allocator);
                self.allocator.destroy(closure);
            },
            .upvalue => {
                const upvalue: *ObjUpvalue = @fieldParentPtr("obj", obj);
                self.allocator.destroy(upvalue);
            },
            .class => {
                const class_def: *ObjClass = @fieldParentPtr("obj", obj);
                class_def.deinit();
                self.allocator.destroy(class_def);
            },
            .instance => {
                const instance: *ObjInstance = @fieldParentPtr("obj", obj);
                instance.deinit();
                self.allocator.destroy(instance);
            },
            .bound_method => {
                const bound: *ObjBoundMethod = @fieldParentPtr("obj", obj);
                self.allocator.destroy(bound);
            },
            .native => {
                const native_fn: *ObjNative = @fieldParentPtr("obj", obj);
                self.allocator.destroy(native_fn);
            },
            .module => {
                const module: *ObjModule = @fieldParentPtr("obj", obj);
                module.deinit();
                self.allocator.destroy(module);
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
                    .upvalue => std.debug.print("<upvalue>\n", .{}),
                }
            },
            .nil => std.debug.print("nil\n", .{}),
        }
    }

    fn traceBytecode(vm: *VM, frame: *CallFrame) !void {
        std.debug.print("          ", .{});
        for (0..vm.stack_top) |_| {
            std.debug.print("[ ]", .{});
        }
        std.debug.print("\n", .{});
        _ = frame.closure.func.chunk.disassembleInstruction(frame.ip);
    }
};

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const Lexer = @import("lexer.zig").Lexer;
const Compiler = @import("compiler.zig").Compiler;
