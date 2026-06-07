//! Stack-based virtual machine for executing bytecode
const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const STACK_MAX = 256;

pub const VM = struct {
    chunk: *Chunk,
    allocator: std.mem.Allocator,
    globals: std.StringHashMap(Value),
    ip: usize = 0, // instruction pointer
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,

    pub fn init(chunk: *Chunk, allocator: std.mem.Allocator) VM {
        return VM{
            .chunk = chunk,
            .allocator = allocator,
            .globals = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
    }

    pub fn run(self: *VM) !InterpretResult {
        return self.execute();
    }

    fn execute(self: *VM) !InterpretResult {
        while (true) {
            // Uncomment for bytecode tracing
            // try self.traceBytecode();

            const instruction: OpCode = @enumFromInt(self.chunk.code[self.ip]);
            self.ip += 1;

            switch (instruction) {
                .@"return" => {
                    if (self.stack_top > 0) {
                        const result = self.pop();
                        self.printValue(result);
                    }
                    return .ok;
                },
                .constant => {
                    const index = self.chunk.code[self.ip];
                    self.ip += 1;
                    const value = self.chunk.constants[index];
                    try self.push(value);
                },
                .define_global => {
                    const name_index = self.chunk.code[self.ip];
                    self.ip += 1;
                    const name = self.chunk.constants[name_index];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const value = self.pop();
                    try self.globals.put(name.string, value);
                },
                .get_global => {
                    const name_index = self.chunk.code[self.ip];
                    self.ip += 1;
                    const name = self.chunk.constants[name_index];
                    if (name != .string) return error.OperandsMustBeStrings;
                    const value = self.globals.get(name.string) orelse return error.UndefinedVariable;
                    try self.push(value);
                },
                .set_global => {
                    const name_index = self.chunk.code[self.ip];
                    self.ip += 1;
                    const name = self.chunk.constants[name_index];
                    if (name != .string) return error.OperandsMustBeStrings;
                    if (!self.globals.contains(name.string)) return error.UndefinedVariable;
                    const value = self.pop();
                    try self.globals.put(name.string, value);
                },
                .pop => {
                    _ = self.pop();
                },
                .nil => try self.push(.nil),
                .true => try self.push(.{ .boolean = true }),
                .false => try self.push(.{ .boolean = false }),
                .add => try self.binaryOp(.add),
                .subtract => try self.binaryOp(.subtract),
                .multiply => try self.binaryOp(.multiply),
                .divide => try self.binaryOp(.divide),
                .negate => {
                    const value = self.pop();
                    if (value != .number) {
                        return error.OperandMustBeNumber;
                    }
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
                    if (a != .number or b != .number) {
                        return error.OperandsMustBeNumbers;
                    }
                    try self.push(.{ .boolean = a.number > b.number });
                },
                .less => {
                    const b = self.pop();
                    const a = self.pop();
                    if (a != .number or b != .number) {
                        return error.OperandsMustBeNumbers;
                    }
                    try self.push(.{ .boolean = a.number < b.number });
                },
                .print => {
                    const value = self.pop();
                    self.printValue(value);
                },
            }
        }
    }

    fn binaryOp(self: *VM, op: enum { add, subtract, multiply, divide }) !void {
        const b = self.pop();
        const a = self.pop();

        if (a != .number or b != .number) {
            return error.OperandsMustBeNumbers;
        }

        const result = switch (op) {
            .add => a.number + b.number,
            .subtract => a.number - b.number,
            .multiply => a.number * b.number,
            .divide => a.number / b.number,
        };

        try self.push(.{ .number = result });
    }

    fn push(self: *VM, value: Value) !void {
        if (self.stack_top >= STACK_MAX) {
            return error.StackOverflow;
        }
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM) Value {
        return self.stack[self.stack_top - 1];
    }

    fn printValue(self: *VM, value: Value) void {
        _ = self;
        switch (value) {
            .number => |number| std.debug.print("{}\n", .{number}),
            .boolean => |boolean| std.debug.print("{}\n", .{boolean}),
            .string => |string| std.debug.print("{s}\n", .{string}),
            .function_template => |function_template| std.debug.print("<fn {s}>\n", .{function_template.name}),
            .closure => |closure| std.debug.print("<closure {s}>\n", .{closure.template.name}),
            .class_def => |class_def| std.debug.print("<class {s}>\n", .{class_def.name}),
            .instance => |instance| std.debug.print("<instance {s}>\n", .{instance.class.name}),
            .module => std.debug.print("<module>\n", .{}),
            .nil => std.debug.print("nil\n", .{}),
        }
    }

    fn traceBytecode(self: *VM) !void {
        std.debug.print("          ", .{});
        for (0..self.stack_top) |_| {
            std.debug.print("[ ]", .{});
        }
        std.debug.print("\n", .{});
        _ = self.chunk.disassembleInstruction(self.ip);
    }
};

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};
