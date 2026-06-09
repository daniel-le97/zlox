//! Bytecode chunk - contains instructions and constants
const std = @import("std");
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    // Constants and literals
    constant,
    nil,
    true,
    false,

    // Globals
    define_global,
    get_global,
    set_global,

    // Locals
    get_local,
    set_local,

    // Stack
    pop,

    // Arithmetic
    add,
    subtract,
    multiply,
    divide,
    negate,

    // Comparison
    equal,
    greater,
    less,
    not,

    // Control flow
    jump_if_false,
    jump,
    loop,

    // Functions
    call,
    closure,
    get_upvalue,
    set_upvalue,
    close_upvalue,

    // Classes
    class,
    get_property,
    set_property,
    method,
    invoke,
    inherit,
    get_super,
    super_invoke,

    // I/O
    print,

    // Exit
    @"return",
};

pub const Chunk = struct {
    allocator: std.mem.Allocator,
    code: std.ArrayList(u8),
    lines: std.ArrayList(usize),
    constants: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return .{
            .allocator = allocator,
            .code = .{ .items = &.{}, .capacity = 0 },
            .lines = .{ .items = &.{}, .capacity = 0 },
            .constants = .{ .items = &.{}, .capacity = 0 },
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.allocator);
        self.lines.deinit(self.allocator);
        self.constants.deinit(self.allocator);
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(self.allocator, byte);
        try self.lines.append(self.allocator, line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        try self.constants.append(self.allocator, value);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn readByte(self: *const Chunk, offset: usize) u8 {
        return self.code.items[offset];
    }

    pub fn readShort(self: *const Chunk, offset: usize) u16 {
        return (@as(u16, self.code.items[offset]) << 8) | self.code.items[offset + 1];
    }

    pub fn readConstant(self: *const Chunk, offset: usize) Value {
        return self.constants.items[offset];
    }

    pub fn writeAt(self: *Chunk, offset: usize, byte: u8) void {
        self.code.items[offset] = byte;
    }

    pub fn disassemble(self: *const Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *const Chunk, offset: usize) usize {
        std.debug.print("{:0>4} ", .{offset});

        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{:>4} ", .{self.lines.items[offset]});
        }

        const instruction: OpCode = @enumFromInt(self.code.items[offset]);
        return switch (instruction) {
            .nil => self.simpleInstruction("NIL", offset),
            .true => self.simpleInstruction("TRUE", offset),
            .false => self.simpleInstruction("FALSE", offset),
            .constant => self.constantInstruction("CONSTANT", offset),
            .define_global => self.constantInstruction("DEFINE_GLOBAL", offset),
            .get_global => self.constantInstruction("GET_GLOBAL", offset),
            .set_global => self.constantInstruction("SET_GLOBAL", offset),
            .get_local => self.byteInstruction("GET_LOCAL", offset),
            .set_local => self.byteInstruction("SET_LOCAL", offset),
            .pop => self.simpleInstruction("POP", offset),
            .add => self.simpleInstruction("ADD", offset),
            .subtract => self.simpleInstruction("SUBTRACT", offset),
            .multiply => self.simpleInstruction("MULTIPLY", offset),
            .divide => self.simpleInstruction("DIVIDE", offset),
            .negate => self.simpleInstruction("NEGATE", offset),
            .equal => self.simpleInstruction("EQUAL", offset),
            .greater => self.simpleInstruction("GREATER", offset),
            .less => self.simpleInstruction("LESS", offset),
            .not => self.simpleInstruction("NOT", offset),
            .jump_if_false => self.jumpInstruction("JUMP_IF_FALSE", 1, offset),
            .jump => self.jumpInstruction("JUMP", 1, offset),
            .loop => self.jumpInstruction("LOOP", -1, offset),
            .call => self.byteInstruction("CALL", offset),
            .closure => self.twoByteInstruction("CLOSURE", offset),
            .get_upvalue => self.byteInstruction("GET_UPVALUE", offset),
            .set_upvalue => self.byteInstruction("SET_UPVALUE", offset),
            .close_upvalue => self.simpleInstruction("CLOSE_UPVALUE", offset),
            .class => self.constantInstruction("CLASS", offset),
            .get_property => self.constantInstruction("GET_PROPERTY", offset),
            .set_property => self.constantInstruction("SET_PROPERTY", offset),
            .method => self.constantInstruction("METHOD", offset),
            .invoke => self.invokeInstruction("INVOKE", offset),
            .inherit => self.simpleInstruction("INHERIT", offset),
            .get_super => self.constantInstruction("GET_SUPER", offset),
            .super_invoke => self.invokeInstruction("SUPER_INVOKE", offset),
            .print => self.simpleInstruction("PRINT", offset),
            .@"return" => self.simpleInstruction("RETURN", offset),
        };
    }

    fn simpleInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        _ = self;
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const index = self.code.items[offset + 1];
        std.debug.print("{s:16} {:>4} {}\n", .{ name, index, self.constants.items[index] });
        return offset + 2;
    }

    fn byteInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        std.debug.print("{s:16} {:>4}\n", .{ name, slot });
        return offset + 2;
    }

    fn twoByteInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const count = self.code.items[offset + 2];
        std.debug.print("{s:16} {:>4} {}\n", .{ name, constant, count });
        return offset + 3;
    }

    fn jumpInstruction(self: *const Chunk, name: []const u8, sign: i32, offset: usize) usize {
        const jump: u16 = (@as(u16, self.code.items[offset + 1]) << 8) | self.code.items[offset + 2];
        const target: i32 = @as(i32, @intCast(offset)) + 3 + sign * @as(i32, @intCast(jump));
        std.debug.print("{s:16} {:>4} -> {}\n", .{ name, offset, target });
        return offset + 3;
    }

    fn invokeInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        const arg_count = self.code.items[offset + 2];
        std.debug.print("{s:16} ({d} args) {:>4} {}\n", .{ name, arg_count, constant, self.constants.items[constant] });
        return offset + 3;
    }
};
