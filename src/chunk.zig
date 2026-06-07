//! Bytecode chunk - contains instructions and constants
const std = @import("std");
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    constant,
    define_global,
    get_global,
    set_global,
    pop,
    add,
    subtract,
    multiply,
    divide,
    negate,
    @"return",
    nil,
    true,
    false,
    equal,
    greater,
    less,
    not,
    print,
};

const MAX_CHUNK = 4096;
const MAX_CONSTANTS = 256;

pub const Chunk = struct {
    code: [MAX_CHUNK]u8 = undefined,
    lines: [MAX_CHUNK]usize = undefined,
    constants: [MAX_CONSTANTS]Value = undefined,
    code_count: usize = 0,
    constant_count: usize = 0,

    pub fn init() Chunk {
        return .{};
    }

    pub fn deinit(self: *Chunk) void {
        _ = self;
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        if (self.code_count >= MAX_CHUNK) return error.ChunkTooLarge;
        self.code[self.code_count] = byte;
        self.lines[self.code_count] = line;
        self.code_count += 1;
    }

    pub fn addConstant(self: *Chunk, value: Value) !u8 {
        if (self.constant_count >= MAX_CONSTANTS) return error.TooManyConstants;
        self.constants[self.constant_count] = value;
        self.constant_count += 1;
        return @intCast(self.constant_count - 1);
    }

    pub fn disassemble(self: *const Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < self.code_count) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *const Chunk, offset: usize) usize {
        std.debug.print("{:0>4} ", .{offset});

        if (offset > 0 and self.lines[offset] == self.lines[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{:>4} ", .{self.lines[offset]});
        }

        const instruction: OpCode = @enumFromInt(self.code[offset]);
        return switch (instruction) {
            .constant => self.constantInstruction("CONSTANT", offset),
            .define_global => self.constantInstruction("DEFINE_GLOBAL", offset),
            .get_global => self.constantInstruction("GET_GLOBAL", offset),
            .set_global => self.constantInstruction("SET_GLOBAL", offset),
            .pop => self.simpleInstruction("POP", offset),
            .add => self.simpleInstruction("ADD", offset),
            .subtract => self.simpleInstruction("SUBTRACT", offset),
            .multiply => self.simpleInstruction("MULTIPLY", offset),
            .divide => self.simpleInstruction("DIVIDE", offset),
            .negate => self.simpleInstruction("NEGATE", offset),
            .@"return" => self.simpleInstruction("RETURN", offset),
            .nil => self.simpleInstruction("NIL", offset),
            .true => self.simpleInstruction("TRUE", offset),
            .false => self.simpleInstruction("FALSE", offset),
            .equal => self.simpleInstruction("EQUAL", offset),
            .greater => self.simpleInstruction("GREATER", offset),
            .less => self.simpleInstruction("LESS", offset),
            .not => self.simpleInstruction("NOT", offset),
            .print => self.simpleInstruction("PRINT", offset),
        };
    }

    fn simpleInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        _ = self;
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn constantInstruction(self: *const Chunk, name: []const u8, offset: usize) usize {
        const index = self.code[offset + 1];
        std.debug.print("{s:16} {:>4} {}\n", .{ name, index, self.constants[index] });
        return offset + 2;
    }
};
