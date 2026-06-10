//! Bytecode chunk - contains instructions and constants
//! Register-based instruction set.
//! Instructions have variable-length encoding:
//!   Rn = register index (u8)
//!   cx = constant index (u8)
//!   j16 = jump offset (u16, big-endian)
const std = @import("std");
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    // ── Load/store (op + dst_reg + [src]) ──
    load_const, //     R_dst, cx
    load_nil, //       R_dst
    load_true, //      R_dst
    load_false, //     R_dst

    // ── Globals (op + const_idx + [reg]) ──
    get_global, //     R_dst, cx
    set_global, //     cx, R_src
    define_global, //  cx, R_src

    // ── Upvalues ──
    get_upvalue, //    R_dst, slot
    set_upvalue, //    slot, R_src
    close_upvalue, //  R_src

    // ── Arithmetic: op + dst + src1 + src2 ──
    add, //            R_dst, R_a, R_b (general: number or string)
    sub, //            R_dst, R_a, R_b (general, with type check)
    mul, //            R_dst, R_a, R_b
    div, //            R_dst, R_a, R_b
    // Fast numeric-only variants (no type tag check)
    add_number, //     R_dst, R_a, R_b
    sub_number, //     R_dst, R_a, R_b
    mul_number, //     R_dst, R_a, R_b
    div_number, //     R_dst, R_a, R_b
    // Constant-folded: R_dst = R_src OP constant[cx]
    sub_const, //      R_dst, R_src, cx

    // ── Unary: op + dst + src ──
    negate, //         R_dst, R_src (general, with type check)
    negate_number, //  R_dst, R_src (fast, assumes number)
    not_register, //   R_dst, R_src

    // ── Comparison: op + dst + a + b ──
    equal, //          R_dst, R_a, R_b (general)
    greater, //        R_dst, R_a, R_b (general, with type check)
    less, //           R_dst, R_a, R_b
    greater_number, // R_dst, R_a, R_b (fast, assumes numbers)
    less_number, //    R_dst, R_a, R_b

    // ── Control flow ──
    jump_if_false, //  R_src, j16
    jump, //           j16
    loop, //           j16

    // ── Data movement ──
    move, //           R_dst, R_src

    // ── Functions ──
    call, //           R_dst, R_callee, argc
    call_self, //      R_dst, argc — self-recursive call, args at R_dst+1
    closure, //        R_dst, cx  (+ upvalue data follows)

    // ── Objects ──
    class, //          R_dst, cx
    get_property, //   R_dst, R_inst, cx
    set_property, //   R_inst, cx, R_src
    method, //         R_class, cx, R_method
    invoke, //         R_dst, R_inst, cx, argc
    inherit, //        R_subclass, R_superclass
    get_super, //      R_dst, cx
    super_invoke, //   R_dst, R_inst, cx, argc

    // ── I/O and modules ──
    import_module, //  path_cx, name_cx
    print, //          R_src

    // ── Exit ──
    @"return", //      R_src
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
            .load_const => self.rconst("LOAD_CONST", offset),
            .load_nil => self.reg("LOAD_NIL", offset, 1),
            .load_true => self.reg("LOAD_TRUE", offset, 1),
            .load_false => self.reg("LOAD_FALSE", offset, 1),
            .get_global => self.rconst("GET_GLOBAL", offset),
            .set_global => self.constReg("SET_GLOBAL", offset),
            .define_global => self.constReg("DEFINE_GLOBAL", offset),
            .get_upvalue => self.reg("GET_UPVALUE", offset, 2),
            .set_upvalue => self.reg("SET_UPVALUE", offset, 2),
            .close_upvalue => self.reg("CLOSE_UPVALUE", offset, 1),
            .add => self.reg3("ADD", offset),
            .sub => self.reg3("SUB", offset),
            .mul => self.reg3("MUL", offset),
            .div => self.reg3("DIV", offset),
            .add_number => self.reg3("ADD_NUM", offset),
            .sub_number => self.reg3("SUB_NUM", offset),
            .mul_number => self.reg3("MUL_NUM", offset),
            .div_number => self.reg3("DIV_NUM", offset),
            .sub_const => self.reg3("SUB_CONST", offset),
            .negate => self.reg2("NEGATE", offset),
            .negate_number => self.reg2("NEGATE_NUM", offset),
            .not_register => self.reg2("NOT", offset),
            .equal => self.reg3("EQUAL", offset),
            .greater => self.reg3("GREATER", offset),
            .less => self.reg3("LESS", offset),
            .greater_number => self.reg3("GREATER_NUM", offset),
            .less_number => self.reg3("LESS_NUM", offset),
            .jump_if_false => self.regJump("JUMP_IF_FALSE", offset),
            .jump => self.jumpInstruction("JUMP", 1, offset),
            .loop => self.jumpInstruction("LOOP", -1, offset),
            .move => self.reg2("MOVE", offset),
            .call => self.reg2("CALL", offset),
            .call_self => self.reg2("CALL_SELF", offset),
            .closure => self.rconst("CLOSURE", offset),
            .class => self.rconst("CLASS", offset),
            .get_property => self.rconst2("GET_PROPERTY", offset),
            .set_property => self.rconst("SET_PROPERTY", offset),
            .method => self.reg3("METHOD", offset),
            .invoke => self.reg2const("INVOKE", offset),
            .inherit => self.reg2("INHERIT", offset),
            .get_super => self.rconst("GET_SUPER", offset),
            .super_invoke => self.reg2const("SUPER_INVOKE", offset),
            .import_module => self.reg2("IMPORT", offset),
            .print => self.reg("PRINT", offset, 1),
            .@"return" => self.reg("RETURN", offset, 1),
        };
    }

    // ── Disassembly helpers ──

    fn reg(self: *const Chunk, name: []const u8, offset: usize, n: usize) usize {
        if (n == 1) {
            std.debug.print("{s:16} R{}\n", .{ name, self.code.items[offset + 1] });
            return offset + 2;
        }
        // n == 2: two byte operands
        std.debug.print("{s:16} R{} R{}\n", .{ name, self.code.items[offset + 1], self.code.items[offset + 2] });
        return offset + 3;
    }

    fn reg2(self: *const Chunk, name: []const u8, offset: usize) usize {
        std.debug.print("{s:16} R{}, R{}\n", .{ name, self.code.items[offset + 1], self.code.items[offset + 2] });
        return offset + 3;
    }

    fn reg3(self: *const Chunk, name: []const u8, offset: usize) usize {
        std.debug.print("{s:16} R{}, R{}, R{}\n", .{ name, self.code.items[offset + 1], self.code.items[offset + 2], self.code.items[offset + 3] });
        return offset + 4;
    }

    fn rconst(self: *const Chunk, name: []const u8, offset: usize) usize {
        const ri = self.code.items[offset + 1];
        const ci = self.code.items[offset + 2];
        std.debug.print("{s:16} R{}, const[{}] = {}\n", .{ name, ri, ci, self.constants.items[ci] });
        return offset + 3;
    }

    fn constReg(self: *const Chunk, name: []const u8, offset: usize) usize {
        const ci = self.code.items[offset + 1];
        const ri = self.code.items[offset + 2];
        std.debug.print("{s:16} const[{}] = {}, R{}\n", .{ name, ci, self.constants.items[ci], ri });
        return offset + 3;
    }

    fn rconst2(self: *const Chunk, name: []const u8, offset: usize) usize {
        const rd = self.code.items[offset + 1];
        const ri = self.code.items[offset + 2];
        const ci = self.code.items[offset + 3];
        std.debug.print("{s:16} R{}, R{}, const[{}]\n", .{ name, rd, ri, ci });
        return offset + 4;
    }

    fn reg2const(self: *const Chunk, name: []const u8, offset: usize) usize {
        const rd = self.code.items[offset + 1];
        const ri = self.code.items[offset + 2];
        const ci = self.code.items[offset + 3];
        const ac = self.code.items[offset + 4];
        std.debug.print("{s:16} R{}, R{}, const[{}], {}args\n", .{ name, rd, ri, ci, ac });
        return offset + 5;
    }

    fn regJump(self: *const Chunk, name: []const u8, offset: usize) usize {
        const r_val = self.code.items[offset + 1];
        const jump: u16 = (@as(u16, self.code.items[offset + 2]) << 8) | self.code.items[offset + 3];
        const target: i32 = @as(i32, @intCast(offset)) + 4 + @as(i32, @intCast(jump));
        std.debug.print("{s:16} R{}, -> {}\n", .{ name, r_val, target });
        return offset + 4;
    }

    fn jumpInstruction(self: *const Chunk, name: []const u8, sign: i32, offset: usize) usize {
        const jump: u16 = (@as(u16, self.code.items[offset + 1]) << 8) | self.code.items[offset + 2];
        const target: i32 = @as(i32, @intCast(offset)) + 3 + sign * @as(i32, @intCast(jump));
        std.debug.print("{s:16} -> {}\n", .{ name, target });
        return offset + 3;
    }
};
