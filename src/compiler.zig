//! Compiler - converts tokens to bytecode
const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const Compiler = struct {
    lexer: *Lexer,
    current: Token,
    previous: Token,
    chunk: *Chunk,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(lexer: *Lexer, chunk: *Chunk) Compiler {
        var compiler = Compiler{
            .lexer = lexer,
            .current = undefined,
            .previous = undefined,
            .chunk = chunk,
        };
        compiler.advance();
        return compiler;
    }

    pub fn compile(self: *Compiler) !bool {
        while (!self.check(.eof)) {
            self.declaration();
        }

        self.emitOpcode(.@"return");

        return !self.had_error;
    }

    fn declaration(self: *Compiler) void {
        if (self.match(&[_]TokenType{.@"var"})) {
            self.varDeclaration();
            return;
        }

        self.statement();
    }

    fn statement(self: *Compiler) void {
        if (self.match(&[_]TokenType{.print})) {
            self.printStatement();
            return;
        }

        if (self.current.type == .identifier) {
            const name_token = self.current;
            self.advance();

            if (self.match(&[_]TokenType{.equal})) {
                self.expression();
                self.consume(.semicolon, "Expect ';' after assignment.");
                self.emitBytes(@intFromEnum(OpCode.set_global), self.identifierConstant(name_token));
                return;
            }
        }

        self.expressionStatement();
    }

    fn printStatement(self: *Compiler) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after value.");
        self.emitOpcode(.print);
    }

    fn expressionStatement(self: *Compiler) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after expression.");
        self.emitOpcode(.pop);
    }

    fn varDeclaration(self: *Compiler) void {
        const name_constant = self.parseVariable("Expect variable name.");

        if (self.match(&[_]TokenType{.equal})) {
            self.expression();
        } else {
            self.emitOpcode(.nil);
        }

        self.consume(.semicolon, "Expect ';' after variable declaration.");
        self.emitBytes(@intFromEnum(OpCode.define_global), name_constant);
    }

    fn expression(self: *Compiler) void {
        self.parseComparison();
    }

    fn parseComparison(self: *Compiler) void {
        self.parseEquality();

        while (self.match(&[_]TokenType{ .greater, .greater_equal, .less, .less_equal })) {
            const op_type = self.previous.type;
            self.parseEquality();

            switch (op_type) {
                .greater => self.emitOpcode(.greater),
                .greater_equal => {
                    self.emitOpcode(.less);
                    self.emitOpcode(.not);
                },
                .less => self.emitOpcode(.less),
                .less_equal => {
                    self.emitOpcode(.greater);
                    self.emitOpcode(.not);
                },
                else => {},
            }
        }
    }

    fn parseEquality(self: *Compiler) void {
        self.parseTerm();

        while (self.match(&[_]TokenType{ .bang_equal, .equal_equal })) {
            const op_type = self.previous.type;
            self.parseTerm();

            switch (op_type) {
                .equal_equal => self.emitOpcode(.equal),
                .bang_equal => {
                    self.emitOpcode(.equal);
                    self.emitOpcode(.not);
                },
                else => {},
            }
        }
    }

    fn parseTerm(self: *Compiler) void {
        self.parseFactor();

        while (self.match(&[_]TokenType{ .plus, .minus })) {
            const op_type = self.previous.type;
            self.parseFactor();

            switch (op_type) {
                .plus => self.emitOpcode(.add),
                .minus => self.emitOpcode(.subtract),
                else => {},
            }
        }
    }

    fn parseFactor(self: *Compiler) void {
        self.parseUnary();

        while (self.match(&[_]TokenType{ .star, .slash })) {
            const op_type = self.previous.type;
            self.parseUnary();

            switch (op_type) {
                .star => self.emitOpcode(.multiply),
                .slash => self.emitOpcode(.divide),
                else => {},
            }
        }
    }

    fn parseUnary(self: *Compiler) void {
        if (self.match(&[_]TokenType{ .bang, .minus })) {
            const op_type = self.previous.type;
            self.parseUnary();

            switch (op_type) {
                .minus => self.emitOpcode(.negate),
                .bang => self.emitOpcode(.not),
                else => {},
            }
            return;
        }

        self.parsePrimary();
    }

    fn parsePrimary(self: *Compiler) void {
        if (self.match(&[_]TokenType{.nil})) {
            self.emitOpcode(.nil);
            return;
        }

        if (self.match(&[_]TokenType{.true_kw})) {
            self.emitOpcode(.true);
            return;
        }

        if (self.match(&[_]TokenType{.false_kw})) {
            self.emitOpcode(.false);
            return;
        }

        if (self.match(&[_]TokenType{.number})) {
            const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch {
                self.errorAtPrevious("Invalid number.");
                return;
            };
            self.emitConstant(.{ .number = value });
            return;
        }

        if (self.match(&[_]TokenType{.string})) {
            self.emitConstant(.{ .string = self.previous.lexeme[1 .. self.previous.lexeme.len - 1] });
            return;
        }

        if (self.match(&[_]TokenType{.identifier})) {
            self.emitBytes(@intFromEnum(OpCode.get_global), self.identifierConstant(self.previous));
            return;
        }

        if (self.match(&[_]TokenType{.left_paren})) {
            self.expression();
            self.consume(.right_paren, "Expect ')' after expression.");
            return;
        }

        self.errorAtCurrent("Expect expression.");
    }

    fn match(self: *Compiler, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Compiler, ttype: TokenType) bool {
        return self.current.type == ttype;
    }

    fn advance(self: *Compiler) void {
        self.previous = self.current;
        self.current = self.lexer.nextToken();

        if (self.current.type == .@"error") {
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Compiler, ttype: TokenType, message: []const u8) void {
        if (self.current.type == ttype) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn emitOpcode(self: *Compiler, opcode: OpCode) void {
        self.emitByte(@intFromEnum(opcode));
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.chunk.write(byte, self.previous.line) catch {
            std.debug.print("Memory error\n", .{});
        };
    }

    fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn parseVariable(self: *Compiler, message: []const u8) u8 {
        self.consume(.identifier, message);
        return self.identifierConstant(self.previous);
    }

    fn identifierConstant(self: *Compiler, token: Token) u8 {
        return self.chunk.addConstant(.{ .string = token.lexeme }) catch {
            std.debug.print("Memory error\n", .{});
            return 0;
        };
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        const index = self.chunk.addConstant(value) catch {
            std.debug.print("Memory error\n", .{});
            return;
        };
        self.emitByte(@intFromEnum(OpCode.constant));
        self.emitByte(index);
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        self.errorAt(self.current, message);
    }

    fn errorAtPrevious(self: *Compiler, message: []const u8) void {
        self.errorAt(self.previous, message);
    }

    fn errorAt(self: *Compiler, token: Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;

        std.debug.print("[line {}] Error", .{token.line});

        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type == .@"error") {
            // Nothing
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.had_error = true;
    }
};
