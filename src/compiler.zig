//! Compiler - converts tokens to bytecode
const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const ObjFunction = @import("value.zig").ObjFunction;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

const MAX_LOCALS = 256;
const MAX_UPVALUES = 256;
const MAX_PARAMS = @import("value.zig").MAX_PARAMS;

pub const FunctionType = enum {
    function,
    method,
    initializer,
    script,
};

const Local = struct {
    name: []const u8,
    depth: i32,
    is_captured: bool = false,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    current: Token,
    previous: Token,
    had_error: bool = false,
    panic_mode: bool = false,
    function: *ObjFunction,
    func_type: FunctionType,

    // Scope tracking
    locals: [MAX_LOCALS]Local = undefined,
    local_count: usize = 0,
    scope_depth: i32 = 0,

    // Upvalue tracking
    upvalues: [MAX_UPVALUES]Upvalue = undefined,

    // Enclosing compiler (for nested functions)
    enclosing: ?*Compiler = null,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer, func_type: FunctionType, enclosing: ?*Compiler, first_token: ?Token) !Compiler {
        const func = try allocator.create(ObjFunction);
        func.* = ObjFunction.init(allocator, "<script>");

        var compiler = Compiler{
            .allocator = allocator,
            .lexer = lexer,
            .current = undefined,
            .previous = undefined,
            .function = func,
            .func_type = func_type,
            .enclosing = enclosing,
            .local_count = 0,
            .scope_depth = 0,
        };

        // Reserve stack slot 0 for VM internal use
        compiler.locals[0] = .{ .name = "", .depth = 0, .is_captured = false };
        compiler.local_count = 1;

        if (first_token) |tok| {
            compiler.current = tok;
        } else {
            compiler.advance();
        }
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.function.deinit();
        self.allocator.destroy(self.function);
    }

    pub fn compile(self: *Compiler) !bool {
        while (!self.check(.eof)) {
            self.declaration();
        }

        self.emitOpcode(.@"return");

        return !self.had_error;
    }

    // === Declarations ===

    fn declaration(self: *Compiler) void {
        if (self.match(&[_]TokenType{.class})) {
            self.classDeclaration();
            return;
        }

        if (self.match(&[_]TokenType{.fun})) {
            self.funDeclaration();
            return;
        }

        if (self.match(&[_]TokenType{.@"var"})) {
            self.varDeclaration();
            return;
        }

        self.statement();
    }

    fn classDeclaration(self: *Compiler) void {
        const name = self.consumeIdentifier("Expect class name.") catch return;
        const name_constant = self.identifierConstant(name);

        self.emitBytes(@intFromEnum(OpCode.class), name_constant);

        if (self.match(&[_]TokenType{.less})) {
            const super_name = self.consumeIdentifier("Expect superclass name.") catch return;
            self.namedVariable(super_name, false);

            self.consume(.left_brace, "Expect '{' before class body.") catch return;
            self.emitOpcode(.inherit);
        } else {
            self.consume(.left_brace, "Expect '{' before class body.") catch return;
        }

        while (!self.check(.right_brace) and !self.check(.eof)) {
            const method_name = self.consumeIdentifier("Expect method name.") catch return;
            const method_constant = self.identifierConstant(method_name);

            const is_init = std.mem.eql(u8, method_name.lexeme, "init");
            const func_type: FunctionType = if (is_init) .initializer else .method;
            self.method(func_type);

            self.emitBytes(@intFromEnum(OpCode.method), method_constant);
        }

        // After all methods: current is the class closing brace
        self.consume(.right_brace, "Expect '}' after class body.") catch return;
    }

    fn method(self: *Compiler, func_type: FunctionType) void {
        self.consume(.left_paren, "Expect '(' after method name.") catch return;

        const func = self.allocator.create(ObjFunction) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func.* = ObjFunction.init(self.allocator, "");

        var method_compiler = Compiler.init(self.allocator, self.lexer, func_type, self, self.current) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };

        // Parse parameters
        if (!method_compiler.check(.right_paren)) {
            while (true) {
                method_compiler.function.arity += 1;
                if (method_compiler.function.arity > MAX_PARAMS) {
                    method_compiler.errorAtCurrent("Can't have more than 255 parameters.");
                }
                const param = method_compiler.consumeIdentifier("Expect parameter name.") catch return;
                method_compiler.addLocal(param);
                method_compiler.markInitialized();
                if (!method_compiler.match(&[_]TokenType{.comma})) break;
            }
        }

        method_compiler.consume(.right_paren, "Expect ')' after parameters.") catch return;
        method_compiler.consume(.left_brace, "Expect '{' before method body.") catch return;

        // Compile body
        method_compiler.beginScope();
        method_compiler.block();
        _ = method_compiler.endScope();

        // Sync parent's current from child
        self.current = method_compiler.current;

        method_compiler.emitOpcode(.nil);
        method_compiler.emitOpcode(.@"return");

        // Add compiled function as constant
        _ = self.function.chunk.addConstant(.{ .obj = &func.obj }) catch {
            method_compiler.errorAtCurrent("Too many constants.");
        };
    }

    fn funDeclaration(self: *Compiler) void {
        const name = self.consumeIdentifier("Expect function name.") catch return;
        const name_constant = self.identifierConstant(name);

        self.compileFunction(.function, name.lexeme);

        self.defineVariable(name_constant);
    }

    fn compileFunction(self: *Compiler, func_type: FunctionType, name: []const u8) void {
        const func = self.allocator.create(ObjFunction) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func.* = ObjFunction.init(self.allocator, name);

        var func_compiler = Compiler.init(self.allocator, self.lexer, func_type, self, self.current) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func_compiler.function.deinit();
        func_compiler.function = func;
        func_compiler.function.name = name;

        func_compiler.consume(.left_paren, "Expect '(' after function name.") catch {
            self.current = func_compiler.current;
            return;
        };

        // Parse parameters
        if (!func_compiler.check(.right_paren)) {
            while (true) {
                func_compiler.function.arity += 1;
                if (func_compiler.function.arity > MAX_PARAMS) {
                    func_compiler.errorAtCurrent("Can't have more than 255 parameters.");
                }
                const param = func_compiler.consumeIdentifier("Expect parameter name.") catch {
                    self.current = func_compiler.current;
                    return;
                };
                func_compiler.addLocal(param);
                func_compiler.markInitialized();
                if (!func_compiler.match(&[_]TokenType{.comma})) break;
            }
        }

        func_compiler.consume(.right_paren, "Expect ')' after parameters.") catch {
            self.current = func_compiler.current;
            return;
        };
        func_compiler.consume(.left_brace, "Expect '{' before function body.") catch {
            self.current = func_compiler.current;
            return;
        };

        // Compile body
        func_compiler.beginScope();
        func_compiler.block();
        _ = func_compiler.endScope();

        // Sync parent's current from child after block consumed the '}'
        self.current = func_compiler.current;

        // Implicit return
        if (func_type == .initializer) {
            func_compiler.emitBytes(@intFromEnum(OpCode.get_local), 0);
        } else {
            func_compiler.emitOpcode(.nil);
        }
        func_compiler.emitOpcode(.@"return");

        // Transfer compiled function as constant to parent
        _ = self.function.chunk.addConstant(.{ .obj = &func.obj }) catch {
            func_compiler.errorAtCurrent("Too many constants.");
            return;
        };

        // Emit closure opcode
        const const_idx = @as(u8, @intCast(self.function.chunk.constants.items.len - 1));
        self.emitBytes(@intFromEnum(OpCode.closure), const_idx);

        // Emit upvalue descriptors from the child compiler
        for (0..func.upvalue_count) |i| {
            self.emitByte(if (func_compiler.upvalues[i].is_local) 1 else 0);
            self.emitByte(func_compiler.upvalues[i].index);
        }
    }

    fn parseFunction(self: *Compiler, func_type: FunctionType, name: []const u8) void {
        // Legacy wrapper — used by methods
        const func = self.allocator.create(ObjFunction) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func.* = ObjFunction.init(self.allocator, name);

        var func_compiler = Compiler.init(self.allocator, self.lexer, func_type, self, self.current) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func_compiler.function.deinit();
        func_compiler.function = func;
        func_compiler.function.name = name;

        func_compiler.consume(.left_paren, "Expect '(' after function name.") catch return;

        // Parse parameters
        if (!func_compiler.check(.right_paren)) {
            while (true) {
                func_compiler.function.arity += 1;
                if (func_compiler.function.arity > MAX_PARAMS) {
                    func_compiler.errorAtCurrent("Can't have more than 255 parameters.");
                }
                const param = func_compiler.consumeIdentifier("Expect parameter name.") catch return;
                func_compiler.addLocal(param);
                func_compiler.markInitialized();
                if (!func_compiler.match(&[_]TokenType{.comma})) break;
            }
        }

        func_compiler.consume(.right_paren, "Expect ')' after parameters.") catch return;
        func_compiler.consume(.left_brace, "Expect '{' before function body.") catch return;

        // Compile body
        func_compiler.beginScope();
        func_compiler.block();
        _ = func_compiler.endScope();

        // Implicit return
        if (func_type == .initializer) {
            func_compiler.emitBytes(@intFromEnum(OpCode.get_local), 0);
        } else {
            func_compiler.emitOpcode(.nil);
        }
        func_compiler.emitOpcode(.@"return");

        // Transfer compiled function as constant to parent
        _ = self.function.chunk.addConstant(.{ .obj = &func.obj }) catch {
            func_compiler.errorAtCurrent("Too many constants.");
        };
    }

    fn varDeclaration(self: *Compiler) void {
        const name_constant = self.parseVariable("Expect variable name.");

        if (self.match(&[_]TokenType{.equal})) {
            self.expression();
        } else {
            self.emitOpcode(.nil);
        }

        self.consume(.semicolon, "Expect ';' after variable declaration.") catch return;
        self.defineVariable(name_constant);
    }

    // === Statements ===

    fn statement(self: *Compiler) void {
        if (self.match(&[_]TokenType{.print})) {
            self.printStatement();
            return;
        }

        if (self.match(&[_]TokenType{.if_kw})) {
            self.ifStatement();
            return;
        }

        if (self.match(&[_]TokenType{.while_kw})) {
            self.whileStatement();
            return;
        }

        if (self.match(&[_]TokenType{.for_kw})) {
            self.forStatement();
            return;
        }

        if (self.match(&[_]TokenType{.@"return"})) {
            self.returnStatement();
            return;
        }

        if (self.match(&[_]TokenType{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
            return;
        }

        if (self.match(&[_]TokenType{.break_kw})) {
            self.errorAtCurrent("'break' not yet supported in VM.");
            self.consume(.semicolon, "") catch return;
            return;
        }

        if (self.match(&[_]TokenType{.continue_kw})) {
            self.errorAtCurrent("'continue' not yet supported in VM.");
            self.consume(.semicolon, "") catch return;
            return;
        }

        self.expressionStatement();
    }

    fn printStatement(self: *Compiler) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after value.") catch return;
        self.emitOpcode(.print);
    }

    fn expressionStatement(self: *Compiler) void {
        self.expression();
        self.consume(.semicolon, "Expect ';' after expression.") catch return;
        self.emitOpcode(.pop);
    }

    fn ifStatement(self: *Compiler) void {
        self.consume(.left_paren, "Expect '(' after 'if'.") catch return;
        self.expression();
        self.consume(.right_paren, "Expect ')' after condition.") catch return;

        const then_jump = self.emitJump(.jump_if_false);
        self.emitOpcode(.pop);

        if (self.match(&.{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
        } else {
            self.statement();
        }

        const else_jump = self.emitJump(.jump);

        self.patchJump(then_jump);
        self.emitOpcode(.pop);

        if (self.match(&.{.else_kw})) {
            if (self.match(&.{.left_brace})) {
                self.beginScope();
                self.block();
                _ = self.endScope();
            } else {
                self.statement();
            }
        }

        self.patchJump(else_jump);
    }

    fn whileStatement(self: *Compiler) void {
        const loop_start = self.currentChunk().code.items.len;

        self.consume(.left_paren, "Expect '(' after 'while'.") catch return;
        self.expression();
        self.consume(.right_paren, "Expect ')' after condition.") catch return;

        const exit_jump = self.emitJump(.jump_if_false);
        self.emitOpcode(.pop);

        if (self.match(&.{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
        } else {
            self.statement();
        }

        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitOpcode(.pop);
    }

    fn forStatement(self: *Compiler) void {
        self.beginScope();

        self.consume(.left_paren, "Expect '(' after 'for'.") catch return;

        // Initializer
        if (self.match(&.{.semicolon})) {
            // No initializer
        } else if (self.match(&[_]TokenType{.@"var"})) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loop_start = self.currentChunk().code.items.len;

        // Condition
        var exit_jump: i32 = -1;
        if (!self.match(&.{.semicolon})) {
            self.expression();
            self.consume(.semicolon, "Expect ';' after loop condition.") catch return;
            exit_jump = self.emitJump(.jump_if_false);
            self.emitOpcode(.pop);
        }

        // Increment
        if (!self.match(&.{.right_paren})) {
            const body_jump = self.emitJump(.jump);
            const increment_start = self.currentChunk().code.items.len;
            self.expression();
            self.emitOpcode(.pop);
            self.consume(.right_paren, "Expect ')' after for clauses.") catch return;

            self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }

        // Body
        if (self.match(&.{.left_brace})) {
            self.block();
        } else {
            self.statement();
        }

        self.emitLoop(loop_start);

        if (exit_jump != -1) {
            self.patchJump(exit_jump);
            self.emitOpcode(.pop);
        }

        _ = self.endScope();
    }

    fn returnStatement(self: *Compiler) void {
        if (self.func_type == .script) {
            self.errorAtPrevious("Can't return from top-level code.");
        }

        if (self.match(&.{.semicolon})) {
            self.emitOpcode(.nil);
            self.emitOpcode(.@"return");
        } else {
            if (self.func_type == .initializer) {
                self.errorAtPrevious("Can't return a value from an initializer.");
            }
            self.expression();
            self.consume(.semicolon, "Expect ';' after return value.") catch return;
            self.emitOpcode(.@"return");
        }
    }

    fn block(self: *Compiler) void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            self.declaration();
        }

        self.consume(.right_brace, "Expect '}' after block.") catch return;
    }

    // === Expressions ===

    fn expression(self: *Compiler) void {
        self.assignment();
    }

    fn assignment(self: *Compiler) void {
        // Check for simple variable assignment: name = value
        if (self.current.type == .identifier) {
            const next = self.lexer.peekToken();
            if (next.type == .equal) {
                const name = self.current;
                self.advance(); // consume identifier
                self.advance(); // consume =

                // Determine if this is a local, upvalue, or global
                var set_op: OpCode = undefined;
                var arg: u8 = undefined;

                if (self.resolveLocal(name.lexeme)) |index| {
                    set_op = .set_local;
                    arg = index;
                } else if (self.resolveUpvalue(name.lexeme)) |index| {
                    set_op = .set_upvalue;
                    arg = index;
                } else {
                    set_op = .set_global;
                    arg = self.identifierConstant(name);
                }

                self.expression();
                self.emitBytes(@intFromEnum(set_op), arg);
                return;
            }
        }

        self.orExpr();

        if (self.match(&[_]TokenType{.equal})) {
            self.errorAtCurrent("Invalid assignment target.");
        }
    }

    fn orExpr(self: *Compiler) void {
        self.andExpr();

        while (self.match(&[_]TokenType{.or_kw})) {
            const else_jump = self.emitJump(.jump_if_false);
            const end_jump = self.emitJump(.jump);

            self.patchJump(else_jump);
            self.emitOpcode(.pop);

            self.andExpr();
            self.patchJump(end_jump);
        }
    }

    fn andExpr(self: *Compiler) void {
        self.equality();

        while (self.match(&[_]TokenType{.and_kw})) {
            const end_jump = self.emitJump(.jump_if_false);
            self.emitOpcode(.pop);
            self.equality();
            self.patchJump(end_jump);
        }
    }

    fn equality(self: *Compiler) void {
        self.comparison();

        while (self.match(&[_]TokenType{ .bang_equal, .equal_equal })) {
            const op_type = self.previous.type;
            self.comparison();

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

    fn comparison(self: *Compiler) void {
        self.term();

        while (self.match(&[_]TokenType{ .greater, .greater_equal, .less, .less_equal })) {
            const op_type = self.previous.type;
            self.term();

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

    fn term(self: *Compiler) void {
        self.factor();

        while (self.match(&[_]TokenType{ .plus, .minus })) {
            const op_type = self.previous.type;
            self.factor();

            switch (op_type) {
                .plus => self.emitOpcode(.add),
                .minus => self.emitOpcode(.subtract),
                else => {},
            }
        }
    }

    fn factor(self: *Compiler) void {
        self.unary();

        while (self.match(&[_]TokenType{ .star, .slash })) {
            const op_type = self.previous.type;
            self.unary();

            switch (op_type) {
                .star => self.emitOpcode(.multiply),
                .slash => self.emitOpcode(.divide),
                else => {},
            }
        }
    }

    fn unary(self: *Compiler) void {
        if (self.match(&[_]TokenType{ .bang, .minus })) {
            const op_type = self.previous.type;
            self.unary();

            switch (op_type) {
                .minus => self.emitOpcode(.negate),
                .bang => self.emitOpcode(.not),
                else => {},
            }
            return;
        }

        self.call();
    }

    fn call(self: *Compiler) void {
        self.primary();

        while (true) {
            if (self.match(&[_]TokenType{.left_paren})) {
                self.finishCall();
            } else if (self.match(&[_]TokenType{.dot})) {
                const name = self.consumeIdentifier("Expect property name after '.'.") catch return;
                const name_constant = self.identifierConstant(name);

                if (self.match(&[_]TokenType{.left_paren})) {
                    self.finishCall();
                } else {
                    self.emitBytes(@intFromEnum(OpCode.get_property), name_constant);
                }
            } else {
                break;
            }
        }
    }

    fn finishCall(self: *Compiler) void {
        var arg_count: u8 = 0;
        if (!self.check(.right_paren)) {
            while (true) {
                self.expression();
                arg_count += 1;
                if (arg_count > MAX_PARAMS) {
                    self.errorAtCurrent("Can't have more than 255 arguments.");
                }
                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        self.consume(.right_paren, "Expect ')' after arguments.") catch return;
        self.emitBytes(@intFromEnum(OpCode.call), arg_count);
    }

    fn primary(self: *Compiler) void {
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
            const str = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
            self.emitConstant(.{ .string = str });
            return;
        }

        if (self.match(&[_]TokenType{.this})) {
            self.emitBytes(@intFromEnum(OpCode.get_local), 0);
            return;
        }

        if (self.match(&[_]TokenType{.identifier})) {
            self.namedVariable(self.previous, true);
            return;
        }

        if (self.match(&[_]TokenType{.left_paren})) {
            self.expression();
            self.consume(.right_paren, "Expect ')' after expression.") catch return;
            return;
        }

        // Super calls
        if (self.match(&[_]TokenType{.super})) {
            self.consume(.dot, "Expect '.' after 'super'.") catch return;
            const super_method = self.consumeIdentifier("Expect superclass method name.") catch return;
            const method_constant = self.identifierConstant(super_method);

            self.namedVariable(.{ .type = .this, .lexeme = "this", .line = self.previous.line, .start = 0, .end = 0 }, false);

            if (self.match(&[_]TokenType{.left_paren})) {
                self.finishCall();
            } else {
                self.emitBytes(@intFromEnum(OpCode.get_super), method_constant);
            }
            return;
        }

        self.errorAtCurrent("Expect expression.");
    }

    fn namedVariable(self: *Compiler, token: Token, can_assign: bool) void {
        _ = can_assign;

        // Try to resolve as local
        if (self.resolveLocal(token.lexeme)) |index| {
            self.emitBytes(@intFromEnum(OpCode.get_local), index);
            return;
        }

        // Try to resolve as upvalue
        if (self.resolveUpvalue(token.lexeme)) |index| {
            self.emitBytes(@intFromEnum(OpCode.get_upvalue), index);
            return;
        }

        // Fall back to global
        self.emitBytes(@intFromEnum(OpCode.get_global), self.identifierConstant(token));
    }

    // === Local variables ===

    fn addLocal(self: *Compiler, token: Token) void {
        if (self.local_count >= MAX_LOCALS) {
            self.errorAtPrevious("Too many local variables in function.");
            return;
        }

        self.locals[self.local_count] = .{
            .name = token.lexeme,
            .depth = -1,
        };
        self.local_count += 1;
    }

    fn markInitialized(self: *Compiler) void {
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }

    fn resolveLocal(self: *Compiler, name: []const u8) ?u8 {
        var i: i32 = @intCast(self.local_count);
        while (i > 0) {
            i -= 1;
            const idx: usize = @intCast(i);
            if (std.mem.eql(u8, self.locals[idx].name, name)) {
                if (self.locals[idx].depth == -1) {
                    self.errorAtPrevious("Can't read local variable in its own initializer.");
                }
                return @intCast(idx);
            }
        }
        return null;
    }

    fn resolveUpvalue(self: *Compiler, name: []const u8) ?u8 {
        if (self.enclosing == null) return null;

        if (self.enclosing.?.resolveLocal(name)) |local_index| {
            self.enclosing.?.locals[local_index].is_captured = true;
            return self.addUpvalue(local_index, true);
        }

        if (self.enclosing.?.resolveUpvalue(name)) |upvalue_index| {
            return self.addUpvalue(upvalue_index, false);
        }

        return null;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) u8 {
        const upvalue_count = self.function.upvalue_count;

        for (0..upvalue_count) |i| {
            if (self.upvalues[i].index == index and self.upvalues[i].is_local == is_local) {
                return @intCast(i);
            }
        }

        self.upvalues[upvalue_count] = .{ .index = index, .is_local = is_local };
        self.function.upvalue_count += 1;
        return @intCast(upvalue_count);
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) usize {
        self.scope_depth -= 1;

        var count: usize = 0;
        while (self.local_count > 1 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            if (self.locals[self.local_count - 1].is_captured) {
                self.emitOpcode(.close_upvalue);
            } else {
                self.emitOpcode(.pop);
            }
            self.local_count -= 1;
            count += 1;
        }
        return count;
    }

    fn parseVariable(self: *Compiler, message: []const u8) u8 {
        const token = self.consumeIdentifier(message) catch return 0;

        if (self.scope_depth > 0) {
            self.addLocal(token);
            return 0;
        }

        return self.identifierConstant(token);
    }

    fn defineVariable(self: *Compiler, global: u8) void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        self.emitBytes(@intFromEnum(OpCode.define_global), global);
    }

    // === Helpers ===

    fn currentChunk(self: *Compiler) *Chunk {
        return &self.function.chunk;
    }

    fn emitJump(self: *Compiler, opcode: OpCode) i32 {
        self.emitOpcode(opcode);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return @intCast(self.currentChunk().code.items.len - 2);
    }

    fn patchJump(self: *Compiler, offset: i32) void {
        const jump: usize = @intCast(offset);
        const jump_amount = self.currentChunk().code.items.len - jump - 2;

        if (jump_amount > 65535) {
            self.errorAtPrevious("Too much code to jump over.");
        }

        self.currentChunk().writeAt(jump, @intCast((jump_amount >> 8) & 0xff));
        self.currentChunk().writeAt(jump + 1, @intCast(jump_amount & 0xff));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) void {
        self.emitOpcode(.loop);

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > 65535) {
            self.errorAtPrevious("Loop body too large.");
        }

        self.emitByte(@intCast((offset >> 8) & 0xff));
        self.emitByte(@intCast(offset & 0xff));
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        const index = self.currentChunk().addConstant(value) catch {
            self.errorAtCurrent("Too many constants in one chunk.");
            return;
        };
        self.emitBytes(@intFromEnum(OpCode.constant), index);
    }

    fn emitOpcode(self: *Compiler, opcode: OpCode) void {
        self.emitByte(@intFromEnum(opcode));
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch {
            std.debug.print("Chunk write error\n", .{});
        };
    }

    fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn identifierConstant(self: *Compiler, token: Token) u8 {
        return self.currentChunk().addConstant(.{ .string = token.lexeme }) catch {
            std.debug.print("Too many constants\n", .{});
            return 0;
        };
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

    fn consume(self: *Compiler, ttype: TokenType, message: []const u8) !void {
        if (self.current.type == ttype) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
        return error.CompileError;
    }

    fn consumeIdentifier(self: *Compiler, message: []const u8) !Token {
        if (self.current.type == .identifier) {
            self.advance();
            return self.previous;
        }

        self.errorAtCurrent(message);
        return error.CompileError;
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
