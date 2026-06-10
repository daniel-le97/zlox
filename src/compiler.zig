//! Compiler - converts tokens to register-based bytecode
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

pub const FunctionType = enum { function, method, initializer, script };

const Local = struct {
    name: []const u8,
    depth: i32,
    reg: u8, // register assigned to this local
    is_captured: bool = false,
};

const Upvalue = struct { index: u8, is_local: bool };

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    current: Token,
    previous: Token,
    had_error: bool = false,
    panic_mode: bool = false,
    function: *ObjFunction,
    func_type: FunctionType,

    // ── Register tracking ──
    next_reg: u8 = 1, // R0 is reserved (closure/this)
    max_reg: u8 = 1,

    // Scope tracking
    locals: [MAX_LOCALS]Local = undefined,
    local_count: usize = 0,
    scope_depth: i32 = 0,

    // Upvalue tracking
    upvalues: [MAX_UPVALUES]Upvalue = undefined,

    // Loop tracking
    innermost_loop_start: usize = 0,
    innermost_loop_scope_depth: i32 = -1,
    is_for_loop: bool = false,
    for_increment_start: usize = 0,
    break_jumps: [256]i32 = undefined,
    break_count: usize = 0,

    enclosing: ?*Compiler = null,

    // Track the last closure register for defineVariable
    closure_reg: u8 = 0,

    // When set, expression compilation targets this register for the result
    target_reg: ?u8 = null,

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
        // Reserve R0
        compiler.locals[0] = .{ .name = "", .depth = 0, .reg = 0, .is_captured = false };
        compiler.local_count = 1;
        if (first_token) |tok| compiler.current = tok else compiler.advance();
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.function.deinit();
        self.allocator.destroy(self.function);
    }

    pub fn compile(self: *Compiler) !bool {
        while (!self.check(.eof)) self.declaration();
        // Implicit return nil for the script
        const r = self.allocReg();
        self.emit2(@intFromEnum(OpCode.load_nil), r);
        self.emit2(@intFromEnum(OpCode.@"return"), r);
        self.function.num_registers = self.max_reg;
        return !self.had_error;
    }

    // ── Register helpers ──

    fn allocReg(self: *Compiler) u8 {
        const r = self.next_reg;
        self.next_reg += 1;
        if (self.next_reg > self.max_reg) self.max_reg = self.next_reg;
        return r;
    }

    fn localReg(self: *Compiler, index: usize) u8 {
        return self.locals[index].reg;
    }

    // ── Declarations ──

    fn declaration(self: *Compiler) void {
        if (self.match(&[_]TokenType{.import_kw})) return self.importDeclaration();
        if (self.match(&[_]TokenType{.module_kw})) return self.moduleDeclaration();
        if (self.match(&[_]TokenType{.class})) return self.classDeclaration();
        if (self.match(&[_]TokenType{.fun})) return self.funDeclaration();
        if (self.match(&[_]TokenType{.@"var"})) return self.varDeclaration();
        self.statement();
    }

    fn importDeclaration(self: *Compiler) void {
        _ = self.consumeString("Expect string path after import.") catch return;
        self.consume(.semicolon, "Expect ';' after import.") catch return;
    }

    fn moduleDeclaration(self: *Compiler) void {
        const name_token = self.consumeIdentifier("Expect module name.") catch return;
        self.consume(.semicolon, "Expect ';' after module declaration.") catch return;
        const r = self.allocReg();
        self.emit2(@intFromEnum(OpCode.load_nil), r);
        self.emit3(@intFromEnum(OpCode.define_global), self.identifierConstant(name_token), r);
    }

    fn classDeclaration(self: *Compiler) void {
        const name = self.consumeIdentifier("Expect class name.") catch return;
        const name_cx = self.identifierConstant(name);
        const r_class = self.allocReg();
        self.emit3(@intFromEnum(OpCode.class), r_class, name_cx);

        if (self.match(&[_]TokenType{.less})) {
            const super_name = self.consumeIdentifier("Expect superclass name.") catch return;
            const r_super = self.emitNamedVar(super_name);
            self.emit3(@intFromEnum(OpCode.inherit), r_class, r_super);
            self.consume(.left_brace, "Expect '{' before class body.") catch return;
        } else {
            self.consume(.left_brace, "Expect '{' before class body.") catch return;
        }

        while (!self.check(.right_brace) and !self.check(.eof)) {
            const method_name = self.consumeIdentifier("Expect method name.") catch return;
            const method_cx = self.identifierConstant(method_name);
            const is_init = std.mem.eql(u8, method_name.lexeme, "init");
            const ft: FunctionType = if (is_init) .initializer else .method;
            self.compileMethod(ft);
            const r_method = self.allocReg();
            self.emit4(@intFromEnum(OpCode.method), r_class, method_cx, r_method);
        }
        self.consume(.right_brace, "Expect '}' after class body.") catch return;

        self.emit3(@intFromEnum(OpCode.define_global), name_cx, r_class);
    }

    fn compileMethod(self: *Compiler, func_type: FunctionType) void {
        _ = self;
        _ = func_type;
    }

    fn funDeclaration(self: *Compiler) void {
        const name = self.consumeIdentifier("Expect function name.") catch return;
        if (self.scope_depth > 0) {
            // Local function declaration (nested function)
            _ = self.addLocal(name);
            self.markInitialized();
        }
        self.compileFunction(.function, self.previous.lexeme);
        if (self.scope_depth > 0) {
            self.locals[self.local_count - 1].reg = self.closure_reg;
        } else {
            self.emit3(@intFromEnum(OpCode.define_global), self.identifierConstant(name), self.closure_reg);
        }
    }

    fn compileFunction(self: *Compiler, func_type: FunctionType, name: []const u8) void {
        const func = self.allocator.create(ObjFunction) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        func.* = ObjFunction.init(self.allocator, name);

        var fc = Compiler.init(self.allocator, self.lexer, func_type, self, self.current) catch {
            self.errorAtCurrent("Out of memory.");
            return;
        };
        fc.function.deinit();
        fc.function = func;
        fc.function.name = name;

        // Parameters get registers 1..arity
        fc.next_reg = 1;
        fc.consume(.left_paren, "Expect '(' after function name.") catch {
            self.current = fc.current;
            return;
        };
        if (!fc.check(.right_paren)) {
            while (true) {
                fc.function.arity += 1;
                const param = fc.consumeIdentifier("Expect parameter name.") catch {
                    self.current = fc.current;
                    return;
                };
                const p_reg = fc.allocReg();
                _ = fc.addLocal(param);
                fc.locals[fc.local_count - 1].reg = p_reg;
                fc.markInitialized();
                if (!fc.match(&[_]TokenType{.comma})) break;
            }
        }
        fc.consume(.right_paren, "Expect ')' after parameters.") catch {
            self.current = fc.current;
            return;
        };
        fc.consume(.left_brace, "Expect '{' before function body.") catch {
            self.current = fc.current;
            return;
        };

        fc.beginScope();
        fc.block();
        _ = fc.endScope();
        self.current = fc.current;

        // Implicit return
        const r_ret = fc.allocReg();
        if (func_type == .initializer) {
            fc.emit2(@intFromEnum(OpCode.load_nil), r_ret);
            fc.emit2(@intFromEnum(OpCode.@"return"), 0); // return R0 (this)
        } else {
            fc.emit2(@intFromEnum(OpCode.load_nil), r_ret);
            fc.emit2(@intFromEnum(OpCode.@"return"), r_ret);
        }
        fc.function.num_registers = fc.max_reg;

        // Add as constant
        _ = self.function.chunk.addConstant(.{ .obj = &func.obj }) catch {
            fc.errorAtCurrent("Too many constants.");
            return;
        };

        // Emit closure opcode
        const const_idx = @as(u8, @intCast(self.function.chunk.constants.items.len - 1));
        const r_closure = self.allocReg();
        self.emit3(@intFromEnum(OpCode.closure), r_closure, const_idx);
        for (0..func.upvalue_count) |i| {
            self.emitByte(if (fc.upvalues[i].is_local) 1 else 0);
            self.emitByte(fc.upvalues[i].index);
        }

        // Store closure register for defineVariable
        self.closure_reg = r_closure;
    }

    fn varDeclaration(self: *Compiler) void {
        const name_cx = self.parseVariable("Expect variable name.");
        if (self.scope_depth > 0) {
            // Local variable: assign a register
            if (self.match(&[_]TokenType{.equal})) {
                const r = self.expression();
                self.locals[self.local_count - 1].reg = r;
            } else {
                const r = self.allocReg();
                self.emit2(@intFromEnum(OpCode.load_nil), r);
                self.locals[self.local_count - 1].reg = r;
            }
            self.markInitialized();
        } else {
            if (self.match(&[_]TokenType{.equal})) {
                const r = self.expression();
                self.emit3(@intFromEnum(OpCode.define_global), name_cx, r);
            } else {
                const r = self.allocReg();
                self.emit2(@intFromEnum(OpCode.load_nil), r);
                self.emit3(@intFromEnum(OpCode.define_global), name_cx, r);
            }
        }
        self.consume(.semicolon, "Expect ';' after variable declaration.") catch return;
    }

    // ── Statements ──

    fn statement(self: *Compiler) void {
        if (self.match(&[_]TokenType{.print})) return self.printStatement();
        if (self.match(&[_]TokenType{.if_kw})) return self.ifStatement();
        if (self.match(&[_]TokenType{.while_kw})) return self.whileStatement();
        if (self.match(&[_]TokenType{.for_kw})) return self.forStatement();
        if (self.match(&[_]TokenType{.@"return"})) return self.returnStatement();
        if (self.match(&[_]TokenType{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
            return;
        }
        if (self.match(&[_]TokenType{.break_kw})) return self.breakStatement();
        if (self.match(&[_]TokenType{.continue_kw})) return self.continueStatement();
        self.expressionStatement();
    }

    fn printStatement(self: *Compiler) void {
        const r = self.expression();
        self.consume(.semicolon, "Expect ';' after value.") catch return;
        self.emit2(@intFromEnum(OpCode.print), r);
    }

    fn expressionStatement(self: *Compiler) void {
        _ = self.expression();
        self.consume(.semicolon, "Expect ';' after expression.") catch return;
        // Result register is just discarded
    }

    fn ifStatement(self: *Compiler) void {
        self.consume(.left_paren, "Expect '(' after 'if'.") catch return;
        const r_cond = self.expression();
        self.consume(.right_paren, "Expect ')' after condition.") catch return;
        const then_jump = self.emitJump(.jump_if_false, r_cond);
        if (self.match(&.{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
        } else self.statement();
        const else_jump = self.emitJump(.jump, 0);
        self.patchJump(then_jump);
        if (self.match(&.{.else_kw})) {
            if (self.match(&.{.left_brace})) {
                self.beginScope();
                self.block();
                _ = self.endScope();
            } else self.statement();
        }
        self.patchJump(else_jump);
    }

    fn whileStatement(self: *Compiler) void {
        const loop_start = self.currentChunk().code.items.len;
        self.consume(.left_paren, "Expect '(' after 'while'.") catch return;
        const r_cond = self.expression();
        self.consume(.right_paren, "Expect ')' after condition.") catch return;
        const exit_jump = self.emitJump(.jump_if_false, r_cond);
        self.beginLoop(loop_start);
        self.is_for_loop = false;
        if (self.match(&.{.left_brace})) {
            self.beginScope();
            self.block();
            _ = self.endScope();
        } else self.statement();
        self.emitLoop(loop_start);
        self.patchBreaks();
        self.patchJump(exit_jump);
        self.innermost_loop_scope_depth = -1;
    }

    fn forStatement(self: *Compiler) void {
        self.beginScope();
        self.consume(.left_paren, "Expect '(' after 'for'.") catch return;
        if (self.match(&.{.semicolon})) {} else if (self.match(&[_]TokenType{.@"var"})) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loop_start = self.currentChunk().code.items.len;
        var exit_jump: i32 = -1;
        if (!self.match(&.{.semicolon})) {
            const r_cond = self.expression();
            self.consume(.semicolon, "Expect ';' after loop condition.") catch return;
            exit_jump = self.emitJump(.jump_if_false, r_cond);
        }
        if (!self.match(&.{.right_paren})) {
            const body_jump = self.emitJump(.jump, 0);
            const inc_start = self.currentChunk().code.items.len;
            _ = self.expression();
            self.consume(.right_paren, "Expect ')' after for clauses.") catch return;
            self.emitLoop(loop_start);
            loop_start = inc_start;
            self.patchJump(body_jump);
        }
        self.beginLoop(loop_start);
        self.is_for_loop = true;
        self.for_increment_start = loop_start;
        if (self.match(&.{.left_brace})) {
            self.block();
        } else {
            self.statement();
        }
        self.emitLoop(loop_start);
        if (exit_jump != -1) self.patchJump(exit_jump);
        self.patchBreaks();
        _ = self.endScope();
        self.innermost_loop_scope_depth = -1;
    }

    fn returnStatement(self: *Compiler) void {
        if (self.func_type == .script) {
            self.errorAtPrevious("Can't return from top-level code.");
            return;
        }
        if (self.match(&.{.semicolon})) {
            const r = self.allocReg();
            self.emit2(@intFromEnum(OpCode.load_nil), r);
            self.emit2(@intFromEnum(OpCode.@"return"), r);
        } else {
            if (self.func_type == .initializer) self.errorAtPrevious("Can't return a value from an initializer.");
            const r = self.expression();
            self.consume(.semicolon, "Expect ';' after return value.") catch return;
            self.emit2(@intFromEnum(OpCode.@"return"), r);
        }
    }

    fn breakStatement(self: *Compiler) void {
        if (self.innermost_loop_scope_depth == -1) {
            self.errorAtCurrent("'break' outside of loop.");
            return;
        }
        self.consume(.semicolon, "Expect ';' after 'break'.") catch return;
        _ = self.endLoopScope();
        const jump = self.emitJump(.jump, 0);
        if (self.break_count < self.break_jumps.len) {
            self.break_jumps[self.break_count] = jump;
            self.break_count += 1;
        }
    }

    fn continueStatement(self: *Compiler) void {
        if (self.innermost_loop_scope_depth == -1) {
            self.errorAtCurrent("'continue' outside of loop.");
            return;
        }
        self.consume(.semicolon, "Expect ';' after 'continue'.") catch return;
        _ = self.endLoopScope();
        if (self.is_for_loop) self.emitLoop(self.for_increment_start) else self.emitLoop(self.innermost_loop_start);
    }

    fn patchBreaks(self: *Compiler) void {
        for (0..self.break_count) |i| self.patchJump(self.break_jumps[i]);
        self.break_count = 0;
    }

    fn endLoopScope(self: *Compiler) usize {
        self.scope_depth = self.innermost_loop_scope_depth;
        var count: usize = 0;
        while (self.local_count > 1 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            if (self.locals[self.local_count - 1].is_captured) {
                const r = self.locals[self.local_count - 1].reg;
                self.emit2(@intFromEnum(OpCode.close_upvalue), r);
            }
            self.local_count -= 1;
            count += 1;
        }
        return count;
    }

    fn beginLoop(self: *Compiler, loop_start: usize) void {
        self.innermost_loop_start = loop_start;
        self.innermost_loop_scope_depth = self.scope_depth;
        self.break_count = 0;
    }

    fn block(self: *Compiler) void {
        while (!self.check(.right_brace) and !self.check(.eof)) self.declaration();
        self.consume(.right_brace, "Expect '}' after block.") catch return;
    }

    // ── Expressions (ALL return register index) ──

    fn expression(self: *Compiler) u8 {
        return self.assignment();
    }

    fn assignment(self: *Compiler) u8 {
        if (self.current.type == .identifier) {
            const next = self.lexer.peekToken();
            if (next.type == .equal) {
                const name = self.current;
                self.advance();
                self.advance(); // =
                const r_val = self.expression();

                if (self.resolveLocal(name.lexeme)) |idx| {
                    const r_dst = self.localReg(idx);
                    if (r_dst != r_val) {
                        self.emit3(@intFromEnum(OpCode.move), r_dst, r_val);
                    }
                    return r_dst;
                } else if (self.resolveUpvalue(name.lexeme)) |slot| {
                    self.emit3(@intFromEnum(OpCode.set_upvalue), slot, r_val);
                    return r_val;
                } else {
                    self.emit3(@intFromEnum(OpCode.set_global), self.identifierConstant(name), r_val);
                    return r_val;
                }
            }
        }
        return self.orExpr();
    }

    fn orExpr(self: *Compiler) u8 {
        var r = self.andExpr();
        while (self.match(&[_]TokenType{.or_kw})) {
            const else_jump = self.emitJump(.jump_if_false, r);
            const end_jump = self.emitJump(.jump, 0);
            self.patchJump(else_jump);
            r = self.andExpr();
            self.patchJump(end_jump);
        }
        return r;
    }

    fn andExpr(self: *Compiler) u8 {
        var r = self.equality();
        while (self.match(&[_]TokenType{.and_kw})) {
            const end_jump = self.emitJump(.jump_if_false, r);
            r = self.equality();
            self.patchJump(end_jump);
        }
        return r;
    }

    fn equality(self: *Compiler) u8 {
        var r = self.comparison();
        while (self.match(&[_]TokenType{ .bang_equal, .equal_equal })) {
            const op = self.previous.type;
            const saved = self.target_reg;
            self.target_reg = null;
            const r_b = self.comparison();
            const r_dst = saved orelse self.allocReg();
            self.emit4(@intFromEnum(OpCode.equal), r_dst, r, r_b);
            if (op == .bang_equal) {
                self.emit3(@intFromEnum(OpCode.not_register), r_dst, r_dst);
            }
            r = r_dst;
        }
        return r;
    }

    fn comparison(self: *Compiler) u8 {
        var r = self.term();
        while (self.match(&[_]TokenType{ .greater, .greater_equal, .less, .less_equal })) {
            const op = self.previous.type;
            const saved = self.target_reg;
            self.target_reg = null;
            const r_b = self.term();
            const r_dst = saved orelse self.allocReg();
            switch (op) {
                .greater => self.emit4(@intFromEnum(OpCode.greater_number), r_dst, r, r_b),
                .greater_equal => {
                    self.emit4(@intFromEnum(OpCode.less_number), r_dst, r, r_b);
                    self.emit3(@intFromEnum(OpCode.not_register), r_dst, r_dst);
                },
                .less => self.emit4(@intFromEnum(OpCode.less_number), r_dst, r, r_b),
                .less_equal => {
                    self.emit4(@intFromEnum(OpCode.greater_number), r_dst, r, r_b);
                    self.emit3(@intFromEnum(OpCode.not_register), r_dst, r_dst);
                },
                else => {},
            }
            r = r_dst;
        }
        return r;
    }

    fn term(self: *Compiler) u8 {
        var r = self.factor();
        while (self.match(&[_]TokenType{ .minus, .plus })) {
            const op = self.previous.type;
            const saved = self.target_reg;
            self.target_reg = null;
            // Constant folding: detect `reg - const` → sub_const
            if (op == .minus and self.check(.number)) {
                const val = std.fmt.parseFloat(f64, self.current.lexeme) catch {
                    self.errorAtCurrent("Invalid number.");
                    return 0;
                };
                self.advance();
                const ci = self.function.chunk.addConstant(.{ .number = val }) catch {
                    self.errorAtPrevious("Too many constants.");
                    return 0;
                };
                const r_dst = saved orelse self.allocReg();
                self.emit4(@intFromEnum(OpCode.sub_const), r_dst, r, ci);
                r = r_dst;
            } else {
                const r_b = self.factor();
                const r_dst = saved orelse self.allocReg();
                switch (op) {
                    .plus => self.emit4(@intFromEnum(OpCode.add_number), r_dst, r, r_b),
                    .minus => self.emit4(@intFromEnum(OpCode.sub_number), r_dst, r, r_b),
                    else => {},
                }
                r = r_dst;
            }
        }
        return r;
    }

    fn factor(self: *Compiler) u8 {
        var r = self.unary();
        while (self.match(&[_]TokenType{ .slash, .star })) {
            const op = self.previous.type;
            const saved = self.target_reg;
            self.target_reg = null;
            const r_b = self.unary();
            const r_dst = saved orelse self.allocReg();
            switch (op) {
                .star => self.emit4(@intFromEnum(OpCode.mul_number), r_dst, r, r_b),
                .slash => self.emit4(@intFromEnum(OpCode.div_number), r_dst, r, r_b),
                else => {},
            }
            r = r_dst;
        }
        return r;
    }

    fn unary(self: *Compiler) u8 {
        if (self.match(&[_]TokenType{.bang})) {
            const r_src = self.unary();
            const r_dst = self.target_reg orelse self.allocReg();
            self.emit3(@intFromEnum(OpCode.not_register), r_dst, r_src);
            return r_dst;
        }
        if (self.match(&[_]TokenType{.minus})) {
            const r_src = self.unary();
            const r_dst = self.target_reg orelse self.allocReg();
            self.emit3(@intFromEnum(OpCode.negate_number), r_dst, r_src);
            return r_dst;
        }
        return self.call();
    }

    fn call(self: *Compiler) u8 {
        // Self-recursive call optimization
        if (self.check(.identifier) and
            self.function.name.len > 0 and
            std.mem.eql(u8, self.current.lexeme, self.function.name) and
            self.lexer.peekToken().type == .left_paren)
        {
            self.advance(); // consume function name
            _ = self.match(&[_]TokenType{.left_paren}); // consume '('
            const r_dst = self.allocReg();
            var arg_count: u8 = 0;
            if (!self.check(.right_paren)) {
                while (true) {
                    const arg_reg = r_dst + 1 + arg_count;
                    self.target_reg = arg_reg;
                    const r_expr = self.expression();
                    self.target_reg = null;
                    if (r_expr != arg_reg) {
                        self.emit3(@intFromEnum(OpCode.move), arg_reg, r_expr);
                    }
                    arg_count += 1;
                    if (arg_count > MAX_PARAMS) self.errorAtCurrent("Can't have more than 255 arguments.");
                    if (!self.match(&[_]TokenType{.comma})) break;
                }
            }
            self.consume(.right_paren, "Expect ')' after arguments.") catch return r_dst;
            self.emit3(@intFromEnum(OpCode.call_self), r_dst, arg_count);
            return r_dst;
        }

        var r_callee = self.primary();

        while (true) {
            if (self.match(&[_]TokenType{.left_paren})) {
                var arg_count: u8 = 0;
                if (!self.check(.right_paren)) {
                    while (true) {
                        const arg_reg = r_callee + 1 + arg_count;
                        self.target_reg = arg_reg;
                        const r_expr = self.expression();
                        self.target_reg = null;
                        if (r_expr != arg_reg) {
                            self.emit3(@intFromEnum(OpCode.move), arg_reg, r_expr);
                        }
                        arg_count += 1;
                        if (arg_count > MAX_PARAMS) self.errorAtCurrent("Can't have more than 255 arguments.");
                        if (!self.match(&[_]TokenType{.comma})) break;
                    }
                }
                self.consume(.right_paren, "Expect ')' after arguments.") catch return r_callee;
                const r_dst = self.allocReg();
                self.emit4(@intFromEnum(OpCode.call), r_dst, r_callee, arg_count);
                r_callee = r_dst;
            } else if (self.match(&[_]TokenType{.dot})) {
                const name = self.consumeIdentifier("Expect property name after '.'.") catch return r_callee;
                const name_cx = self.identifierConstant(name);
                if (self.match(&[_]TokenType{.equal})) {
                    const r_val = self.expression();
                    self.emit4(@intFromEnum(OpCode.set_property), r_callee, name_cx, r_val);
                    r_callee = r_val;
                    return r_callee;
                } else if (self.match(&[_]TokenType{.left_paren})) {
                    self.emit4(@intFromEnum(OpCode.get_property), r_callee, r_callee, name_cx);
                    var arg_count: u8 = 0;
                    if (!self.check(.right_paren)) {
                        while (true) {
                            const arg_reg = r_callee + 1 + arg_count;
                            const r_expr = self.expression();
                            if (r_expr != arg_reg) self.emit3(@intFromEnum(OpCode.move), arg_reg, r_expr);
                            arg_count += 1;
                            if (!self.match(&[_]TokenType{.comma})) break;
                        }
                    }
                    self.consume(.right_paren, "Expect ')' after arguments.") catch return r_callee;
                    const r_dst = self.allocReg();
                    self.emit4(@intFromEnum(OpCode.call), r_dst, r_callee, arg_count);
                    r_callee = r_dst;
                } else {
                    const r_dst = self.allocReg();
                    self.emit4(@intFromEnum(OpCode.get_property), r_dst, r_callee, name_cx);
                    r_callee = r_dst;
                }
            } else {
                break;
            }
        }
        return r_callee;
    }

    fn primary(self: *Compiler) u8 {
        if (self.match(&[_]TokenType{.nil})) {
            const r = self.target_reg orelse self.allocReg();
            self.emit2(@intFromEnum(OpCode.load_nil), r);
            return r;
        }
        if (self.match(&[_]TokenType{.true_kw})) {
            const r = self.target_reg orelse self.allocReg();
            self.emit2(@intFromEnum(OpCode.load_true), r);
            return r;
        }
        if (self.match(&[_]TokenType{.false_kw})) {
            const r = self.target_reg orelse self.allocReg();
            self.emit2(@intFromEnum(OpCode.load_false), r);
            return r;
        }
        if (self.match(&[_]TokenType{.number})) {
            const r = self.target_reg orelse self.allocReg();
            const val = std.fmt.parseFloat(f64, self.previous.lexeme) catch {
                self.errorAtPrevious("Invalid number.");
                return 0;
            };
            const ci = self.function.chunk.addConstant(.{ .number = val }) catch {
                self.errorAtPrevious("Too many constants.");
                return 0;
            };
            self.emit3(@intFromEnum(OpCode.load_const), r, ci);
            return r;
        }
        if (self.match(&[_]TokenType{.string})) {
            const str = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
            const r = self.target_reg orelse self.allocReg();
            const ci = self.function.chunk.addConstant(.{ .string = str }) catch {
                self.errorAtPrevious("Too many constants.");
                return 0;
            };
            self.emit3(@intFromEnum(OpCode.load_const), r, ci);
            return r;
        }
        if (self.match(&[_]TokenType{.this})) {
            return 0; // R0 = this
        }
        if (self.match(&[_]TokenType{.identifier})) {
            return self.emitNamedVar(self.previous);
        }
        if (self.match(&[_]TokenType{.left_paren})) {
            const r = self.expression();
            self.consume(.right_paren, "Expect ')' after expression.") catch return r;
            return r;
        }
        if (self.match(&[_]TokenType{.super})) {
            self.consume(.dot, "Expect '.' after 'super'.") catch return 0;
            const super_method = self.consumeIdentifier("Expect superclass method name.") catch return 0;
            const method_cx = self.identifierConstant(super_method);
            if (self.match(&[_]TokenType{.left_paren})) {
                var arg_count: u8 = 0;
                if (!self.check(.right_paren)) {
                    while (true) {
                        const arg_reg = 1 + arg_count;
                        const r_expr = self.expression();
                        if (r_expr != arg_reg) self.emit3(@intFromEnum(OpCode.move), arg_reg, r_expr);
                        arg_count += 1;
                        if (!self.match(&[_]TokenType{.comma})) break;
                    }
                }
                self.consume(.right_paren, "Expect ')' after arguments.") catch return 0;
                const r_dst = self.allocReg();
                self.emit5(@intFromEnum(OpCode.super_invoke), r_dst, 0, method_cx, arg_count);
                return r_dst;
            } else {
                const r_dst = self.allocReg();
                self.emit3(@intFromEnum(OpCode.get_super), r_dst, method_cx);
                return r_dst;
            }
        }
        self.errorAtCurrent("Expect expression.");
        return 0;
    }

    fn emitNamedVar(self: *Compiler, token: Token) u8 {
        if (self.resolveLocal(token.lexeme)) |idx| {
            return self.localReg(idx);
        }
        if (self.resolveUpvalue(token.lexeme)) |slot| {
            const r = self.allocReg();
            self.emit3(@intFromEnum(OpCode.get_upvalue), r, slot);
            return r;
        }
        const r = self.allocReg();
        self.emit3(@intFromEnum(OpCode.get_global), r, self.identifierConstant(token));
        return r;
    }

    fn namedVariable(self: *Compiler, token: Token, can_assign: bool) void {
        _ = can_assign;
        _ = self.emitNamedVar(token);
    }

    // ── Locals ──

    fn addLocal(self: *Compiler, token: Token) usize {
        if (self.local_count >= MAX_LOCALS) {
            self.errorAtPrevious("Too many local variables.");
            return 0;
        }
        self.locals[self.local_count] = .{ .name = token.lexeme, .depth = -1, .reg = 0 };
        self.local_count += 1;
        return self.local_count - 1;
    }

    fn markInitialized(self: *Compiler) void {
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }

    fn resolveLocal(self: *Compiler, name: []const u8) ?usize {
        var i: i32 = @intCast(self.local_count);
        while (i > 0) {
            i -= 1;
            const idx: usize = @intCast(i);
            if (std.mem.eql(u8, self.locals[idx].name, name)) {
                if (self.locals[idx].depth == -1) self.errorAtPrevious("Can't read local variable in its own initializer.");
                return idx;
            }
        }
        return null;
    }

    fn resolveUpvalue(self: *Compiler, name: []const u8) ?u8 {
        if (self.enclosing == null) return null;
        if (self.enclosing.?.resolveLocal(name)) |idx| {
            self.enclosing.?.locals[idx].is_captured = true;
            return self.addUpvalue(@intCast(idx), true);
        }
        if (self.enclosing.?.resolveUpvalue(name)) |idx| {
            return self.addUpvalue(idx, false);
        }
        return null;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) u8 {
        for (self.upvalues[0..self.function.upvalue_count], 0..) |uv, i| {
            if (uv.index == index and uv.is_local == is_local) return @intCast(i);
        }
        if (self.function.upvalue_count >= MAX_UPVALUES) {
            self.errorAtPrevious("Too many upvalues.");
            return 0;
        }
        self.upvalues[self.function.upvalue_count] = .{ .index = index, .is_local = is_local };
        self.function.upvalue_count += 1;
        return self.function.upvalue_count - 1;
    }

    // ── Parsing helpers ──

    fn parseVariable(self: *Compiler, error_msg: []const u8) u8 {
        const token = self.consumeIdentifier(error_msg) catch return 0;
        if (self.scope_depth > 0) {
            _ = self.addLocal(token);
            return 0;
        }
        return self.identifierConstant(token);
    }

    fn defineVariable(self: *Compiler, global: u8) void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emit3(@intFromEnum(OpCode.define_global), global, self.closure_reg);
    }

    fn identifierConstant(self: *Compiler, token: Token) u8 {
        return self.function.chunk.addConstant(.{ .string = token.lexeme }) catch {
            self.errorAtPrevious("Too many constants.");
            return 0;
        };
    }

    // ── Emit helpers ──

    fn emitOpcode(self: *Compiler, op: OpCode) void {
        self.currentChunk().write(@intFromEnum(op), self.previous.line) catch {};
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch {};
    }

    fn emit2(self: *Compiler, a: u8, b: u8) void {
        self.currentChunk().write(a, self.previous.line) catch {};
        self.currentChunk().write(b, self.previous.line) catch {};
    }

    fn emit3(self: *Compiler, a: u8, b: u8, c: u8) void {
        self.currentChunk().write(a, self.previous.line) catch {};
        self.currentChunk().write(b, self.previous.line) catch {};
        self.currentChunk().write(c, self.previous.line) catch {};
    }

    fn emit4(self: *Compiler, a: u8, b: u8, c: u8, d: u8) void {
        self.currentChunk().write(a, self.previous.line) catch {};
        self.currentChunk().write(b, self.previous.line) catch {};
        self.currentChunk().write(c, self.previous.line) catch {};
        self.currentChunk().write(d, self.previous.line) catch {};
    }

    fn emit5(self: *Compiler, a: u8, b: u8, c: u8, d: u8, e: u8) void {
        self.currentChunk().write(a, self.previous.line) catch {};
        self.currentChunk().write(b, self.previous.line) catch {};
        self.currentChunk().write(c, self.previous.line) catch {};
        self.currentChunk().write(d, self.previous.line) catch {};
        self.currentChunk().write(e, self.previous.line) catch {};
    }

    /// Returns the byte index of the 2-byte jump offset field.
    fn emitJump(self: *Compiler, op: OpCode, reg: u8) i32 {
        if (op == .jump_if_false) {
            self.emitByte(@intFromEnum(op));
            self.emitByte(reg);
            const off = self.currentChunk().code.items.len;
            self.emitByte(0);
            self.emitByte(0);
            return @intCast(off);
        } else { // .jump
            self.emitByte(@intFromEnum(op));
            const off = self.currentChunk().code.items.len;
            self.emitByte(0);
            self.emitByte(0);
            return @intCast(off);
        }
    }

    fn patchJump(self: *Compiler, offset: i32) void {
        const pos = @as(usize, @intCast(offset));
        const jump = self.currentChunk().code.items.len - pos - 2;
        self.currentChunk().code.items[pos] = @truncate(@as(u16, @intCast(jump)) >> 8);
        self.currentChunk().code.items[pos + 1] = @truncate(@as(u16, @intCast(jump)));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) void {
        const offset = self.currentChunk().code.items.len - loop_start + 3;
        self.emitByte(@intFromEnum(OpCode.loop));
        self.emitByte(@truncate(@as(u16, @intCast(offset)) >> 8));
        self.emitByte(@truncate(@as(u16, @intCast(offset))));
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        const ci = self.function.chunk.addConstant(value) catch {
            self.errorAtPrevious("Too many constants.");
            return;
        };
        const r = self.allocReg();
        self.emitByte(@intFromEnum(OpCode.load_const));
        self.emitByte(r);
        self.emitByte(ci);
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return &self.function.chunk;
    }

    // ── Lexer helpers (unchanged) ──

    fn advance(self: *Compiler) void {
        self.previous = self.current;
        while (true) {
            self.current = self.lexer.nextToken();
            if (self.current.type != .@"error") break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn check(self: *Compiler, expected: TokenType) bool {
        return self.current.type == expected;
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

    fn consume(self: *Compiler, expected: TokenType, msg: []const u8) !void {
        if (self.current.type == expected) {
            self.advance();
            return;
        }
        self.errorAtCurrent(msg);
        return error.ParseError;
    }

    fn consumeIdentifier(self: *Compiler, msg: []const u8) !Token {
        if (self.current.type == .identifier) {
            const t = self.current;
            self.advance();
            return t;
        }
        self.errorAtCurrent(msg);
        return error.ParseError;
    }

    fn consumeString(self: *Compiler, msg: []const u8) !Token {
        if (self.current.type == .string) {
            const t = self.current;
            self.advance();
            return t;
        }
        self.errorAtCurrent(msg);
        return error.ParseError;
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        std.debug.print("[line {}] Error at '{}': {s}\n", .{ self.current.line, self.current.type, message });
        self.had_error = true;
    }

    fn errorAtPrevious(self: *Compiler, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        std.debug.print("[line {}] Error at '{}': {s}\n", .{ self.previous.line, self.previous.type, message });
        self.had_error = true;
    }

    fn synchronize(self: *Compiler) void {
        self.panic_mode = false;
        while (self.current.type != .eof) {
            if (self.previous.type == .semicolon) return;
            switch (self.current.type) {
                .class, .fun, .@"var", .for_kw, .if_kw, .while_kw, .print, .@"return" => return,
                else => self.advance(),
            }
        }
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) usize {
        self.scope_depth -= 1;
        var count: usize = 0;
        while (self.local_count > 1 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            if (self.locals[self.local_count - 1].is_captured) {
                self.emit2(@intFromEnum(OpCode.close_upvalue), self.locals[self.local_count - 1].reg);
            }
            self.local_count -= 1;
            count += 1;
        }
        return count;
    }
};
