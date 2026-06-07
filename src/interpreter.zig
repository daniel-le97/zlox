const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const Value = @import("value.zig").Value;
const Environment = @import("value.zig").Environment;
const ClassDefinition = @import("value.zig").ClassDefinition;
const Instance = @import("value.zig").Instance;
const FunctionTemplate = @import("value.zig").FunctionTemplate;
const FunctionInstance = @import("value.zig").FunctionInstance;
const MAX_PARAMS = @import("value.zig").MAX_PARAMS;

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    source_path: []const u8,
    source: []const u8,
    lexer: Lexer,
    current: Token,
    previous: Token,
    globals: *Environment,
    environment: *Environment,
    return_value: ?Value = null,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(source: []const u8, source_path: []const u8, io: std.Io, allocator: std.mem.Allocator) !Interpreter {
        const globals = try allocator.create(Environment);
        globals.* = Environment.init(allocator, null);

        var interpreter = Interpreter{
            .allocator = allocator,
            .io = io,
            .source_path = source_path,
            .source = source,
            .lexer = Lexer.init(source, allocator),
            .current = undefined,
            .previous = undefined,
            .globals = globals,
            .environment = globals,
        };
        interpreter.advance();
        return interpreter;
    }

    pub fn deinit(self: *Interpreter) void {
        self.globals.deinit();
        self.allocator.destroy(self.globals);
    }

    pub fn run(self: *Interpreter) !void {
        while (!self.check(.eof)) {
            try self.declaration();
            if (self.return_value != null) break;
        }
    }

    fn declaration(self: *Interpreter) anyerror!void {
        if (self.match(.import_kw)) {
            try self.importDeclaration();
            return;
        }

        if (self.match(.module_kw)) {
            try self.moduleDeclaration();
            return;
        }

        if (self.match(.class)) {
            try self.classDeclaration();
            return;
        }

        if (self.match(.@"var")) {
            try self.varDeclaration();
            return;
        }

        if (self.match(.fun)) {
            try self.functionDeclaration();
            return;
        }

        try self.statement();
    }

    fn statement(self: *Interpreter) anyerror!void {
        if (self.match(.if_kw)) {
            try self.ifStatement();
            return;
        }

        if (self.match(.while_kw)) {
            try self.whileStatement();
            return;
        }

        if (self.match(.for_kw)) {
            try self.forStatement();
            return;
        }

        if (self.match(.print)) {
            try self.printStatement();
            return;
        }

        if (self.match(.@"return")) {
            try self.returnStatement();
            return;
        }

        if (self.match(.left_brace)) {
            try self.block();
            return;
        }

        if (self.match(.break_kw)) {
            try self.breakStatement();
            return;
        }

        if (self.match(.continue_kw)) {
            try self.continueStatement();
            return;
        }

        if (self.canAssignProperty() and self.isPropertyAssignmentAhead()) {
            try self.assignPropertyStatement();
            return;
        }

        if (self.current.type == .identifier and self.isAssignmentAhead()) {
            const name_token = self.current;
            self.advance();
            if (self.match(.equal)) {
                const value = try self.expression();
                try self.consume(.semicolon, "Expect ';' after assignment.");
                try self.assignName(name_token.lexeme, value);
                return;
            }
        }

        try self.expressionStatement();
    }

    fn block(self: *Interpreter) anyerror!void {
        const parent = self.environment;
        const block_env = try self.allocator.create(Environment);
        block_env.* = Environment.init(self.allocator, parent);
        self.environment = block_env;
        defer {
            self.environment.deinit();
            self.allocator.destroy(self.environment);
            self.environment = parent;
        }

        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
            if (self.return_value != null) break;
        }

        try self.consume(.right_brace, "Expect '}' after block.");
    }

    fn printStatement(self: *Interpreter) !void {
        const value = try self.expression();
        try self.consume(.semicolon, "Expect ';' after value.");
        self.printValue(value);
    }

    fn ifStatement(self: *Interpreter) !void {
        try self.consume(.left_paren, "Expect '(' after 'if'.");
        const condition_source = try self.captureDelimitedSource(.right_paren);
        try self.consume(.right_paren, "Expect ')' after if condition.");

        const condition = try self.evaluateExpressionSource(condition_source, self.environment);

        if (self.match(.left_brace)) {
            const then_source = try self.captureBlockSource();
            if (condition.isTruthy()) {
                if (try self.runSnippetSource(then_source, self.environment)) |value| {
                    self.return_value = value;
                }
            }
        } else {
            if (condition.isTruthy()) {
                try self.statement();
            } else {
                _ = try self.captureDelimitedSource(.semicolon);
                try self.consume(.semicolon, "Expect ';' after if branch.");
            }
        }

        if (self.match(.else_kw)) {
            if (self.match(.left_brace)) {
                const else_source = try self.captureBlockSource();
                if (!condition.isTruthy()) {
                    if (try self.runSnippetSource(else_source, self.environment)) |value| {
                        self.return_value = value;
                    }
                }
            } else {
                if (!condition.isTruthy()) {
                    try self.statement();
                } else {
                    _ = try self.captureDelimitedSource(.semicolon);
                    try self.consume(.semicolon, "Expect ';' after else branch.");
                }
            }
        }
    }

    fn whileStatement(self: *Interpreter) !void {
        try self.consume(.left_paren, "Expect '(' after 'while'.");
        const condition_source = try self.captureDelimitedSource(.right_paren);
        try self.consume(.right_paren, "Expect ')' after while condition.");

        try self.consume(.left_brace, "Expect '{' after while condition.");
        const body_source = try self.captureBlockSource();

        while (true) {
            const condition = try self.evaluateExpressionSource(condition_source, self.environment);
            if (!condition.isTruthy()) break;

            const result = self.runSnippetSource(body_source, self.environment) catch |err| switch (err) {
                error.BreakLoop => break,
                error.ContinueLoop => continue,
                else => return err,
            };

            if (result) |value| {
                self.return_value = value;
                return;
            }
        }
    }

    fn forStatement(self: *Interpreter) !void {
        try self.consume(.left_paren, "Expect '(' after 'for'.");

        if (self.match(.semicolon)) {
            // No initializer.
        } else if (self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        const condition_source = if (self.check(.semicolon)) null else try self.captureDelimitedSource(.semicolon);
        try self.consume(.semicolon, "Expect ';' after loop condition.");

        const increment_source = if (self.check(.right_paren)) null else try self.captureDelimitedSource(.right_paren);
        try self.consume(.right_paren, "Expect ')' after for clauses.");

        try self.consume(.left_brace, "Expect '{' after for clauses.");
        const body_source = try self.captureBlockSource();

        while (true) {
            if (condition_source) |source| {
                const condition = try self.evaluateExpressionSource(source, self.environment);
                if (!condition.isTruthy()) break;
            }

            const result = self.runSnippetSource(body_source, self.environment) catch |err| switch (err) {
                error.BreakLoop => break,
                error.ContinueLoop => {
                    if (increment_source) |source| {
                        _ = try self.evaluateExpressionSource(source, self.environment);
                    }
                    continue;
                },
                else => return err,
            };

            if (result) |value| {
                self.return_value = value;
                return;
            }

            if (increment_source) |source| {
                _ = try self.evaluateExpressionSource(source, self.environment);
            }
        }
    }

    fn breakStatement(self: *Interpreter) !void {
        try self.consume(.semicolon, "Expect ';' after 'break'.");
        return error.BreakLoop;
    }

    fn continueStatement(self: *Interpreter) !void {
        try self.consume(.semicolon, "Expect ';' after 'continue'.");
        return error.ContinueLoop;
    }

    fn returnStatement(self: *Interpreter) !void {
        const value = if (!self.check(.semicolon)) try self.expression() else .nil;
        try self.consume(.semicolon, "Expect ';' after return value.");
        self.return_value = value;
    }

    fn varDeclaration(self: *Interpreter) !void {
        const name_token = try self.consumeIdentifier("Expect variable name.");
        const value = if (self.match(.equal)) try self.expression() else .nil;
        try self.consume(.semicolon, "Expect ';' after variable declaration.");
        try self.environment.define(name_token.lexeme, value);
    }

    fn importDeclaration(self: *Interpreter) !void {
        const path_token = try self.consumeString("Expect string path after import.");
        const imported_path = path_token.lexeme[1 .. path_token.lexeme.len - 1];
        try self.consume(.semicolon, "Expect ';' after import.");

        const resolved_path = try self.resolveImportPath(imported_path);
        defer self.allocator.free(resolved_path);

        const module_name = self.moduleNameFromPath(imported_path);
        const module_environment = try self.loadModule(resolved_path);
        try self.environment.define(module_name, .{ .module = module_environment });
    }

    fn moduleDeclaration(self: *Interpreter) !void {
        _ = try self.consumeIdentifier("Expect module name.");
        try self.consume(.semicolon, "Expect ';' after module declaration.");
    }

    fn classDeclaration(self: *Interpreter) !void {
        const name_token = try self.consumeIdentifier("Expect class name.");
        var superclass: ?*ClassDefinition = null;

        if (self.match(.less)) {
            const superclass_token = try self.consumeIdentifier("Expect superclass name.");
            const superclass_value = try self.lookupName(superclass_token.lexeme);
            superclass = switch (superclass_value) {
                .class_def => |class_def| class_def,
                else => return error.NotCallable,
            };
        }

        try self.consume(.left_brace, "Expect '{' before class body.");

        const class_def = try self.allocator.create(ClassDefinition);
        class_def.* = ClassDefinition.init(self.allocator, name_token.lexeme, superclass);

        while (!self.check(.right_brace) and !self.check(.eof)) {
            const method_name = try self.consumeIdentifier("Expect method name.");
            try self.consume(.left_paren, "Expect '(' after method name.");

            var param_names: [MAX_PARAMS][]const u8 = undefined;
            var param_count: u8 = 0;
            if (!self.check(.right_paren)) {
                while (true) {
                    if (param_count >= MAX_PARAMS) return error.TooManyParameters;
                    const param = try self.consumeIdentifier("Expect parameter name.");
                    param_names[param_count] = param.lexeme;
                    param_count += 1;
                    if (!self.match(.comma)) break;
                }
            }

            try self.consume(.right_paren, "Expect ')' after parameters.");
            try self.consume(.left_brace, "Expect '{' before method body.");
            const body_source = try self.captureBlockSource();

            const template = try self.allocator.create(FunctionTemplate);
            template.* = .{
                .name = method_name.lexeme,
                .arity = param_count,
                .param_names = param_names,
                .param_count = param_count,
                .body_source = body_source,
            };

            const method = try self.allocator.create(FunctionInstance);
            method.* = .{ .template = template, .closure = self.environment };
            try class_def.defineMethod(method_name.lexeme, method);
        }

        try self.consume(.right_brace, "Expect '}' after class body.");
        try self.environment.define(name_token.lexeme, .{ .class_def = class_def });
    }

    fn functionDeclaration(self: *Interpreter) !void {
        const name_token = try self.consumeIdentifier("Expect function name.");
        try self.consume(.left_paren, "Expect '(' after function name.");

        var param_names: [MAX_PARAMS][]const u8 = undefined;
        var param_count: u8 = 0;
        if (!self.check(.right_paren)) {
            while (true) {
                if (param_count >= MAX_PARAMS) return error.TooManyParameters;
                const param = try self.consumeIdentifier("Expect parameter name.");
                param_names[param_count] = param.lexeme;
                param_count += 1;
                if (!self.match(.comma)) break;
            }
        }

        try self.consume(.right_paren, "Expect ')' after parameters.");
        try self.consume(.left_brace, "Expect '{' before function body.");
        const body_source = try self.captureBlockSource();

        const template = try self.allocator.create(FunctionTemplate);
        template.* = .{
            .name = name_token.lexeme,
            .arity = param_count,
            .param_names = param_names,
            .param_count = param_count,
            .body_source = body_source,
        };

        const instance = try self.allocator.create(FunctionInstance);
        instance.* = .{ .template = template, .closure = self.environment };
        try self.environment.define(name_token.lexeme, .{ .closure = instance });
    }

    fn expressionStatement(self: *Interpreter) !void {
        _ = try self.expression();
        try self.consume(.semicolon, "Expect ';' after expression.");
    }

    fn expression(self: *Interpreter) anyerror!Value {
        return self.assignment();
    }

    fn assignment(self: *Interpreter) anyerror!Value {
        if (self.canAssignProperty() and self.isPropertyAssignmentAhead()) {
            const target = try self.consumePropertyTarget();
            try self.consume(.equal, "Expect '=' in assignment.");
            const value = try self.assignment();
            try self.assignProperty(target.object, target.property, value);
            return value;
        }

        if (self.current.type == .identifier and self.isAssignmentAhead()) {
            const name_token = self.current;
            self.advance();
            try self.consume(.equal, "Expect '=' in assignment.");
            const value = try self.assignment();
            try self.assignName(name_token.lexeme, value);
            return value;
        }

        return self.orExpr();
    }

    fn orExpr(self: *Interpreter) anyerror!Value {
        var value = try self.andExpr();
        while (self.match(.or_kw)) {
            if (value.isTruthy()) {
                _ = try self.andExpr();
            } else {
                value = try self.andExpr();
            }
        }
        return value;
    }

    fn andExpr(self: *Interpreter) anyerror!Value {
        var value = try self.equality();
        while (self.match(.and_kw)) {
            if (!value.isTruthy()) {
                _ = try self.equality();
            } else {
                value = try self.equality();
            }
        }
        return value;
    }

    fn equality(self: *Interpreter) anyerror!Value {
        var value = try self.comparison();
        while (self.matchAny(&.{ .bang_equal, .equal_equal })) {
            const op = self.previous.type;
            const right = try self.comparison();
            const equal = switch (value) {
                .number => |left| switch (right) {
                    .number => |right_number| left == right_number,
                    else => false,
                },
                .boolean => |left| switch (right) {
                    .boolean => |right_boolean| left == right_boolean,
                    else => false,
                },
                .string => |left| switch (right) {
                    .string => |right_string| std.mem.eql(u8, left, right_string),
                    else => false,
                },
                .function_template => |left| switch (right) {
                    .function_template => |right_function| left == right_function,
                    else => false,
                },
                .closure => |left| switch (right) {
                    .closure => |right_closure| left == right_closure,
                    else => false,
                },
                .class_def => |left| switch (right) {
                    .class_def => |right_class| left == right_class,
                    else => false,
                },
                .instance => |left| switch (right) {
                    .instance => |right_instance| left == right_instance,
                    else => false,
                },
                .module => |left| switch (right) {
                    .module => |right_module| left == right_module,
                    else => false,
                },
                .nil => switch (right) {
                    .nil => true,
                    else => false,
                },
            };
            value = .{ .boolean = switch (op) {
                .equal_equal => equal,
                .bang_equal => !equal,
                else => false,
            } };
        }
        return value;
    }

    fn comparison(self: *Interpreter) anyerror!Value {
        var value = try self.term();
        while (self.matchAny(&.{ .greater, .greater_equal, .less, .less_equal })) {
            const op = self.previous.type;
            const right = try self.term();
            if (@as(std.meta.Tag(Value), value) != .number or @as(std.meta.Tag(Value), right) != .number) return error.OperandsMustBeNumbers;
            const left_number = value.number;
            const right_number = right.number;
            value = .{ .boolean = switch (op) {
                .greater => left_number > right_number,
                .greater_equal => left_number >= right_number,
                .less => left_number < right_number,
                .less_equal => left_number <= right_number,
                else => false,
            } };
        }
        return value;
    }

    fn term(self: *Interpreter) anyerror!Value {
        var value = try self.factor();
        while (self.matchAny(&.{ .plus, .minus })) {
            const op = self.previous.type;
            const right = try self.factor();
            switch (op) {
                .plus => {
                    if (value == .number and right == .number) {
                        value = .{ .number = value.number + right.number };
                    } else if (value == .string and right == .string) {
                        value = .{ .string = try self.concatStrings(value.string, right.string) };
                    } else {
                        return error.OperandsMustMatch;
                    }
                },
                .minus => {
                    if (value != .number or right != .number) return error.OperandsMustBeNumbers;
                    value = .{ .number = value.number - right.number };
                },
                else => {},
            }
        }
        return value;
    }

    fn factor(self: *Interpreter) anyerror!Value {
        var value = try self.unary();
        while (self.matchAny(&.{ .star, .slash })) {
            const op = self.previous.type;
            const right = try self.unary();
            if (value != .number or right != .number) return error.OperandsMustBeNumbers;
            value = switch (op) {
                .star => .{ .number = value.number * right.number },
                .slash => .{ .number = value.number / right.number },
                else => unreachable,
            };
        }
        return value;
    }

    fn unary(self: *Interpreter) anyerror!Value {
        if (self.matchAny(&.{ .bang, .minus })) {
            const op = self.previous.type;
            const right = try self.unary();
            return switch (op) {
                .bang => .{ .boolean = !right.isTruthy() },
                .minus => blk: {
                    if (right != .number) return error.OperandMustBeNumber;
                    break :blk .{ .number = -right.number };
                },
                else => unreachable,
            };
        }
        return self.call();
    }

    fn call(self: *Interpreter) anyerror!Value {
        var value = try self.primary();
        while (true) {
            if (self.match(.left_paren)) {
                var args: [MAX_PARAMS]Value = undefined;
                var arg_count: usize = 0;
                if (!self.check(.right_paren)) {
                    while (true) {
                        if (arg_count >= MAX_PARAMS) return error.TooManyParameters;
                        args[arg_count] = try self.expression();
                        arg_count += 1;
                        if (!self.match(.comma)) break;
                    }
                }
                try self.consume(.right_paren, "Expect ')' after arguments.");
                value = try self.callValue(value, args[0..arg_count]);
                continue;
            }
            if (self.match(.dot)) {
                const property = try self.consumeIdentifier("Expect property name after '.'.");
                value = try self.getProperty(value, property.lexeme);
                continue;
            }
            break;
        }
        return value;
    }

    fn primary(self: *Interpreter) anyerror!Value {
        if (self.match(.nil)) return .nil;
        if (self.match(.true_kw)) return .{ .boolean = true };
        if (self.match(.false_kw)) return .{ .boolean = false };

        if (self.match(.number)) {
            const parsed = std.fmt.parseFloat(f64, self.previous.lexeme) catch return error.InvalidNumber;
            return .{ .number = parsed };
        }

        if (self.match(.string)) {
            return .{ .string = self.previous.lexeme[1 .. self.previous.lexeme.len - 1] };
        }

        if (self.match(.identifier)) {
            return self.lookupName(self.previous.lexeme);
        }

        if (self.match(.this)) {
            return self.lookupName("this");
        }

        if (self.match(.super)) {
            return self.lookupName("super");
        }

        if (self.match(.left_paren)) {
            const value = try self.expression();
            try self.consume(.right_paren, "Expect ')' after expression.");
            return value;
        }

        return self.errorAt(self.current, "Expect expression.");
    }

    fn callValue(self: *Interpreter, callee: Value, args: []const Value) !Value {
        switch (callee) {
            .closure => |instance| return self.invoke(instance, args),
            .class_def => |class_def| return self.construct(class_def, args),
            else => return error.NotCallable,
        }
    }

    fn construct(self: *Interpreter, class_def: *ClassDefinition, args: []const Value) !Value {
        const instance = try self.allocator.create(Instance);
        instance.* = Instance.init(self.allocator, class_def);

        if (class_def.findMethod("init")) |initializer| {
            const bound = try self.bindMethod(instance, initializer, class_def);
            _ = try self.invoke(bound, args);
        } else if (args.len != 0) {
            return error.ArityMismatch;
        }

        return .{ .instance = instance };
    }

    fn invoke(self: *Interpreter, instance: *FunctionInstance, args: []const Value) !Value {
        const template = instance.template;
        if (args.len != template.param_count) return error.ArityMismatch;

        const previous_environment = self.environment;
        const call_env = try self.allocator.create(Environment);
        call_env.* = Environment.init(self.allocator, instance.closure);
        self.environment = call_env;
        defer self.environment = previous_environment;

        var index: usize = 0;
        while (index < template.param_count) : (index += 1) {
            try self.environment.define(template.param_names[index], args[index]);
        }

        var body_interpreter = Interpreter{
            .allocator = self.allocator,
            .io = self.io,
            .source_path = self.source_path,
            .source = template.body_source,
            .lexer = Lexer.init(template.body_source, self.allocator),
            .current = undefined,
            .previous = undefined,
            .globals = self.globals,
            .environment = self.environment,
        };
        body_interpreter.advance();
        try body_interpreter.run();
        return body_interpreter.return_value orelse .nil;
    }

    fn bindMethod(self: *Interpreter, instance: *Instance, method: *FunctionInstance, owner_class: *ClassDefinition) !*FunctionInstance {
        const bound_environment = try self.allocator.create(Environment);
        bound_environment.* = Environment.init(self.allocator, method.closure);
        try bound_environment.define("this", .{ .instance = instance });
        if (owner_class.superclass) |superclass| {
            try bound_environment.define("super", .{ .class_def = superclass });
        }

        const bound = try self.allocator.create(FunctionInstance);
        bound.* = .{ .template = method.template, .closure = bound_environment };
        return bound;
    }

    fn lookupName(self: *Interpreter, name: []const u8) !Value {
        return self.environment.get(name) orelse return error.UndefinedVariable;
    }

    fn assignName(self: *Interpreter, name: []const u8, value: Value) !void {
        try self.environment.assign(name, value);
    }

    fn getProperty(self: *Interpreter, target: Value, name: []const u8) !Value {
        return switch (target) {
            .instance => |instance| {
                if (instance.getField(name)) |value| return value;
                if (instance.class.findMethod(name)) |method| {
                    const bound = try self.bindMethod(instance, method, instance.class);
                    return .{ .closure = bound };
                }
                return error.UndefinedProperty;
            },
            .module => |module_environment| module_environment.get(name) orelse return error.UndefinedProperty,
            .class_def => |class_def| {
                const this_value = self.environment.get("this") orelse return error.UndefinedProperty;
                const instance = switch (this_value) {
                    .instance => |instance| instance,
                    else => return error.UndefinedProperty,
                };

                if (class_def.findMethod(name)) |method| {
                    const bound = try self.bindMethod(instance, method, class_def);
                    return .{ .closure = bound };
                }
                return error.UndefinedProperty;
            },
            else => error.NotCallable,
        };
    }

    fn assignProperty(self: *Interpreter, target: Value, name: []const u8, value: Value) !void {
        _ = self;
        switch (target) {
            .instance => |instance| try instance.setField(name, value),
            .module => |module_environment| try module_environment.define(name, value),
            else => return error.UndefinedProperty,
        }
    }

    fn captureBlockSource(self: *Interpreter) ![]const u8 {
        const body_start = self.current.start;
        var depth: usize = 1;
        while (depth > 0) {
            self.advance();
            switch (self.current.type) {
                .left_brace => depth += 1,
                .right_brace => depth -= 1,
                .eof => return error.UnterminatedBlock,
                else => {},
            }
        }

        const body_end = self.current.start;
        const body = self.source[body_start..body_end];
        self.advance();
        return body;
    }

    fn consumeString(self: *Interpreter, message: []const u8) !Token {
        if (self.current.type == .string) {
            const token = self.current;
            self.advance();
            return token;
        }
        try self.reportError(self.current, message);
        return error.ParseError;
    }

    fn moduleNameFromPath(self: *Interpreter, path: []const u8) []const u8 {
        _ = self;
        const separator = std.mem.lastIndexOfAny(u8, path, "/\\") orelse 0;
        const file_name = if (separator == 0 and !(path.len > 0 and (path[0] == '/' or path[0] == '\\'))) path else path[separator + 1 ..];
        const dot_index = std.mem.lastIndexOfScalar(u8, file_name, '.') orelse file_name.len;
        return file_name[0..dot_index];
    }

    fn loadModule(self: *Interpreter, path: []const u8) !*Environment {
        var buffer: [1024 * 1024]u8 = undefined;
        const source = try std.Io.Dir.readFile(std.Io.Dir.cwd(), self.io, path, &buffer);

        const module_environment = try self.allocator.create(Environment);
        module_environment.* = Environment.init(self.allocator, null);

        const saved_source = self.source;
        const saved_source_path = self.source_path;
        const saved_lexer = self.lexer;
        const saved_current = self.current;
        const saved_previous = self.previous;
        const saved_environment = self.environment;
        const saved_return_value = self.return_value;
        const saved_had_error = self.had_error;
        const saved_panic_mode = self.panic_mode;

        self.source = source;
        self.source_path = path;
        self.lexer = Lexer.init(source, self.allocator);
        self.current = undefined;
        self.previous = undefined;
        self.environment = module_environment;
        self.return_value = null;
        self.had_error = false;
        self.panic_mode = false;
        self.advance();

        errdefer {
            self.source = saved_source;
            self.source_path = saved_source_path;
            self.lexer = saved_lexer;
            self.current = saved_current;
            self.previous = saved_previous;
            self.environment = saved_environment;
            self.return_value = saved_return_value;
            self.had_error = saved_had_error;
            self.panic_mode = saved_panic_mode;
        }

        try self.run();

        self.source = saved_source;
        self.source_path = saved_source_path;
        self.lexer = saved_lexer;
        self.current = saved_current;
        self.previous = saved_previous;
        self.environment = saved_environment;
        self.return_value = saved_return_value;
        self.had_error = saved_had_error;
        self.panic_mode = saved_panic_mode;

        return module_environment;
    }

    fn resolveImportPath(self: *Interpreter, imported_path: []const u8) ![]u8 {
        if (std.fs.path.isAbsolute(imported_path)) {
            return self.allocator.dupe(u8, imported_path);
        }

        const base_dir = std.fs.path.dirname(self.source_path) orelse ".";
        if (std.mem.eql(u8, base_dir, ".")) {
            return self.allocator.dupe(u8, imported_path);
        }

        return std.fs.path.join(self.allocator, &.{ base_dir, imported_path });
    }

    fn captureDelimitedSource(self: *Interpreter, terminator: TokenType) ![]const u8 {
        const start = self.current.start;
        var temp = self.lexer;
        temp.start = self.current.start;
        temp.current = self.current.start;

        var paren_depth: usize = 0;
        while (true) {
            const token = temp.nextToken();
            switch (token.type) {
                .left_paren => paren_depth += 1,
                .right_paren => {
                    if (terminator == .right_paren and paren_depth == 0) {
                        self.current = token;
                        self.lexer.current = token.end;
                        return self.source[start..token.start];
                    }
                    if (paren_depth > 0) paren_depth -= 1;
                },
                .semicolon => {
                    if (terminator == .semicolon and paren_depth == 0) {
                        self.current = token;
                        self.lexer.current = token.end;
                        return self.source[start..token.start];
                    }
                },
                .eof => return error.UnterminatedBlock,
                else => {},
            }
        }
    }

    fn evaluateExpressionSource(self: *Interpreter, source: []const u8, environment: *Environment) !Value {
        var child = self.spawnChildInterpreter(source, environment);
        const value = try child.expression();
        return value;
    }

    fn runSnippetSource(self: *Interpreter, source: []const u8, environment: *Environment) !?Value {
        const saved_source = self.source;
        const saved_lexer = self.lexer;
        const saved_current = self.current;
        const saved_previous = self.previous;
        const saved_environment = self.environment;
        const saved_return_value = self.return_value;
        const saved_had_error = self.had_error;
        const saved_panic_mode = self.panic_mode;

        self.source = source;
        self.lexer = Lexer.init(source, self.allocator);
        self.current = undefined;
        self.previous = undefined;
        self.environment = environment;
        self.return_value = null;
        self.had_error = false;
        self.panic_mode = false;
        self.advance();

        errdefer {
            self.source = saved_source;
            self.lexer = saved_lexer;
            self.current = saved_current;
            self.previous = saved_previous;
            self.environment = saved_environment;
            self.return_value = saved_return_value;
            self.had_error = saved_had_error;
            self.panic_mode = saved_panic_mode;
        }

        try self.run();
        const result = self.return_value;

        self.source = saved_source;
        self.lexer = saved_lexer;
        self.current = saved_current;
        self.previous = saved_previous;
        self.environment = saved_environment;
        self.return_value = saved_return_value;
        self.had_error = saved_had_error;
        self.panic_mode = saved_panic_mode;

        return result;
    }

    fn spawnChildInterpreter(self: *Interpreter, source: []const u8, environment: *Environment) Interpreter {
        var child = Interpreter{
            .allocator = self.allocator,
            .io = self.io,
            .source_path = self.source_path,
            .source = source,
            .lexer = Lexer.init(source, self.allocator),
            .current = undefined,
            .previous = undefined,
            .globals = self.globals,
            .environment = environment,
        };
        child.advance();
        return child;
    }

    fn canAssignProperty(self: *Interpreter) bool {
        return self.current.type == .identifier or self.current.type == .this;
    }

    fn isPropertyAssignmentAhead(self: *Interpreter) bool {
        var temp = self.lexer;
        temp.start = self.current.end;
        temp.current = self.current.end;

        const dot = temp.nextToken();
        if (dot.type != .dot) return false;
        const name = temp.nextToken();
        if (name.type != .identifier) return false;
        const equal = temp.nextToken();
        return equal.type == .equal;
    }

    const PropertyTarget = struct {
        object: Value,
        property: []const u8,
    };

    fn consumePropertyTarget(self: *Interpreter) !PropertyTarget {
        const object = switch (self.current.type) {
            .identifier => try self.lookupName(self.current.lexeme),
            .this => try self.lookupName("this"),
            else => return error.ParseError,
        };

        self.advance();
        try self.consume(.dot, "Expect '.' in property assignment.");
        const property = try self.consumeIdentifier("Expect property name after '.'.");
        return .{ .object = object, .property = property.lexeme };
    }

    fn match(self: *Interpreter, ttype: TokenType) bool {
        if (!self.check(ttype)) return false;
        self.advance();
        return true;
    }

    fn matchAny(self: *Interpreter, types: []const TokenType) bool {
        for (types) |ttype| {
            if (self.match(ttype)) return true;
        }
        return false;
    }

    fn isAssignmentAhead(self: *Interpreter) bool {
        var temp = self.lexer;
        temp.start = self.current.end;
        temp.current = self.current.end;
        const next = temp.nextToken();
        return next.type == .equal;
    }

    fn check(self: *Interpreter, ttype: TokenType) bool {
        return self.current.type == ttype;
    }

    fn advance(self: *Interpreter) void {
        self.previous = self.current;
        self.current = self.lexer.nextToken();
        if (self.current.type == .@"error") {
            self.reportError(self.current, self.current.lexeme) catch {};
        }
    }

    fn consume(self: *Interpreter, ttype: TokenType, message: []const u8) !void {
        if (self.current.type == ttype) {
            self.advance();
            return;
        }
        try self.reportError(self.current, message);
        return error.ParseError;
    }

    fn consumeIdentifier(self: *Interpreter, message: []const u8) !Token {
        if (self.current.type == .identifier) {
            const token = self.current;
            self.advance();
            return token;
        }
        try self.reportError(self.current, message);
        return error.ParseError;
    }

    fn printValue(self: *Interpreter, value: Value) void {
        _ = self;
        switch (value) {
            .number => |number| std.debug.print("{}\n", .{number}),
            .boolean => |boolean| std.debug.print("{}\n", .{boolean}),
            .string => |string| std.debug.print("{s}\n", .{string}),
            .function_template => |template| std.debug.print("<fn {s}>\n", .{template.name}),
            .closure => |instance| std.debug.print("<fn {s}>\n", .{instance.template.name}),
            .class_def => |class_def| std.debug.print("<class {s}>\n", .{class_def.name}),
            .instance => |instance| std.debug.print("<instance {s}>\n", .{instance.class.name}),
            .module => std.debug.print("<module>\n", .{}),
            .nil => std.debug.print("nil\n", .{}),
        }
    }

    fn assignPropertyStatement(self: *Interpreter) !void {
        const target = try self.consumePropertyTarget();
        try self.consume(.equal, "Expect '=' in assignment.");
        const value = try self.expression();
        try self.consume(.semicolon, "Expect ';' after assignment.");
        try self.assignProperty(target.object, target.property, value);
    }

    fn concatStrings(self: *Interpreter, left: []const u8, right: []const u8) ![]const u8 {
        var result = try self.allocator.alloc(u8, left.len + right.len);
        @memcpy(result[0..left.len], left);
        @memcpy(result[left.len..], right);
        return result;
    }

    fn reportError(self: *Interpreter, token: Token, message: []const u8) anyerror!void {
        if (self.panic_mode) return error.ParseError;
        self.panic_mode = true;

        std.debug.print("[line {}] Error", .{token.line});
        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type != .@"error") {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }
        std.debug.print(": {s}\n", .{message});
        self.had_error = true;
    }

    fn errorAt(self: *Interpreter, token: Token, message: []const u8) !Value {
        try self.reportError(token, message);
        return error.ParseError;
    }
};
