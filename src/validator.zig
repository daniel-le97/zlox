const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const Runner = struct {
    io: std.Io,
    allocator: std.mem.Allocator,
    seen_paths: [64][]u8 = undefined,
    seen_count: usize = 0,

    pub fn init(io: std.Io, allocator: std.mem.Allocator) Runner {
        return .{ .io = io, .allocator = allocator };
    }

    pub fn deinit(self: *Runner) void {
        var index: usize = 0;
        while (index < self.seen_count) : (index += 1) {
            self.allocator.free(self.seen_paths[index]);
        }
    }

    pub fn runPath(self: *Runner, path: []const u8) anyerror!void {
        const resolved = try self.resolvePath(".", path);
        defer self.allocator.free(resolved);

        try self.runResolvedPath(resolved);
    }

    fn runResolvedPath(self: *Runner, resolved_path: []const u8) anyerror!void {
        if (self.isSeen(resolved_path)) return;
        try self.rememberPath(resolved_path);

        var buffer: [1024 * 1024]u8 = undefined;
        const source = try std.Io.Dir.readFile(std.Io.Dir.cwd(), self.io, resolved_path, &buffer);

        var lexer = Lexer.init(source, self.allocator);
        var parser = Parser.init(self, &lexer, resolved_path);
        try parser.parseProgram();
    }

    fn runImportedPath(self: *Runner, base_path: []const u8, imported_path: []const u8) anyerror!void {
        const resolved = try self.resolvePath(base_path, imported_path);
        defer self.allocator.free(resolved);
        self.runResolvedPath(resolved) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
    }

    fn rememberPath(self: *Runner, path: []const u8) !void {
        if (self.seen_count >= self.seen_paths.len) return error.TooManyImportedFiles;
        self.seen_paths[self.seen_count] = try self.allocator.dupe(u8, path);
        self.seen_count += 1;
    }

    fn isSeen(self: *Runner, path: []const u8) bool {
        var index: usize = 0;
        while (index < self.seen_count) : (index += 1) {
            if (std.mem.eql(u8, self.seen_paths[index], path)) return true;
        }
        return false;
    }

    fn resolvePath(self: *Runner, base_path: []const u8, child_path: []const u8) ![]u8 {
        if (isAbsolutePath(child_path)) {
            return self.allocator.dupe(u8, child_path);
        }

        const base_dir = dirname(base_path);
        if (std.mem.eql(u8, base_dir, ".")) {
            return self.allocator.dupe(u8, child_path);
        }

        return std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ base_dir, child_path });
    }
};

const Parser = struct {
    runner: *Runner,
    lexer: *Lexer,
    current: Token,
    previous: Token,
    current_path: []const u8,
    panic_mode: bool = false,

    fn init(runner: *Runner, lexer: *Lexer, current_path: []const u8) Parser {
        var parser = Parser{
            .runner = runner,
            .lexer = lexer,
            .current = undefined,
            .previous = undefined,
            .current_path = current_path,
        };
        parser.advance();
        return parser;
    }

    fn parseProgram(self: *Parser) anyerror!void {
        while (!self.check(.eof)) {
            try self.declaration();
        }
    }

    fn declaration(self: *Parser) anyerror!void {
        if (self.match(.class)) return self.classDeclaration();
        if (self.match(.fun)) return self.functionDeclaration();
        if (self.match(.import_kw)) return self.importDeclaration();
        if (self.match(.module_kw)) return self.moduleDeclaration();
        if (self.match(.@"var")) return self.varDeclaration();

        return self.statement();
    }

    fn classDeclaration(self: *Parser) anyerror!void {
        try self.consume(.identifier, "Expect class name.");
        if (self.match(.less)) {
            try self.consume(.identifier, "Expect superclass name after '<'.");
        }

        try self.consume(.left_brace, "Expect '{' before class body.");
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.consume(.identifier, "Expect method name.");
            try self.functionTail("method");
        }
        try self.consume(.right_brace, "Expect '}' after class body.");
    }

    fn functionDeclaration(self: *Parser) anyerror!void {
        try self.consume(.identifier, "Expect function name.");
        try self.functionTail("function");
    }

    fn functionTail(self: *Parser, kind: []const u8) anyerror!void {
        _ = kind;
        try self.consume(.left_paren, "Expect '(' after name.");
        if (!self.check(.right_paren)) {
            while (true) {
                try self.consume(.identifier, "Expect parameter name.");
                if (!self.match(.comma)) break;
            }
        }
        try self.consume(.right_paren, "Expect ')' after parameters.");
        try self.consume(.left_brace, "Expect '{' before function body.");
        try self.block();
    }

    fn varDeclaration(self: *Parser) anyerror!void {
        try self.consume(.identifier, "Expect variable name.");
        if (self.match(.equal)) {
            try self.expression();
        }
        try self.consume(.semicolon, "Expect ';' after variable declaration.");
    }

    fn importDeclaration(self: *Parser) anyerror!void {
        try self.consume(.string, "Expect string path after import.");
        const imported_path = self.stringValue(self.previous);
        try self.consume(.semicolon, "Expect ';' after import.");
        try self.runner.runImportedPath(self.current_path, imported_path);
    }

    fn moduleDeclaration(self: *Parser) anyerror!void {
        try self.consume(.identifier, "Expect module name.");
        try self.consume(.semicolon, "Expect ';' after module declaration.");
    }

    fn statement(self: *Parser) anyerror!void {
        if (self.match(.print)) return self.printStatement();
        if (self.match(.@"return")) return self.returnStatement();
        if (self.match(.if_kw)) return self.ifStatement();
        if (self.match(.while_kw)) return self.whileStatement();
        if (self.match(.for_kw)) return self.forStatement();
        if (self.match(.break_kw)) return self.simpleStatement();
        if (self.match(.continue_kw)) return self.simpleStatement();
        if (self.match(.left_brace)) return self.block();

        return self.expressionStatement();
    }

    fn simpleStatement(self: *Parser) anyerror!void {
        try self.consume(.semicolon, "Expect ';' after statement.");
    }

    fn printStatement(self: *Parser) anyerror!void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after value.");
    }

    fn returnStatement(self: *Parser) anyerror!void {
        if (!self.check(.semicolon)) {
            try self.expression();
        }
        try self.consume(.semicolon, "Expect ';' after return value.");
    }

    fn ifStatement(self: *Parser) anyerror!void {
        try self.consume(.left_paren, "Expect '(' after if.");
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after condition.");
        try self.statement();
        if (self.match(.else_kw)) {
            try self.statement();
        }
    }

    fn whileStatement(self: *Parser) anyerror!void {
        try self.consume(.left_paren, "Expect '(' after while.");
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after condition.");
        try self.statement();
    }

    fn forStatement(self: *Parser) anyerror!void {
        try self.consume(.left_paren, "Expect '(' after for.");
        if (self.match(.semicolon)) {} else if (self.match(.@"var")) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        if (!self.check(.semicolon)) {
            try self.expression();
        }
        try self.consume(.semicolon, "Expect ';' after loop condition.");

        if (!self.check(.right_paren)) {
            try self.expression();
        }
        try self.consume(.right_paren, "Expect ')' after for clauses.");
        try self.statement();
    }

    fn block(self: *Parser) anyerror!void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }
        try self.consume(.right_brace, "Expect '}' after block.");
    }

    fn expressionStatement(self: *Parser) anyerror!void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after expression.");
    }

    fn expression(self: *Parser) anyerror!void {
        try self.assignment();
    }

    fn assignment(self: *Parser) anyerror!void {
        try self.orExpr();
        if (self.match(.equal)) {
            try self.assignment();
        }
    }

    fn orExpr(self: *Parser) anyerror!void {
        try self.andExpr();
        while (self.match(.or_kw)) {
            try self.andExpr();
        }
    }

    fn andExpr(self: *Parser) anyerror!void {
        try self.equality();
        while (self.match(.and_kw)) {
            try self.equality();
        }
    }

    fn equality(self: *Parser) anyerror!void {
        try self.comparison();
        while (self.matchAny(&.{ .bang_equal, .equal_equal })) {
            try self.comparison();
        }
    }

    fn comparison(self: *Parser) anyerror!void {
        try self.term();
        while (self.matchAny(&.{ .greater, .greater_equal, .less, .less_equal })) {
            try self.term();
        }
    }

    fn term(self: *Parser) anyerror!void {
        try self.factor();
        while (self.matchAny(&.{ .plus, .minus })) {
            try self.factor();
        }
    }

    fn factor(self: *Parser) anyerror!void {
        try self.unary();
        while (self.matchAny(&.{ .star, .slash })) {
            try self.unary();
        }
    }

    fn unary(self: *Parser) anyerror!void {
        if (self.matchAny(&.{ .bang, .minus })) {
            try self.unary();
            return;
        }
        try self.call();
    }

    fn call(self: *Parser) anyerror!void {
        try self.primary();

        while (true) {
            if (self.match(.left_paren)) {
                if (!self.check(.right_paren)) {
                    while (true) {
                        try self.expression();
                        if (!self.match(.comma)) break;
                    }
                }
                try self.consume(.right_paren, "Expect ')' after arguments.");
                continue;
            }

            if (self.match(.dot)) {
                try self.consume(.identifier, "Expect property name after '.'.");
                continue;
            }

            break;
        }
    }

    fn primary(self: *Parser) anyerror!void {
        if (self.matchAny(&.{ .false_kw, .true_kw, .nil, .number, .string, .identifier, .this })) return;

        if (self.match(.super)) {
            try self.consume(.dot, "Expect '.' after super.");
            try self.consume(.identifier, "Expect superclass method name.");
            return;
        }

        if (self.match(.left_paren)) {
            try self.expression();
            try self.consume(.right_paren, "Expect ')' after expression.");
            return;
        }

        return self.errorAtCurrent("Expect expression.");
    }

    fn match(self: *Parser, ttype: TokenType) bool {
        if (!self.check(ttype)) return false;
        self.advance();
        return true;
    }

    fn matchAny(self: *Parser, types: []const TokenType) bool {
        for (types) |ttype| {
            if (self.match(ttype)) return true;
        }
        return false;
    }

    fn check(self: *Parser, ttype: TokenType) bool {
        return self.current.type == ttype;
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.lexer.nextToken();
        if (self.current.type == .@"error") {
            self.errorAtCurrent(self.current.lexeme) catch {};
        }
    }

    fn consume(self: *Parser, ttype: TokenType, message: []const u8) anyerror!void {
        if (self.current.type == ttype) {
            self.advance();
            return;
        }
        return self.errorAtCurrent(message);
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror!void {
        return self.errorAt(self.current, message);
    }

    fn errorAt(self: *Parser, token: Token, message: []const u8) anyerror!void {
        if (self.panic_mode) return error.ParseError;
        self.panic_mode = true;

        std.debug.print("[line {}] Error", .{token.line});
        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type != .@"error") {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }
        std.debug.print(": {s}\n", .{message});
        return error.ParseError;
    }

    fn stringValue(self: *Parser, token: Token) []const u8 {
        _ = self;
        if (token.lexeme.len >= 2 and token.lexeme[0] == '"' and token.lexeme[token.lexeme.len - 1] == '"') {
            return token.lexeme[1 .. token.lexeme.len - 1];
        }
        return token.lexeme;
    }
};

fn isAbsolutePath(path: []const u8) bool {
    return path.len >= 1 and (path[0] == '/' or path[0] == '\\' or (path.len >= 2 and path[1] == ':'));
}

fn dirname(path: []const u8) []const u8 {
    var index: usize = path.len;
    while (index > 0) : (index -= 1) {
        const c = path[index - 1];
        if (c == '/' or c == '\\') {
            if (index == 1) return path[0..1];
            return path[0 .. index - 1];
        }
    }
    return ".";
}
