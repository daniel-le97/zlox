//! Lexer - tokenizes Lox source code
const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    // One or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // Literals
    identifier,
    string,
    number,

    // Keywords
    and_kw,
    class,
    break_kw,
    continue_kw,
    import_kw,
    else_kw,
    false_kw,
    for_kw,
    fun,
    if_kw,
    module_kw,
    nil,
    or_kw,
    print,
    @"return",
    super,
    this,
    true_kw,
    @"var",
    while_kw,

    // Special
    eof,
    @"error",
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: ?[]const u8 = null,
    line: usize,
    start: usize,
    end: usize,
};

pub const Lexer = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    allocator: std.mem.Allocator,
    peeked: ?Token = null,

    pub fn init(source: []const u8, allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .source = source,
            .allocator = allocator,
        };
    }

    pub fn nextToken(self: *Lexer) Token {
        if (self.peeked) |tok| {
            self.peeked = null;
            return tok;
        }
        return self.nextTokenInternal();
    }

    pub fn peekToken(self: *Lexer) Token {
        if (self.peeked == null) {
            self.peeked = self.nextTokenInternal();
        }
        return self.peeked.?;
    }

    fn nextTokenInternal(self: *Lexer) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            ',' => self.makeToken(.comma),
            '.' => self.makeToken(.dot),
            '-' => self.makeToken(.minus),
            '+' => self.makeToken(.plus),
            ';' => self.makeToken(.semicolon),
            '*' => self.makeToken(.star),
            '!' => {
                const ttype: TokenType = if (self.match('=')) .bang_equal else .bang;
                return self.makeToken(ttype);
            },
            '=' => {
                const ttype: TokenType = if (self.match('=')) .equal_equal else .equal;
                return self.makeToken(ttype);
            },
            '<' => {
                const ttype: TokenType = if (self.match('=')) .less_equal else .less;
                return self.makeToken(ttype);
            },
            '>' => {
                const ttype: TokenType = if (self.match('=')) .greater_equal else .greater;
                return self.makeToken(ttype);
            },
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    return self.nextToken();
                }
                return self.makeToken(.slash);
            },
            '"' => self.string(),
            else => self.errorToken("Unexpected character."),
        };
    }

    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.string);
    }

    fn number(self: *Lexer) Token {
        while (isDigit(self.peek())) _ = self.advance();

        // Look for fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume the dot
            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(.number);
    }

    fn identifier(self: *Lexer) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Lexer) TokenType {
        const lexeme = self.source[self.start..self.current];
        return if (std.mem.eql(u8, lexeme, "and")) .and_kw else if (std.mem.eql(u8, lexeme, "break")) .break_kw else if (std.mem.eql(u8, lexeme, "class")) .class else if (std.mem.eql(u8, lexeme, "continue")) .continue_kw else if (std.mem.eql(u8, lexeme, "else")) .else_kw else if (std.mem.eql(u8, lexeme, "false")) .false_kw else if (std.mem.eql(u8, lexeme, "for")) .for_kw else if (std.mem.eql(u8, lexeme, "fun")) .fun else if (std.mem.eql(u8, lexeme, "if")) .if_kw else if (std.mem.eql(u8, lexeme, "import")) .import_kw else if (std.mem.eql(u8, lexeme, "module")) .module_kw else if (std.mem.eql(u8, lexeme, "nil")) .nil else if (std.mem.eql(u8, lexeme, "or")) .or_kw else if (std.mem.eql(u8, lexeme, "print")) .print else if (std.mem.eql(u8, lexeme, "return")) .@"return" else if (std.mem.eql(u8, lexeme, "super")) .super else if (std.mem.eql(u8, lexeme, "this")) .this else if (std.mem.eql(u8, lexeme, "true")) .true_kw else if (std.mem.eql(u8, lexeme, "var")) .@"var" else if (std.mem.eql(u8, lexeme, "while")) .while_kw else .identifier;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                else => break,
            }
        }
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return '\x00';
        return self.source[self.current];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return '\x00';
        return self.source[self.current + 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Lexer, ttype: TokenType) Token {
        return Token{
            .type = ttype,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
            .start = self.start,
            .end = self.current,
        };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return Token{
            .type = .@"error",
            .lexeme = message,
            .line = self.line,
            .start = self.start,
            .end = self.current,
        };
    }
};

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}
