const std = @import("std");
const fs = std.fs;
const expect = std.testing.expect;
//const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn readFile(path: [*]const u8, alloc: *std.mem.Allocator) ![]u8 {
    const f: fs.File = try fs.cwd().openFile(path, .{ .read = true });
    defer f.close();

    return try f.readToEndAlloc(alloc);
}

pub const TokenType = enum { LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
// Literals.
IDENTIFIER, STRING, NUMBER,
// Keywords.
AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, ERROR, EOF };

pub const Token = struct {
    kind: TokenType,
    start: usize,
    line: usize,
    end: usize,
};

const ScanError = error{
    UnexpectedChar,
};

pub const Tokenizer = struct {
    uts: std.unicode.Utf8Iterator,
    start: usize,
    line: usize,

    pub fn init(s: []const u8) @This() {
        return @This(){
            .uts = std.unicode.Utf8Iterator{ .bytes = s, .i = 0 },
            .start = 0,
            .line = 0,
        };
    }

    pub fn nextToken(self: *@This()) ScanError!Token {
        self.skipWhiteSpace();
        self.start = self.uts.i;
        var c = self.uts.nextCodepoint() orelse return self.makeToken(TokenType.EOF);

        switch (c) {
            '(' => return self.makeToken(TokenType.LEFT_PAREN),
            ')' => return self.makeToken(TokenType.RIGHT_PAREN),
            '{' => return self.makeToken(TokenType.LEFT_BRACE),
            '}' => return self.makeToken(TokenType.RIGHT_BRACE),
            ';' => return self.makeToken(TokenType.SEMICOLON),
            ',' => return self.makeToken(TokenType.COMMA),
            '.' => return self.makeToken(TokenType.DOT),
            '-' => return self.makeToken(TokenType.MINUS),
            '+' => return self.makeToken(TokenType.PLUS),
            '/' => return self.makeToken(TokenType.SLASH),
            '*' => return self.makeToken(TokenType.STAR),
            '!' => return self.makeToken(if (self.match("=")) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => return self.makeToken(if (self.match("=")) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '>' => return self.makeToken(if (self.match("=")) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '<' => return self.makeToken(if (self.match("=")) TokenType.LESS_EQUAL else TokenType.LESS),
            else => return error.UnexpectedChar,
        }
    }

    fn makeToken(self: *@This(), t: TokenType) Token {
        return Token{
            .kind = t,
            .start = self.start,
            .line = self.line,
            .end = self.uts.i,
        };
    }

    fn match(self: *@This(), c: []const u8) bool {
        if (std.mem.eql(u8, c, self.uts.peek(1))) {
            _ = self.uts.nextCodepoint();
            return true;
        }
        return false;
    }

    fn skipWhiteSpace(self: *@This()) void {
        while (true) {
            const c = self.uts.peek(1);
            const cp = std.unicode.utf8Decode(c) catch return;
            switch (cp) {
                ' ', '\t', '\r' => {
                    _ = self.uts.nextCodepoint();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.uts.nextCodepoint();
                },
                '/' => {
                    //comments
                    if (std.mem.eql(u8, self.uts.peek(2), "//")) {
                        loop: while (self.uts.nextCodepoint()) |v| {
                            if (v == '\n') {
                                self.line += 1;
                                break :loop;
                            }
                        }
                    }
                },
                else => return,
            }
        }
    }
};

test "tokenizer tokens something" {
    var sc = Tokenizer.init("( !=//hello\n ) thing");
    try expect((try sc.nextToken()).kind == TokenType.LEFT_PAREN);
    try expect((try sc.nextToken()).kind == TokenType.BANG_EQUAL);
    try expect((try sc.nextToken()).kind == TokenType.RIGHT_PAREN);
}
