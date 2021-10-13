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

pub const Scanner = struct {
    uts: std.unicode.Utf8Iterator,
    start: usize,
    line: usize,

    pub fn init(s: []const u8) @This() {
        return Scanner{
            .uts = std.unicode.Utf8Iterator{ .bytes = s, .i = 0 },
            .start = 0,
            .line = 0,
        };
    }

    pub fn nextToken(self: *@This()) ScanError!Token {
        var c = self.uts.nextCodepoint() orelse return self.makeToken(TokenType.EOF);
        switch (c) {
            '(' => return self.makeToken(TokenType.LEFT_PAREN),
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
};

test "scanner scans something" {
    var sc = Scanner.init("(do thing");
    try expect((try sc.nextToken()).kind == TokenType.LEFT_PAREN);
}
