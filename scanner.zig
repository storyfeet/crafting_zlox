const std = @import("std");
const fs = std.fs;
const expect = std.testing.expect;
//const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

const SYMBOL = "@{}()[]\\\"*+-/?<>|%";
const SPACE = "'\n\r\t ";
const SYM_SPACE = SYMBOL ++ SPACE;

pub fn readFile(path: [*]const u8, alloc: *std.mem.Allocator) ![]u8 {
    const f: fs.File = try fs.cwd().openFile(path, .{ .read = true });
    defer f.close();

    return try f.readToEndAlloc(alloc);
}

pub const TokenType = enum { LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
// Literals.
IDENT, STRING, NUMBER,
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
    UnterminatedString,
    NonUnicode,
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
            '"' => return try self.string(),
            else => {},
        }
        if (isDigit(c)) {
            return self.number();
        }
        if (isLetter(c)) {
            return self.ident();
        }
        return error.UnexpectedChar;
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

    fn string(self: *@This()) !Token {
        while (true) {
            var c = self.uts.peek(1);
            if (c.len == 0) return error.UnterminatedString;
            if (std.mem.eql(u8, c, "\"")) {
                _ = self.uts.nextCodepoint();
                return self.makeToken(TokenType.STRING);
            }
            if (std.mem.eql(u8, c, "\n")) {
                self.line += 1;
            }
            _ = self.uts.nextCodepoint();
        }
    }

    fn number(self: *@This()) !Token {
        var hasPoint = false;
        while (true) {
            const pk = self.uts.peek(1);
            if (pk.len == 0) return self.makeToken(TokenType.NUMBER);
            const c = std.unicode.utf8Decode(pk) catch |_| return error.NonUnicode;
            if (isDigit(c)) {
                _ = self.uts.nextCodepoint();
                continue;
            }
            if (c == '.') {
                if (hasPoint) return self.makeToken(TokenType.NUMBER);
                hasPoint = true;
                _ = self.uts.nextCodepoint();
                continue;
            }
            return self.makeToken(TokenType.NUMBER);
        }
    }

    fn ident(self: *@This()) !Token {
        while (true) {
            const pk = self.uts.peek(1);
            if (pk.len == 0) return self.makeToken(TokenType.IDENT);
            const cp = std.unicode.utf8Decode(pk) catch return error.NonUnicode;
            if (isSymSpace(cp)) {
                return self.makeToken(TokenType.IDENT);
            }
            _ = self.uts.nextCodepoint();
        }
    }
};

fn isDigit(c: u21) bool {
    return c >= '0' and c <= '9';
}

fn isLetter(c: u21) bool {
    return !isSymSpace(c) and !isDigit(c);
}

fn isSymSpace(c: u21) bool {
    var it = std.unicode.Utf8Iterator{ .i = 0, .bytes = SYM_SPACE };
    while (it.nextCodepoint()) |cp| {
        if (cp == c) {
            return true;
        }
    }
    return false;
}

test "tokenizer tokens something" {
    var base = "( !=//你好hello\n\"f你\" )34.7 thing";
    var sc = Tokenizer.init(base);
    try expect((try sc.nextToken()).kind == TokenType.LEFT_PAREN);
    try expect((try sc.nextToken()).kind == TokenType.BANG_EQUAL);
    var st = try sc.nextToken();
    try expect(st.kind == TokenType.STRING);
    try expect(std.mem.eql(u8, base[st.start..st.end], "\"f你\""));
    try expect((try sc.nextToken()).kind == TokenType.RIGHT_PAREN);
    st = try sc.nextToken();
    try expect(st.kind == TokenType.NUMBER);
    try expect(std.mem.eql(u8, base[st.start..st.end], "34.7"));
}

test "tokenize line of code" {
    var base = "啊们3 = 3.4 + 100 * g";
    var tzer = Tokenizer.init(base);

    var nt = try tzer.nextToken();
    try expect(nt.kind == TokenType.IDENT);
    try expect(std.mem.eql(u8, base[nt.start..nt.end], "啊们3"));
    try expect((try tzer.nextToken()).kind == TokenType.EQUAL);
    try expect((try tzer.nextToken()).kind == TokenType.NUMBER);
    try expect((try tzer.nextToken()).kind == TokenType.PLUS);
    try expect((try tzer.nextToken()).kind == TokenType.NUMBER);
    try expect((try tzer.nextToken()).kind == TokenType.STAR);
    try expect((try tzer.nextToken()).kind == TokenType.IDENT);
}
