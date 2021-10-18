const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const std = @import("std");
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const expect = std.testing.expect;

pub fn compile(s: []const u8, ch: *chunk.Chunk) !void {
    _ = try Parser.init(s, ch);
}

const ParseError = error{
    InvalidCharacter,
    FloatParseError,
    UnexpectedToken,
    UnexpectedEOF,
    OutOfMemory,
};

const Parser = struct {
    prev: Token,
    curr: Token,
    scanner: scanner.Tokenizer,
    chk: *chunk.Chunk,

    pub fn init(s: []const u8, ch: *chunk.Chunk) !@This() {
        var sc = scanner.Tokenizer.init(s);
        return Parser{
            .prev = undefined,
            .curr = try sc.nextToken(),
            .scanner = scanner.Tokenizer.init(s),
            .chk = ch,
        };
    }

    pub fn advance(self: @This()) !void {
        self.prev = self.current;
        self.curr = try self.scanner.nextToken();
    }

    pub fn parsePrecedence(self: *@This()) ParseError!void {}
    pub fn grouping(self: *@This()) ParseError!void {}
    pub fn unary(self: *@This()) ParseError!void {}
    pub fn binary(self: *@This()) ParseError!void {}
    pub fn number(self: *@This()) ParseError!void {
        var s = self.scanner.tokenStr(self.curr);
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addOp(chunk.OpData{ .CONSTANT = val });
    }
};

const Precedence = enum(u8) { NONE, ASSIGNMENT, // =
OR, // or
AND, // and
EQUALITY, // == !=
COMPARISON, // < > <= >=
TERM, // + -
FACTOR, // * /
UNARY, // ! -
CALL, // . ()
PRIMARY };

const ParseFn = fn (*Parser) ParseError!void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
};

fn ruleTable(tk: TokenType) ParseRule {
    return switch (tk) {
        .LEFT_PAREN => ParseRule{ .prefix = Parser.grouping, .infix = null, .precedence = .NONE },
        .RIGHT_PAREN => ParseRule{},
        .LEFT_BRACE => ParseRule{},
        .RIGHT_BRACE => ParseRule{},
        .COMMA => ParseRule{},
        .DOT => ParseRule{},
        .MINUS => ParseRule{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM },
        .PLUS => ParseRule{ .infix = Parser.binary, .precedence = .TERM },
        .SEMICOLON => ParseRule{},
        .SLASH => ParseRule{ .infix = Parser.binary, .precedence = .FACTOR },
        .STAR => ParseRule{ .infix = Parser.binary, .precedence = .FACTOR },
        .BANG => ParseRule{},
        .BANG_EQUAL => ParseRule{},
        .EQUAL => ParseRule{},
        .EQUAL_EQUAL => ParseRule{},
        .GREATER => ParseRule{},
        .GREATER_EQUAL => ParseRule{},
        .LESS => ParseRule{},
        .LESS_EQUAL => ParseRule{},
        .IDENT => ParseRule{},
        .STRING => ParseRule{},
        .NUMBER => ParseRule{ .prefix = Parser.number },
        .AND => ParseRule{},
        .CLASS => ParseRule{},
        .ELSE => ParseRule{},
        .FALSE => ParseRule{},
        .FOR => ParseRule{},
        .FUN => ParseRule{},
        .IF => ParseRule{},
        .NIL => ParseRule{},
        .OR => ParseRule{},
        .PRINT => ParseRule{},
        .RETURN => ParseRule{},
        .SUPER => ParseRule{},
        .THIS => ParseRule{},
        .TRUE => ParseRule{},
        .VAR => ParseRule{},
        .WHILE => ParseRule{},
        .ERROR => ParseRule{},
        .EOF => ParseRule{},
    };
}

test "can compile something" {
    var gpa = GPAlloc{};
    const s = "print \"hello\"";
    var ch = chunk.Chunk.init(&gpa.allocator);
    try compile(s, &ch);
}

test "rule table functions" {
    try expect(std.meta.eql(ruleTable(TokenType.LEFT_PAREN), ParseRule{ .prefix = Parser.grouping, .infix = null, .precedence = .NONE }));
}
