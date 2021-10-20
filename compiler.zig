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
    ExpectedRightParen,
    ExpectedMinus,
    ExpectedMathOp,
    ExpectedExpression,
    ExpectedInfix,
    OutOfMemory,
} || scanner.ScanError;

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
            .scanner = sc,
            .chk = ch,
        };
    }

    pub fn advance(self: *@This()) ParseError!void {
        self.prev = self.curr;
        self.curr = try self.scanner.nextToken();
    }

    pub fn consume(self: *@This(), tk: TokenType, e: ParseError) ParseError!void {
        if (self.curr.kind == tk) {
            try self.advance();
            return;
        }
        return e;
    }

    pub fn expression(self: *@This()) ParseError!void {
        try self.parsePrecedence(.ASSIGNMENT);
    }
    pub fn parsePrecedence(self: *@This(), prec: Precedence) ParseError!void {
        try self.advance();
        const preFn: ParseFn = getRule(self.prev.kind).prefix orelse return error.ExpectedExpression;
        try preFn(self);
        while (prec <= getRule(self.curr.kind).precedence) {
            try self.advance();
            const inFn: ParseFn = getRule(self.prev.kind).infix orelse return error.ExpectedInfix;
            try inFn(self);
        }
    }

    pub fn grouping(self: *@This()) ParseError!void {
        try self.expression();
        try self.consume(.RIGHT_PAREN, error.ExpectedRightParen);
    }

    pub fn unary(self: *@This()) ParseError!void {
        const opType = self.prev.kind;
        try self.expression();
        switch (opType) {
            .MINUS => try self.chk.addOp(.NEGATE),
            else => return error.ExpectedMinus,
        }
    }

    pub fn binary(self: *@This()) ParseError!void {
        const opType = self.prev.kind;
        const rule = getRule(opType);
        try self.parsePrecedence(rule.precedence.inc());
        switch (opType) {
            .PLUS => try self.chk.addOp(.ADD),
            .MINUS => try self.chk.addOp(.SUB),
            .STAR => try self.chk.addOp(.MUL),
            .SLASH => try self.chk.addOp(.DIV),
            else => return error.ExpectedMathOp,
        }
    }
    pub fn number(self: *@This()) ParseError!void {
        var s = self.scanner.tokenStr(self.curr);
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addOp(chunk.OpData{ .CONSTANT = val });
    }
};

const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
    fn inc(self: @This()) @This() {
        if (self == .PRIMARY) return .PRIMARY;
        return @intToEnum(@This(), @enumToInt(self) + 1);
    }
};

const ParseFn = fn (*Parser) ParseError!void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .NONE,
};

fn getRule(tk: TokenType) ParseRule {
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
    try expect(std.meta.eql(getRule(.LEFT_PAREN), ParseRule{ .prefix = Parser.grouping, .infix = null, .precedence = .NONE }));
}
