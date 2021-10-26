const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const std = @import("std");
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const vm = @import("vm.zig");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

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

//TODO fix to not just handle expressions
pub fn compileAndRun(s: []const u8, a: *std.mem.Allocator) !chunk.Value {
    var ch = chunk.Chunk.init(a);
    defer ch.deinit();
    var p: Parser = try Parser.init(s, &ch);
    try p.expression();
    try ch.addOp(.RETURN);
    var theVm = vm.VM.init(&ch, a);
    defer theVm.deinit();
    return try theVm.run();
}

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
        while (@enumToInt(prec) <= @enumToInt(getRule(self.curr.kind).precedence)) {
            try self.advance();
            const inFn: ParseFn = getRule(self.prev.kind).infix orelse return error.ExpectedInfix;
            try inFn(self);
        }
    }

    pub fn literal(self: *@This()) ParseError!void {
        switch (self.prev.kind) {
            .FALSE => try self.chk.addOp(.FALSE),
            .TRUE => try self.chk.addOp(.TRUE),
            .NIL => try self.chk.addOp(.NIL),
            else => unreachable,
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
            .BANG => try self.chk.addOp(.NOT),
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
        var s = self.scanner.tokenStr(self.prev);
        std.debug.print("{s}", .{s});
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addOp(chunk.OpData{ .CONSTANT = .{ .NUMBER = val } });
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
        .MINUS => ParseRule{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM },
        .PLUS => ParseRule{ .infix = Parser.binary, .precedence = .TERM },
        .STAR, .SLASH => ParseRule{ .infix = Parser.binary, .precedence = .FACTOR },
        .BANG => ParseRule{ .prefix = Parser.unary, .precedence = .NONE },
        .NUMBER => ParseRule{ .prefix = Parser.number },
        .FALSE, .TRUE, .NIL => ParseRule{ .prefix = Parser.literal, .precedence = .NONE },
        else => ParseRule{},
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

test "compiles and runs" {
    var gpa = GPAlloc{};
    var res = try compileAndRun("4 + 5 - (3* 2)", &gpa.allocator);
    try expectEqual(res, .{ .NUMBER = 3 });
}

test "compile and run bool" {
    var gpa = GPAlloc{};
    var res = try compileAndRun("!(3-3)", &gpa.allocator);
    try expectEqual(res, chunk.Value{ .BOOL = true });
}
