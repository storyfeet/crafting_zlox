const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const std = @import("std");
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const vm = @import("vm.zig");
const Obj = value.Obj;
const Value = value.Value;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

pub fn compile(s: []const u8, alloc: *std.mem.Allocator, ch: *chunk.Chunk) !void {
    _ = try Parser.init(s, alloc, ch);
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
pub fn compileAndRun(s: []const u8, a: *std.mem.Allocator) !value.Value {
    var ch = chunk.Chunk.init(a);
    defer ch.deinit();
    var p: Parser = try Parser.init(s, a, &ch);
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
    alloc: *std.mem.Allocator,

    pub fn init(s: []const u8, alloc: *std.mem.Allocator, ch: *chunk.Chunk) !@This() {
        var sc = scanner.Tokenizer.init(s);
        return Parser{
            .prev = undefined,
            .curr = try sc.nextToken(),
            .scanner = sc,
            .chk = ch,
            .alloc = alloc,
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
            .EQUAL_EQUAL => try self.chk.addOp(.EQUAL),
            .LESS => try self.chk.addOp(.LESS),
            .GREATER => try self.chk.addOp(.GREATER),
            .BANG_EQUAL => {
                try self.chk.addOp(.EQUAL);
                try self.chk.addOp(.NOT);
            },
            .LESS_EQUAL => {
                try self.chk.addOp(.GREATER);
                try self.chk.addOp(.NOT);
            },
            .GREATER_EQUAL => {
                try self.chk.addOp(.LESS);
                try self.chk.addOp(.NOT);
            },
            else => return error.ExpectedMathOp,
        }
    }

    pub fn number(self: *@This()) ParseError!void {
        var s = self.scanner.tokenStr(self.prev);
        //std.debug.print("{s}", .{s});
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addOp(chunk.OpData{ .CONSTANT = .{ .NUMBER = val } });
    }

    pub fn string(self: *@This()) ParseError!void {
        var s_orig = self.scanner.tokenStr(self.prev);
        const s_copy: []u8 = try self.alloc.alloc(u8, s_orig.len - 2);
        std.mem.copy(u8, s_copy, s_orig[1 .. s_orig.len - 1]);
        const ob: *value.Obj = try self.alloc.create(Obj);
        ob.* = .{ .data = .{ .STR = s_copy } };
        const val = value.Value{ .OBJ = ob };
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
        .MINUS => ParseRule{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM },
        .PLUS => ParseRule{ .infix = Parser.binary, .precedence = .TERM },
        .STAR, .SLASH => ParseRule{ .infix = Parser.binary, .precedence = .FACTOR },
        .BANG => ParseRule{ .prefix = Parser.unary, .precedence = .NONE },
        .NUMBER => ParseRule{ .prefix = Parser.number },
        .FALSE, .TRUE, .NIL => ParseRule{ .prefix = Parser.literal, .precedence = .NONE },
        .GREATER, .LESS, .LESS_EQUAL, .GREATER_EQUAL => ParseRule{ .infix = Parser.binary, .precedence = .COMPARISON },
        .EQUAL_EQUAL => ParseRule{ .infix = Parser.binary, .precedence = .EQUALITY },
        .STRING => ParseRule{ .prefix = Parser.string },
        else => ParseRule{},
    };
}

test "can compile something" {
    var gpa = GPAlloc{};
    const s = "print \"hello\"";
    var ch = chunk.Chunk.init(&gpa.allocator);
    try compile(s, &gpa.allocator, &ch);
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
    try expectEqual(res, value.Value{ .BOOL = true });
}

test "bools equality" {
    var gpa = GPAlloc{};
    var res = try compileAndRun("(4 + 6 > 3 + 6)", &gpa.allocator);
    //std.debug.print("bools eq : {}", .{res});

    const v = value.Value{ .BOOL = true };
    try expect(try v.equal(res));
}

test "string equality" {
    var gpa = GPAlloc{};
    var res = try compileAndRun(
        \\"hello" == ("hel" + "lo")
    , &gpa.allocator);
    //std.debug.print("HELLO  eq : {s}\n", .{res.asStr()});

    const v = value.Value{ .BOOL = true };
    try expect(try v.equal(res));
}
