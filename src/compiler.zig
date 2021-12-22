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
const assert = std.debug.assert;

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
    ExpectedStatement,
    ExpectedSemicolon,
    ExpectedEquals,
    ExpectedIdent,
    OutOfMemory,
} || scanner.ScanError;

pub fn compileAndRunProgram(s: []const u8, a: *std.mem.Allocator) !void {
    var ch = chunk.Chunk.init(a);
    defer ch.deinit(a);
    var p: Parser = try Parser.init(s, a, &ch);
    try p.program();
    try ch.addOp(.EXIT);
    var theVm = vm.VM.init(&ch, a);
    defer theVm.deinit();
    _ = try theVm.run();
}

pub fn compileAndRunExpression(s: []const u8, a: *std.mem.Allocator) !value.Value {
    var ch = chunk.Chunk.init(a);
    defer ch.deinit(a);
    var p: Parser = try Parser.init(s, a, &ch);
    try p.expression();
    try ch.addOp(.RETURN);
    var theVm = vm.VM.init(&ch, a);
    defer theVm.deinit();
    return try theVm.run();
}

const Parser = struct {
    peek: ?Token,
    scanner: scanner.Tokenizer,
    chk: *chunk.Chunk,
    alloc: *std.mem.Allocator,

    pub fn init(s: []const u8, alloc: *std.mem.Allocator, ch: *chunk.Chunk) !@This() {
        var sc = scanner.Tokenizer.init(s);
        return Parser{
            .peek = null,
            .scanner = sc,
            .chk = ch,
            .alloc = alloc,
        };
    }

    pub fn peekToken(this: *@This()) ParseError!Token {
        if (this.peek) |p| {
            return p;
        }
        var nx = try this.scanner.nextToken();
        this.peek = nx;
        return nx;
    }

    pub fn takeToken(this: *@This()) ParseError!Token {
        if (this.peek) |p| {
            this.peek = null;
            return p;
        }
        return try this.scanner.nextToken();
    }

    pub fn consume(self: *@This(), tk: TokenType, e: ParseError) ParseError!void {
        var nt = try self.takeToken();
        if (nt.kind != tk) {
            return e;
        }
    }

    pub fn program(self: *@This()) ParseError!void {
        var curr = try self.peekToken();
        while (curr.kind != .EOF) {
            try self.declaration();
            curr = try self.peekToken();
        }
    }

    pub fn declaration(self: *@This()) ParseError!void {
        var curr = try self.takeToken();
        switch (curr.kind) {
            .VAR => try self.varDeclaration(),
            else => {
                self.peek = curr;
                try self.statement();
            },
        }
    }

    pub fn statement(self: *@This()) ParseError!void {
        var curr = try self.takeToken();
        switch (curr.kind) {
            .PRINT => try self.printStatement(),
            else => {
                self.peek = curr;
                try self.expressionStatement();
            },
        }
    }

    pub fn varDeclaration(self: *@This()) ParseError!void {
        self.peek = null;
        const nameTok = try self.takeToken();
        if (nameTok.kind != .IDENT) {
            return error.ExpectedIdent;
        }
        const eqTok = try self.takeToken();
        switch (eqTok.kind) {
            .EQUAL => try self.expression(),
            .SEMICOLON => {
                try self.chk.addOp(.NIL);
                try self.defineVariable(nameTok);
                return;
            },
            else => return ParseError.ExpectedEquals,
        }
        try self.consume(.SEMICOLON, error.ExpectedSemicolon);
        try self.defineVariable(nameTok);
    }

    pub fn defineVariable(self: *@This(), tok: Token) ParseError!void {
        const tname = self.scanner.tokenStr(tok);
        var tval = try Value.fromStr(tname, self.alloc);
        try self.chk.addConst(.DEFINE_GLOBAL, tval);
    }

    pub fn expression(self: *@This()) ParseError!void {
        try self.parsePrecedence(.ASSIGNMENT);
    }

    pub fn expressionStatement(self: *@This()) ParseError!void {
        try self.expression();
        try self.consume(.SEMICOLON, error.ExpectedSemicolon);
        try self.chk.addOp(.POP);
    }

    pub fn printStatement(self: *@This()) ParseError!void {
        self.peek = null;
        try self.expression();
        // -- find out what next token really is.
        //var pk = try self.peekToken();
        //std.debug.print("{}\n", .{pk});
        // --
        try self.consume(.SEMICOLON, error.ExpectedSemicolon);
        try self.chk.addOp(.PRINT);
    }

    pub fn parsePrecedence(self: *@This(), prec: Precedence) ParseError!void {
        var curr = try self.peekToken();
        const preFn: ParseFn = getRule(curr.kind).prefix orelse return error.ExpectedExpression;
        try preFn(self);
        curr = try self.peekToken();
        while (@enumToInt(prec) <= @enumToInt(getRule(curr.kind).precedence)) {
            const inFn: ParseFn = getRule(curr.kind).infix orelse return error.ExpectedInfix;
            try inFn(self);
            curr = try self.peekToken();
        }
    }

    pub fn literal(self: *@This()) ParseError!void {
        var curr = try self.takeToken();
        switch (curr.kind) {
            .FALSE => try self.chk.addOp(.FALSE),
            .TRUE => try self.chk.addOp(.TRUE),
            .NIL => try self.chk.addOp(.NIL),
            else => unreachable,
        }
    }

    pub fn grouping(self: *@This()) ParseError!void {
        self.peek = null;
        try self.expression();
        try self.consume(.RIGHT_PAREN, error.ExpectedRightParen);
    }

    pub fn unary(self: *@This()) ParseError!void {
        const op = try self.takeToken();
        try self.expression();
        switch (op.kind) {
            .MINUS => try self.chk.addOp(.NEGATE),
            .BANG => try self.chk.addOp(.NOT),
            else => return error.ExpectedMinus,
        }
    }

    pub fn binary(self: *@This()) ParseError!void {
        const op = try self.takeToken();
        const rule = getRule(op.kind);
        try self.parsePrecedence(rule.precedence.inc());
        switch (op.kind) {
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
        var curr = try self.takeToken();
        var s = self.scanner.tokenStr(curr);
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addConst(.CONSTANT, .{ .NUMBER = val });
    }

    pub fn string(self: *@This()) ParseError!void {
        var curr = try self.takeToken();
        var s_orig = self.scanner.tokenStr(curr);
        //remove quotes
        var val = try Value.fromStr(s_orig[1 .. s_orig.len - 1], self.alloc);
        try self.chk.addConst(.CONSTANT, val);
    }

    pub fn variable(self: *@This()) ParseError!void {
        try self.namedVariable();
    }
    pub fn namedVariable(self: *@This()) ParseError!void {
        var curr = try self.takeToken();
        assert(curr.kind == .IDENT);
        var name = self.scanner.tokenStr(curr);
        var val = try Value.fromStr(name, self.alloc);
        try self.chk.addConst(.GET_GLOBAL, val);
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
        .LEFT_PAREN => .{ .prefix = Parser.grouping, .infix = null },
        .MINUS => .{ .prefix = Parser.unary, .infix = Parser.binary, .precedence = .TERM },
        .PLUS => .{ .infix = Parser.binary, .precedence = .TERM },
        .STAR, .SLASH => .{ .infix = Parser.binary, .precedence = .FACTOR },
        .BANG => .{ .prefix = Parser.unary, .precedence = .NONE },
        .NUMBER => .{ .prefix = Parser.number },
        .FALSE, .TRUE, .NIL => .{ .prefix = Parser.literal, .precedence = .NONE },
        .GREATER, .LESS, .LESS_EQUAL, .GREATER_EQUAL => .{ .infix = Parser.binary, .precedence = .COMPARISON },
        .EQUAL_EQUAL => .{ .infix = Parser.binary, .precedence = .EQUALITY },
        .STRING => .{ .prefix = Parser.string },
        .IDENT => .{ .prefix = Parser.variable },
        else => .{},
    };
}

test "can compile something" {
    var alloc = std.testing.allocator;
    const s = "print \"hello\"";
    var ch = chunk.Chunk.init(alloc);
    defer ch.deinit(alloc);
    try compile(s, alloc, &ch);
}

test "rule table functions" {
    try expect(std.meta.eql(getRule(.LEFT_PAREN), ParseRule{ .prefix = Parser.grouping, .infix = null, .precedence = .NONE }));
}

test "compiles and runs" {
    var alloc = std.testing.allocator;
    var res = try compileAndRunExpression("4 + 5 - (3* 2)", alloc);
    try expectEqual(res, .{ .NUMBER = 3 });
}

test "compile and run bool" {
    var res = try compileAndRunExpression("!(3-3)", std.testing.allocator);
    try expectEqual(res, value.Value{ .BOOL = true });
}

test "bools equality" {
    var res = try compileAndRunExpression("(4 + 6 > 3 + 6)", std.testing.allocator);
    //std.debug.print("bools eq : {}", .{res});

    const v = value.Value{ .BOOL = true };
    try expect(try v.equal(res));
}

test "string equality" {
    var alloc = std.testing.allocator;
    var res = try compileAndRunExpression(
        \\"hello" == ("hel" + "lo")
    , alloc);
    //std.debug.print("HELLO  eq : {s}\n", .{res.asStr()});

    const v = value.Value{ .BOOL = true };
    try expect(try v.equal(res));
}
