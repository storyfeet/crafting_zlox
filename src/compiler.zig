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
    TooManyLocalVariables,
    TooManyCompileErrors,
    LocalAlreadyExists,
} || scanner.ScanError;

pub fn compileAndRunProgram(s: []const u8, a: *std.mem.Allocator) !void {
    var ch = chunk.Chunk.init(a);
    defer ch.deinit(a);
    var p: Parser = try Parser.init(s, a, &ch);
    defer p.deinit();
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
    defer p.deinit();
    try p.expression();
    try ch.addOp(.RETURN);
    var theVm = vm.VM.init(&ch, a);
    defer theVm.deinit();
    return try theVm.run();
}

const ParseErrorData = struct {
    err: ParseError,
    token: Token,
    pub fn print(self: @This()) void {
        std.debug.print("Error {} : at {}\n\n", .{ self.err, self.token });
    }
};

const Local = struct {
    name: []const u8,
    depth: usize,
};

//BOOK - Compiler,
const Scope = struct {
    prev: ?*Scope,
    locals: [256]Local,
    count: u8,
    depth: usize,
    pub fn init(d: usize, alloc: *std.mem.Allocator) !*@This() {
        var res: *Scope = try alloc.create(Scope);
        res.prev = null;
        res.count = 0;
        res.depth = d;
        return res;
    }

    pub fn parent(self: *@This(), alloc: *std.mem.Allocator) ?*@This() {
        var p = self.prev;
        alloc.destory(self);
        return p;
    }

    pub fn incDepth(self: *@This()) void {
        self.depth += 1;
    }

    pub fn addLocal(self: *@This(), name: []const u8) ParseError!void {
        if (self.count == 255) return error.TooManyLocalVariables;
        var loc = &self.locals[self.count];
        loc.name = name;
        loc.depth = self.depth;
        self.count += 1;
    }

    pub fn decDepth(self: *@This()) void {
        self.depth -= 1;
    }
    pub fn deinit(self: *@This(), alloc: *std.mem.Allocator) void {
        if (self.prev) |p| {
            p.deinit(alloc);
        }
        alloc.destroy(self);
    }

    pub fn levelLocalExists(self: *@This(), s: []const u8) bool {
        var i = self.count - 1;
        while (i >= 0) : (i -= 1) {
            var loc = &self.locals[i];
            if (loc.depth != -1 and loc.depth < self.depth) {
                return false;
            }
            if (std.mem.eql(u8, loc.name, s)) {
                return true;
            }
        }
        return false;
    }

    pub fn findLocal(self: *@This(), s: []const u8) ?u8 {
        var i = self.count - 1;
        while (i >= 0) : (i -= 1) {
            if (std.mem.eql(u8, &self.locals[i], s)) {
                return i;
            }
        }
        return null;
    }
};

const Parser = struct {
    peek: ?Token,
    scanner: scanner.Tokenizer,
    chk: *chunk.Chunk,
    alloc: *std.mem.Allocator,
    errors: std.ArrayList(ParseErrorData),
    scope: *Scope,

    pub fn err(self: *@This(), e: ParseError) ParseError {
        const edata = ParseErrorData{
            .err = e,
            .token = self.takeToken() catch |e3| return e3,
        };
        edata.print();
        self.errors.append(edata) catch |e2| return e2;
        return e;
    }

    pub fn init(s: []const u8, alloc: *std.mem.Allocator, ch: *chunk.Chunk) !@This() {
        var sc = scanner.Tokenizer.init(s);
        return Parser{
            .peek = null,
            .scanner = sc,
            .chk = ch,
            .alloc = alloc,
            .errors = std.ArrayList(ParseErrorData).init(alloc),
            .scope = try Scope.init(0, alloc),
        };
    }

    pub fn deinit(self: @This()) void {
        self.errors.deinit();
        self.scope.deinit(self.alloc);
    }

    pub fn peekToken(this: *@This()) ParseError!Token {
        if (this.peek) |p| {
            return p;
        }
        var nx = this.scanner.nextToken() catch |e| return this.err(e);
        this.peek = nx;
        return nx;
    }

    pub fn takeToken(this: *@This()) ParseError!Token {
        if (this.peek) |p| {
            this.peek = null;
            return p;
        }
        return this.scanner.nextToken() catch |e| return this.err(e);
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
            self.declaration() catch |e| {
                try self.endErrLine();
            };
            curr = try self.peekToken();
        }
        if (self.errors.items.len > 0) {
            return error.TooManyCompileErrors;
        }
    }

    pub fn endErrLine(self: *@This()) ParseError!void {
        while (true) {
            var curr = try self.takeToken();
            switch (curr.kind) {
                .EOF, .SEMICOLON => return,
                else => {},
            }
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
            .LEFT_BRACE => {
                self.scope.incDepth();
                try self.block();
                self.scope.decDepth();
            },
            else => {
                self.peek = curr;
                try self.expressionStatement();
            },
        }
    }

    pub fn block(self: *@This()) ParseError!void {
        self.peek = null;
        while (true) {
            var curr = try self.peekToken();
            switch (curr.kind) {
                .RIGHT_BRACE => return,
                .EOF => return self.err(error.UnexpectedEOF),
                else => try self.declaration(),
            }
        }
    }

    pub fn varDeclaration(self: *@This()) ParseError!void {
        self.peek = null;
        const nameTok = try self.takeToken();
        if (nameTok.kind != .IDENT) {
            return self.err(error.ExpectedIdent);
        }
        const eqTok = try self.takeToken();
        switch (eqTok.kind) {
            .EQUAL => try self.expression(),
            .SEMICOLON => {
                try self.chk.addOp(.NIL);
                try self.defineVariable(nameTok);
                return;
            },
            else => return self.err(ParseError.ExpectedEquals),
        }
        try self.consume(.SEMICOLON, error.ExpectedSemicolon);
        try self.defineVariable(nameTok);
    }

    pub fn defineVariable(self: *@This(), tok: Token) ParseError!void {
        const tname = self.scanner.tokenStr(tok);
        var tval = try Value.fromStr(tname, self.alloc);
        if (self.scope.depth > 0) {
            self.scope.addLocal(tname) catch |e| return self.err(e);
            if (self.scope.levelLocalExists(tname)) {
                return self.err(error.LocalAlreadyExists);
            }
        }
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
        const canAssign = @enumToInt(prec) <= @enumToInt(Precedence.ASSIGNMENT);
        const preFn: ParseFn = getRule(curr.kind).prefix orelse return self.err(error.ExpectedExpression);
        try preFn(self, canAssign);
        curr = try self.peekToken();
        while (@enumToInt(prec) <= @enumToInt(getRule(curr.kind).precedence)) {
            const inFn: ParseFn = getRule(curr.kind).infix orelse return self.err(error.ExpectedInfix);
            try inFn(self, canAssign);
            curr = try self.peekToken();
        }
    }

    pub fn literal(self: *@This(), canAssign: bool) ParseError!void {
        var curr = try self.takeToken();
        switch (curr.kind) {
            .FALSE => try self.chk.addOp(.FALSE),
            .TRUE => try self.chk.addOp(.TRUE),
            .NIL => try self.chk.addOp(.NIL),
            else => unreachable,
        }
    }

    pub fn grouping(self: *@This(), canAssign: bool) ParseError!void {
        self.peek = null;
        try self.expression();
        try self.consume(.RIGHT_PAREN, error.ExpectedRightParen);
    }

    pub fn unary(self: *@This(), canAssign: bool) ParseError!void {
        const op = try self.takeToken();
        try self.expression();
        switch (op.kind) {
            .MINUS => try self.chk.addOp(.NEGATE),
            .BANG => try self.chk.addOp(.NOT),
            else => return self.err(error.ExpectedMinus),
        }
    }

    pub fn binary(self: *@This(), canAssign: bool) ParseError!void {
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
            else => return self.err(error.ExpectedMathOp),
        }
    }

    pub fn number(self: *@This(), canAssign: bool) ParseError!void {
        var curr = try self.takeToken();
        var s = self.scanner.tokenStr(curr);
        var val = try std.fmt.parseFloat(f64, s);
        try self.chk.addConst(.CONSTANT, .{ .NUMBER = val });
    }

    pub fn string(self: *@This(), canAssign: bool) ParseError!void {
        var curr = try self.takeToken();
        var s_orig = self.scanner.tokenStr(curr);
        //remove quotes
        var val = try Value.fromStr(s_orig[1 .. s_orig.len - 1], self.alloc);
        try self.chk.addConst(.CONSTANT, val);
    }

    pub fn variable(self: *@This(), canAssign: bool) ParseError!void {
        try self.namedVariable(canAssign);
    }

    pub fn namedVariable(self: *@This(), canAssign: bool) ParseError!void {
        var curr = try self.takeToken();
        assert(curr.kind == .IDENT);
        var name = self.scanner.tokenStr(curr);
        var val = try Value.fromStr(name, self.alloc);
        var eqTok = try self.peekToken();
        if (eqTok.kind == .EQUAL and canAssign) {
            self.peek = null;
            try self.expression();
            if (self.scope.findLocal(name)) |loc| {
                try self.chk.addConst(.SET_LOCAL, loc);
            } else try self.chk.addConst(.SET_GLOBAL, val);
        } else {
            if (self.scope.findLocal(name)) |loc| {
                try self.chk.addConst(.GET_LOCAL, loc);
            } else try self.chk.addConst(.GET_GLOBAL, val);
        }
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

const ParseFn = fn (*Parser, bool) ParseError!void;

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
