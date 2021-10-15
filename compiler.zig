const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const std = @import("std");
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn compile(s: []const u8, ch: *chunk.Chunk) !void {
    _ = try Parser.init(s, ch);
}

const Parser = struct {
    prev: scanner.Token,
    curr: scanner.Token,
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

test "can compile something" {
    var gpa = GPAlloc{};
    const s = "print \"hello\"";
    var ch = chunk.Chunk.init(&gpa.allocator);
    try compile(s, &ch);
}
