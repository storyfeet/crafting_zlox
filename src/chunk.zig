const std = @import("std");
const AC = std.mem.Alloc;
const value = @import("value.zig");
const Value = value.Value;

pub const OpCode = enum(u8) {
    RETURN,
    EXIT,
    CONSTANT,
    NEGATE,
    ADD,
    SUB,
    MUL,
    DIV,
    NIL,
    FALSE,
    TRUE,
    NOT,
    EQUAL,
    GREATER,
    LESS,
    PRINT,
    POP,
    DEFINE_GLOBAL,
    GET_GLOBAL,
    SET_GLOBAL,
};

pub const Chunk = struct {
    consts: std.ArrayList(Value),
    ins: std.ArrayList(u8),
    lines: std.ArrayList(u8),

    pub fn init(alloc: *std.mem.Allocator) Chunk {
        return Chunk{
            .consts = std.ArrayList(Value).init(alloc),
            .ins = std.ArrayList(u8).init(alloc),
            .lines = std.ArrayList(u8).init(alloc),
        };
    }

    pub fn addConst(ch: *Chunk, op: OpCode, v: Value) !void {
        var pos: u8 = @intCast(u8, ch.consts.items.len);
        try ch.consts.append(v);
        try ch.ins.append(@enumToInt(op));
        try ch.ins.append(pos);
    }

    pub fn addOp(ch: *Chunk, od: OpCode) !void {
        try ch.ins.append(@enumToInt(od));
    }

    pub fn deinit(ch: Chunk, alloc: *std.mem.Allocator) void {
        for (ch.consts.items) |c| {
            c.deinit(alloc);
        }
        ch.consts.deinit();
        ch.ins.deinit();
        ch.lines.deinit();
    }
};

pub fn do_thing() void {
    std.debug.print("Thing Doing\n", .{});
}

pub fn dissasemble(codes: []const u8) void {
    var pos: usize = 0;
    while (pos < codes.len) {
        pos = dissasemble_instruction(codes, pos);
    }
}

fn dissasemble_instruction(codes: []const u8, offset: usize) usize {
    std.debug.print("{x} ", .{offset});
    switch (@intToEnum(OpCode, codes[offset])) {
        OpCode.RETURN => {
            std.debug.print("RETURN\n", .{});
            return offset + 1;
        },
        OpCode.CONSTANT => {
            std.debug.print("CONSTANT\n", .{});
            return offset + 2;
        },
    }
}
