const std = @import("std");
const AC = std.mem.Alloc;
const value = @import("value.zig");
const ByteIter = @import("util/byte_iter.zig").ByteIter;
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
    GET_LOCAL,
    SET_LOCAL,
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

    pub fn print(ch: *Chunk, w: anytype) void {
        const Mode = enum {
            Ins,
            Const,
            Slot,
        };
        var mode = Mode.Ins;
        for (ch.ins.items) |ins| {
            switch (mode) {
                .Ins => {
                    var oc = @intToEnum(OpCode, ins);
                    switch (oc) {
                        .SET_LOCAL, .GET_LOCAL => {
                            w.print("{} :", .{oc});
                            mode = .Slot;
                        },
                        .SET_GLOBAL, .GET_GLOBAL, .CONSTANT, .DEFINE_GLOBAL => {
                            w.print("{} :", .{oc});
                            mode = .Const;
                        },
                        else => w.print("{}\n", .{oc}),
                    }
                },
                .Const => {
                    try ch.consts.items[ins].printTo(w);
                    w.print("\n", .{});
                    mode = .Ins;
                },
                .Slot => {
                    w.print("{}\n", .{ins});
                    mode = .Ins;
                },
            }
        }
    }

    pub fn addConst(ch: *Chunk, op: OpCode, v: Value) !void {
        var found: ?usize = null;
        for (ch.consts.items) |cv, i| {
            if (cv.equal(v) catch false) {
                found = i;
                break;
            }
        }
        var pos: u8 = @intCast(u8, found orelse ch.consts.items.len);
        if (found == null) {
            try ch.consts.append(v);
        }
        try ch.ins.append(@enumToInt(op));
        try ch.ins.append(pos);
    }

    pub fn addWithByte(ch: *Chunk, op: OpCode, v: u8) !void {
        try ch.ins.append(@enumToInt(op));
        try ch.ins.append(v);
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
