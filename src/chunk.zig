const std = @import("std");
const AC = std.mem.Alloc;
const value = @import("value.zig");
const ByteIter = @import("util/byte_iter.zig").ByteIter;
const Value = value.Value;

pub const uSlot = u16;

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
    JUMP,
    JUMP_IF_FALSE,
};

pub const ChunkIter = struct {
    chunk: *Chunk,
    ins: ByteIter,
    pub fn init(ch: *Chunk) @This() {
        return ChunkIter{
            .chunk = ch,
            .ins = ByteIter.init(ch.ins.items),
        };
    }

    pub fn jump(self: *@This(), n: u16) !void {
        self.ins.n += @intCast(usize, n);
        if (self.ins.n >= self.chunk.ins.items.len) {
            return error.JumpOutOfBound;
        }
    }
    pub fn readOp(self: *@This()) ?OpCode {
        var b = self.ins.tryReadN(u8) orelse return null;
        return @intToEnum(OpCode, b);
    }

    pub fn readConst(self: *@This()) Value {
        var b = self.ins.readN(u16);
        return self.chunk.consts.items[b];
    }

    pub fn readSlot(self: *@This()) uSlot {
        return self.ins.readN(uSlot);
    }
    ///Jump distinct from slot incase one's representation changes
    pub fn readJump(self: *@This()) u16 {
        return self.ins.readN(uSlot);
    }
};

pub const Chunk = struct {
    consts: std.ArrayList(Value),
    ins: std.ArrayList(u8),
    lines: std.ArrayList(u8),

    pub fn init(alloc: *std.mem.Allocator) Chunk {
        return Chunk{
            .consts = std.ArrayList(Value).init(alloc.*),
            .ins = std.ArrayList(u8).init(alloc.*),
            .lines = std.ArrayList(u8).init(alloc.*),
        };
    }

    pub fn print(ch: *Chunk, w: anytype) void {
        var it = ChunkIter.init(ch);
        while (it.readOp()) |op| {
            switch (op) {
                .SET_LOCAL, .GET_LOCAL => {
                    var slot = it.readSlot();
                    w.print("{} : {}\n", .{ op, slot });
                },
                .SET_GLOBAL, .GET_GLOBAL, .CONSTANT, .DEFINE_GLOBAL => {
                    w.print("{} :", .{op});
                    const c = it.readConst();
                    try c.printTo(w);
                    w.print("\n", .{});
                },
                .JUMP, .JUMP_IF_FALSE => {
                    var target = it.readJump();
                    w.print("{} : {}\n", .{ op, target });
                },
                else => w.print("{}\n", .{op}),
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
        var pos: u16 = @intCast(u16, found orelse ch.consts.items.len);
        if (found == null) {
            try ch.consts.append(v);
        }
        try ch.ins.append(@enumToInt(op));
        try ch.addNumBytes(u16, pos);
    }

    pub fn addNumBytes(ch: *Chunk, comptime T: type, n: T) !void {
        var swp = @byteSwap(T, n);
        var bts = std.mem.toBytes(swp);
        for (bts) |b| {
            try ch.ins.append(b);
        }
    }

    pub fn addWithSlot(ch: *Chunk, op: OpCode, v: u16) !void {
        try ch.ins.append(@enumToInt(op));
        try ch.addNumBytes(u16, v);
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

    pub fn addJump(ch: *Chunk, op: OpCode) !usize {
        try ch.ins.append(@enumToInt(op));
        var res = ch.ins.items.len;
        try ch.ins.append(0xff);
        try ch.ins.append(0xff);
        return res;
    }

    pub fn patchJump(ch: *Chunk, from: usize) !void {
        // -2 accounts for the size of the jump op
        var jump = (ch.ins.items.len - from) - 2;
        if (jump >= 256 * 256) {
            return error.JumpTooBig;
        }
        //@byteSwap(T, n) marked in case byte order changes
        var bts = std.mem.toBytes(@intCast(u16, jump));
        ch.ins.items[from] = bts[1];
        ch.ins.items[from + 1] = bts[0];
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

test "byte conversion" {
    var n: u16 = 260;
    var bytes = std.mem.toBytes(n);
    try std.testing.expectEqual(bytes, [2]u8{ 4, 1 });
}
