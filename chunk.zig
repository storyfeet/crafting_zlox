const std = @import("std");
const AC = std.mem.Alloc;

pub const Value = u16;

pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
};

pub const OpData = union(OpCode) {
    OP_RETURN: void,
    OP_CONSTANT: u8,
};

pub const Chunk = struct {
    consts: std.ArrayList(u8),
    ins: std.ArrayList(u8),
    lines: std.ArrayList(Value),

    pub fn init(alloc: *std.mem.Allocator) Chunk {
        return Chunk{
            .consts = std.ArrayList(u8).init(alloc),
            .ins = std.ArrayList(u8).init(alloc),
            .lines = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn addOp(ch: *Chunk, od: OpData) !void {
        switch (od) {
            OpData.OP_RETURN => try ch.ins.append(@enumToInt(od)),
            OpData.OP_CONSTANT => |c| {
                var pos: u8 = @intCast(u8, ch.consts.items.len);
                try ch.consts.append(c);
                try ch.ins.append(@enumToInt(od));
                try ch.ins.append(pos);
            },
        }
    }

    pub fn deinit(ch: Chunk) void {
        ch.consts.deinit();
        ch.ins.deinit();
        ch.lines.deinit();
    }

    pub fn run(ch: *Chunk) void {}
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
        OpCode.OP_RETURN => {
            std.debug.print("OP_RETURN\n", .{});
            return offset + 1;
        },
        OpCode.OP_CONSTANT => {
            std.debug.print("OP_CONSTANT\n", .{});
            return offset + 2;
        },
    }
}
