const std = @import("std");
const AC = std.mem.Alloc;

pub const Value = f64;

pub const OpCode = enum(u8) {
    RETURN,
    CONSTANT,
    NEGATE,
};

pub const OpData = union(OpCode) {
    RETURN: void,
    CONSTANT: Value,
    NEGATE: void,
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

    pub fn addOp(ch: *Chunk, od: OpData) !void {
        switch (od) {
            OpData.RETURN, OpData.NEGATE => try ch.ins.append(@enumToInt(od)),
            OpData.CONSTANT => |c| {
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
