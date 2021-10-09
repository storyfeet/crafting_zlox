const std = @import("std");
const AC = std.mem.Alloc;

pub const OpCode = enum(u8) {
    OP_RETURN,
};

pub const Chunk = struct {
    consts: std.ArrayList(u8),
    ins: std.ArrayList(u8),
    lines: std.ArrayList(u8),

    pub fn init(alloc: *std.mem.Alloc) Chunk {
        return Chunk{
            .consts = std.ArrayList(u8).init(alloc),
            .ins = std.ArrayList(u8).init(alloc),
            .lines = std.ArrayList(u8).init(alloc),
        };
    }

    pub fn deinit(ch: Chunk) void {
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
        OpCode.OP_RETURN => {
            std.debug.print("OP_RETURN\n", .{});
            return offset + 1;
        },
    }
}
