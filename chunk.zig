const std = @import("std");
const AC = std.mem.Alloc;

pub const ValueType = enum(u8) {
    BOOL,
    NIL,
    NUMBER,
};

pub const Value = union(ValueType) {
    BOOL: bool,
    NIL: void,
    NUMBER: f64,

    pub fn as_bool(self: @This()) bool {
        return switch (self) {
            .BOOL => |b| b,
            .NUMBER => |n| n != 0,
            .NIL => false,
        };
    }
};

pub const OpCode = enum(u8) {
    RETURN,
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
};

pub const OpData = union(OpCode) {
    RETURN: void,
    CONSTANT: Value,
    NEGATE: void,
    ADD: void,
    SUB: void,
    MUL: void,
    DIV: void,
    NIL: void,
    FALSE: void,
    TRUE: void,
    NOT: void,
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
            OpData.CONSTANT => |c| {
                var pos: u8 = @intCast(u8, ch.consts.items.len);
                try ch.consts.append(c);
                try ch.ins.append(@enumToInt(od));
                try ch.ins.append(pos);
            },
            else => try ch.ins.append(@enumToInt(od)),
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
