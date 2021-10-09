const std = @import("std");
const chunk = @import("chunk.zig");

const OpCode = chunk.OpCode;

const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPAlloc{};
    std.debug.print("Zlox usage = ??\n", .{});
    chunk.do_thing();
    var ar = std.ArrayList(u8).init(&gpa.allocator);
    defer ar.deinit();
    try ar.append(@bitCast(u8, OpCode.OP_RETURN));
    try ar.append(@bitCast(u8, OpCode.OP_RETURN));

    chunk.dissasemble(ar.items);
}
