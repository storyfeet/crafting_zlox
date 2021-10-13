const std = @import("std");
const chunk = @import("chunk.zig");

const OpCode = chunk.OpCode;

const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPAlloc{};

    var argIter = std.process.args();
    _ = argIter.skip();

    var numArgs: u8 = 0;
    while (argIter.next(&gpa.allocator)) |ae| {
        numArgs += 1;
        if (ae) |s| {
            std.debug.print("arg = {s}\n", .{s});
            //TODO run file
            defer gpa.allocator.free(s);
        } else |err| {
            return err;
        }
    }

    if (numArgs == 0) {
        std.debug.print("Zlox usage = ??\n", .{});
    }
}
