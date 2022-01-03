const std = @import("std");
const chunk = @import("chunk.zig");
const scanner = @import("scanner.zig");
const compiler = @import("compiler.zig");

const OpCode = chunk.OpCode;

const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPAlloc{};
    var alloc = gpa.allocator();

    var argIter = std.process.args();
    _ = argIter.skip();

    var numArgs: u8 = 0;
    while (argIter.next(alloc)) |ae| {
        numArgs += 1;
        if (ae) |s| {
            defer alloc.free(s);
            std.debug.print("Processing file : {s}\n\n\n", .{s});
            const fstr = try scanner.readFile(s, &alloc);
            try compiler.compileAndRunProgram(fstr, alloc);
        } else |err| {
            return err;
        }
    }

    if (numArgs == 0) {
        std.debug.print("Zlox usage = ??\n", .{});
    }
}
