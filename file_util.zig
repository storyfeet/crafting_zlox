const std = @import("std");
const fs = std.fs;

pub fn readFile(path: [*]const u8, alloc: *std.mem.Allocator) ![]u8 {
    const f: fs.File = try fs.cwd().openFile(path, .{ .read = true });
    defer f.close();

    return try f.readToEndAlloc(alloc);
}
