const std = @import("std");
const expectEqual = std.testing.expectEqual;

const ByteIter = struct {
    list: []u8,
    n: usize,

    pub fn init(list: []u8) @This() {
        return ByteIter{
            .list = list,
            .n = 0,
        };
    }

    pub fn readN(self: *@This(), comptime T: type) T {
        const res = @ptrCast(*align(1) T, &self.list[self.n]);
        self.n += @sizeOf(T);
        return @byteSwap(T, res.*);
    }

    pub fn tryReadN(self: *@This(), comptime T: type) ?T {
        if (self.n + @sizeOf(T) > self.list.len) return null;
        return self.readN(T);
    }

    pub fn peekN(self: *@This(), comptime T: type) T {
        const res = @ptrCast(*align(1) T, &self.list[self.n]);
        return @byteSwap(T, res.*);
    }

    pub fn tryPeekN(self: *@This(), comptime T: type) ?T {
        if (self.n + @sizeOf(T) > self.list.len) return null;
        return peekN(T);
    }
};

test "CanRead16" {
    var list = [_]u8{ 3, 3, 1, 0, 1, 2, 1 };
    var it = ByteIter.init(&list);
    try expectEqual(it.readN(u8), 3);

    var n = it.readN(u16);
    try expectEqual(n, 256 * 3 + 1);
    var n32 = it.readN(u32);
    try expectEqual(n32, 256 * 256 + 256 * 2 + 1);
}
