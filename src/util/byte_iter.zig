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

    pub fn readByte(self: *@This()) u8 {
        const b = self.list[self.n];
        self.n += 1;
        return b;
    }

    pub fn peakByte(self: *@This()) u8 {
        return self.list[n];
    }

    pub fn tryReadByte(self: *@This()) ?u8 {
        if (self.n >= self.list.len) return null;
        return self.readByte();
    }

    pub fn read16(self: *@This()) u16 {
        //const res = @ptrCast(*u16, &self.list[self.n]);
        var res: u16 = 0;
        res += self.list[self.n];
        res <<= 8;
        res += self.list[self.n + 1];
        self.n += 2;
        return res;
    }

    pub fn readN(self: *@This(), comptime n: u8) un {}

    pub fn read32(self: *@This()) u32 {
        const res = @ptrCast(*u32, @alignCast(4, &self.list[self.n]));
        self.n += 4;
        return res.*;
    }
};

test "CanRead16" {
    var list = [_]u8{ 3, 3, 1, 2, 1, 1, 1 };
    var it = ByteIter.init(&list);
    try expectEqual(it.readByte(), 3);
    try expectEqual(it.read16(), 256 * 3 + 1);
    try expectEqual(it.read32(), 100);
}
