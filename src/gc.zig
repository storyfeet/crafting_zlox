//const value = @import("value.zig");
const std = @import("std");
const ArrayList = std.ArrayList;

//const Obj = value.Obj;

/// A DropList holds a set of items out of order, but adds and removes quickly by using a list of empty entries.
pub fn DropList(comptime T: type) type {
    return struct {
        data: ArrayList(?T),
        drops: ArrayList(usize),

        pub fn init(alloc: *std.mem.Allocator) @This() {
            return .{
                .data = ArrayList(?T).init(alloc),
                .drops = ArrayList(usize).init(alloc),
            };
        }

        pub fn deinit(this: @This()) void {
            this.data.deinit();
            this.drops.deinit();
        }

        pub fn push(this: *@This(), t: T) !void {
            if (this.drops.items.len > 0) {
                var v = this.drops.items[this.drops.items.len - 1];
                this.drops.items.len -= 1;
                this.data.items[v] = t;
            } else {
                try this.data.append(t);
            }
        }

        pub fn drop(this: *@This(), n: usize) !?T {
            if (this.data.items.len < n) return null;
            if (this.data.items[n]) |v| {
                try this.drops.append(n);
                this.data.items[n] = null;
                return v;
            }
            return null;
        }
    };
}

test "Drop list does thing" {
    var dl = DropList(i32).init(std.testing.allocator);
    try dl.push(3);
    try dl.push(4);
    try dl.push(5);
    _ = try dl.drop(1);
    try dl.push(6);

    try std.testing.expect(dl.data.items.len == 3);
    try std.testing.expect((dl.data.items[1] orelse 0) == 6);

    dl.deinit();
}
