//const value = @import("value.zig");
const std = @import("std");
const ArrayList = std.ArrayList;

//const Obj = value.Obj;

/// A DropList holds a set of items out of order, but adds and removes quickly by using a list of empty entries.
///
pub fn DropList(comptime T: type) type {
    return struct {
        data: ArrayList(?T),
        drops: ArrayList(usize),
        len: usize,

        pub fn init(alloc: *std.mem.Allocator) @This() {
            return .{
                .data = ArrayList(?T).init(alloc),
                .drops = ArrayList(usize).init(alloc),
                .len = 0,
            };
        }

        pub fn deinit(this: *@This(), alloc: *std.mem.Allocator, destroyer: fn (T, *std.mem.Allocator) void) void {
            var it = this.iter();
            while (it.next()) |nx| {
                destroyer(nx, alloc);
            }

            this.data.deinit();
            this.drops.deinit();
        }

        pub fn push(this: *@This(), t: T) !void {
            if (this.drops.items.len > 0) {
                var v = this.drops.items[this.drops.items.len - 1];
                this.drops.items.len -= 1;
                this.data.items[v] = t;
                this.len += 1;
            } else {
                try this.data.append(t);
                this.len += 1;
            }
        }

        pub fn drop(this: *@This(), n: usize) !?T {
            if (this.data.items.len < n) return null;
            if (this.data.items[n]) |v| {
                try this.drops.append(n);
                this.data.items[n] = null;
                this.len -= 1;
                return v;
            }
            return null;
        }

        /// Keep if f returns true, if returns false, should deinit the item
        pub fn filter(this: *@This(), comptime f: fn (T) bool) !u32 {
            var dropped: u32 = 0;
            for (this.data.items) |v_op, n| {
                if (v_op) |v| {
                    if (!f(v)) {
                        _ = try this.drop(n);
                        dropped += 1;
                    }
                }
            }
            return dropped;
        }

        pub fn iter(this: *@This()) DropListIter(T) {
            return DropListIter(T){ .dl = this, .n = 0 };
        }
        pub fn noDestroy(_: T, _b: *std.mem.Allocator) void {}
    };
}

pub fn DropListIter(comptime T: type) type {
    return struct {
        dl: *DropList(T),
        n: usize,

        pub fn next(this: *@This()) ?T {
            while (true) {
                if (this.n >= this.dl.data.items.len) return null;
                if (this.dl.data.items[this.n]) |it| {
                    this.n += 1;
                    return it;
                }
                this.n += 1;
            }
        }
    };
}

fn test_gt5(n: i32) bool {
    return n > 5;
}

test "Drop list does thing" {
    var dl = DropList(i32).init(std.testing.allocator);
    defer dl.deinit(std.testing.allocator, DropList(i32).noDestroy);
    try dl.push(3);
    try dl.push(7);
    try dl.push(4);
    try dl.push(5);
    _ = try dl.drop(2);
    try dl.push(6);

    try std.testing.expect(dl.data.items.len == 4);
    try std.testing.expect((dl.data.items[2] orelse 0) == 6);

    var dropped = try dl.filter(test_gt5);

    try std.testing.expect(dropped == 2);
    try std.testing.expect(dl.len == 2);
    //Items are added in place, and not moved
    try std.testing.expect((dl.data.items[1] orelse 0) == 7);
    //added where item was dropped
    try std.testing.expect((dl.data.items[2] orelse 0) == 6);

    var it = dl.iter();
    try std.testing.expect((it.next() orelse 0) == 7);
    try std.testing.expect((it.next() orelse 0) == 6);
}
