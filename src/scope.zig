const std = @import("std");
const uSlot = chunk.uSlot;

pub const ScopeError = error{
    OutOfMemory,
};

const chunk = @import("chunk.zig");
const Local = struct {
    name: []const u8,
    depth: ?usize = null,
    isConst: bool,
};

const FoundLocal = struct {
    slot: uSlot,
    isC: bool,
};
pub const Scope = struct {
    const LList = std.ArrayListUnmanaged(Local);
    prev: ?*Scope,
    locals: LList,
    depth: usize,

    pub fn init(alloc: std.mem.Allocator) !*@This() {
        var res: *Scope = try alloc.create(Scope);
        res.locals = LList{};
        res.prev = null;
        res.depth = 0;
        return res;
    }

    pub fn parent(self: *@This(), alloc: std.mem.Allocator) ?*@This() {
        var p = self.prev;
        alloc.destory(self);
        return p;
    }

    pub fn incDepth(self: *@This()) void {
        self.depth += 1;
    }

    pub fn addLocal(self: *@This(), alloc: std.mem.Allocator, name: []const u8, isConst: bool) ScopeError!void {
        var loc = Local{
            .name = name,
            .depth = null,
            .isConst = isConst,
        };
        try self.locals.append(alloc, loc);
    }

    pub fn decDepth(self: *@This()) u8 {
        var res: u8 = 0;
        while (self.locals.items.len > 0) : (self.locals.items.len -= 1) {
            var dp = self.locals.items[self.locals.items.len - 1].depth;
            if (dp) |d| {
                if (d < self.depth) {
                    return res;
                }
            }
            res += 1;
        }
        return res;
    }

    pub fn initDepth(self: *@This()) void {
        self.locals.items[self.locals.items.len - 1].depth = self.depth;
    }

    pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        if (self.prev) |p| {
            p.deinit(alloc);
        }
        self.locals.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn levelLocalExists(self: *@This(), s: []const u8) bool {
        if (self.locals.items.len == 0) return false;
        var i = self.locals.items.len - 1;
        while (true) : (i -= 1) {
            var loc = &self.locals.items[i];
            if (loc.depth) |dp| {
                if (dp < self.depth) {
                    return false;
                }
            }
            if (std.mem.eql(u8, loc.name, s)) {
                std.debug.print("EQL {s} : {s}\n\n", .{ loc.name, s });
                return true;
            }
            if (i == 0) return false;
        }
    }

    pub fn findLocal(self: *@This(), s: []const u8) ?FoundLocal {
        if (self.locals.items.len == 0) return null;
        var i = self.locals.items.len - 1;
        while (true) : (i -= 1) {
            if (std.mem.eql(u8, self.locals.items[i].name, s)) {
                return FoundLocal{ .slot = @intCast(uSlot, i), .isC = self.locals.items[i].isConst };
            }
            if (i == 0) return null;
        }
    }
};
