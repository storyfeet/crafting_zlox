const std = @import("std");

pub const ValueType = enum(u8) {
    BOOL,
    NIL,
    NUMBER,
    OBJ,
};

pub const ValueError = error{
    CompareError,
};

pub const Value = union(ValueType) {
    BOOL: bool,
    NIL: void,
    NUMBER: f64,
    OBJ: *Obj,

    pub fn as_bool(self: @This()) bool {
        return switch (self) {
            .BOOL => |b| b,
            .NUMBER => |n| n != 0,
            .NIL => false,
            .OBJ => true,
        };
    }

    pub fn fromStr(s_orig: []const u8, alloc: std.mem.Allocator) !@This() {
        const s_copy: []u8 = try alloc.alloc(u8, s_orig.len);
        std.mem.copy(u8, s_copy, s_orig);
        const ob = try alloc.create(Obj);
        ob.* = Obj{ .data = .{ .STR = s_copy } };
        return Value{ .OBJ = ob };
    }

    pub fn deinit(this: @This(), alloc: std.mem.Allocator) void {
        switch (this) {
            .OBJ => |v| v.deinit(alloc),
            else => {},
        }
    }

    pub fn equal(a: @This(), b: @This()) ValueError!bool {
        switch (a) {
            .BOOL => |a_bool| switch (b) {
                .BOOL => |b_bool| return a_bool == b_bool,
                else => return false,
            },
            .NIL => return .NIL == b,
            .NUMBER => |a_num| switch (b) {
                .NUMBER => |b_num| return a_num == b_num,
                else => return false,
            },
            .OBJ => |a_p| switch (b) {
                .OBJ => |b_p| return a_p.equal(b_p),
                else => return false,
            },
        }
    }

    pub fn greater(a: @This(), b: @This()) ValueError!bool {
        switch (a) {
            .BOOL => return false,
            .NIL => return false,
            .NUMBER => |a_num| switch (b) {
                .NUMBER => |b_num| return a_num > b_num,
                else => return error.CompareError,
            },
            .OBJ => |a_p| switch (b) {
                .OBJ => |b_p| return a_p.greater(b_p),
                else => return error.CompareError,
            },
        }
    }

    pub fn less(a: @This(), b: @This()) ValueError!bool {
        return greater(b, a);
    }
    pub fn asStr(self: *const @This()) ?[]const u8 {
        switch (self.*) {
            .OBJ => |o| switch (o.data) {
                .STR => |s| return s,
                //else => return null,
            },
            else => return null,
        }
    }

    pub fn printTo(self: @This(), writer: anytype) !void {
        switch (self) {
            .OBJ => |o| try o.printTo(writer),
            .BOOL => |t| writer.print("{}", .{t}),
            .NIL => writer.print("nil", .{}),
            .NUMBER => |n| writer.print("{}", .{n}),
        }
    }
};

pub const ObjType = enum(u4) {
    STR,
};

pub const Obj = struct {
    meta: u8 = 0, //TODO RC etc
    data: ObjData,

    pub fn equal(a: *@This(), b: *@This()) !bool {
        const atype: ObjType = a.data;
        if (atype != b.data) return false;

        _ = switch (a.data) {
            .STR => |a_s| switch (b.data) {
                .STR => |b_s| return std.mem.eql(u8, a_s, b_s),
                // else => .{},
            },
            //else => .{},
        };

        return @ptrToInt(a) == @ptrToInt(b);
    }

    pub fn greater(_: *@This(), _: *@This()) !bool {
        @panic("Greater not implemented");
        //return false; //TODO
    }

    pub fn printTo(this: *@This(), writer: anytype) !void {
        switch (this.data) {
            .STR => |s| writer.print("{s}", .{s}),
        }
    }

    pub fn deinit(this: *@This(), alloc: std.mem.Allocator) void {
        this.data.deinit(alloc);
        alloc.destroy(this);
    }
};

pub const ObjData = union(ObjType) {
    STR: []u8,

    fn deinit(this: @This(), alloc: std.mem.Allocator) void {
        switch (this) {
            .STR => |s| alloc.free(s),
        }
    }
};

pub fn concatStr(a: []const u8, b: []const u8, alloc: std.mem.Allocator) ![]u8 {
    var new_len = a.len + b.len;
    var res: []u8 = try alloc.alloc(u8, new_len);
    std.mem.copy(u8, res[0..], a);
    std.mem.copy(u8, res[a.len..], b);
    return res;
}
