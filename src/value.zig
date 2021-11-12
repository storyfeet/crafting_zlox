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
            .OBJ => false,
        };
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

    pub fn greater(a: *@This(), b: *@This()) !bool {
        return false; //TODO
    }
};

pub const ObjData = union(ObjType) {
    STR: []u8,
};

pub fn concatStr(a: []const u8, b: []const u8, alloc: *std.mem.Allocator) ![]u8 {
    var new_len = a.len + b.len;
    var res: []u8 = try alloc.alloc(u8, new_len);
    std.mem.copy(u8, res[0..], a);
    std.mem.copy(u8, res[a.len..], b);
    return res;
}
