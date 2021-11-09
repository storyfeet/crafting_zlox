pub const ValueType = enum(u8) {
    BOOL,
    NIL,
    NUMBER,
};

pub const Value = union(ValueType) {
    BOOL: bool,
    NIL: void,
    NUMBER: f64,

    pub fn as_bool(self: @This()) bool {
        return switch (self) {
            .BOOL => |b| b,
            .NUMBER => |n| n != 0,
            .NIL => false,
        };
    }

    pub fn equal(a: @This(), b: @This()) bool {
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
        }
    }

    pub fn greater(a: @This(), b: @This()) bool {
        switch (a) {
            .BOOL => return false,
            .NIL => return false,
            .NUMBER => |a_num| switch (b) {
                .NUMBER => |b_num| return a_num > b_num,
                else => return false,
            },
        }
    }

    pub fn less(a: @This(), b: @This()) bool {
        return greater(b, a);
    }
};

pub const ObjType = enum(u4) {};

pub const Obj = union(ObjType) {};

pub const ObjString = struct {};
