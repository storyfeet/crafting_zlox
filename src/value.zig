pub const ValueType = enum(u8) {
    BOOL,
    NIL,
    NUMBER,
    OBJ,
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
            .OBJ => |a_p| switch (b) {
                .OBJ => |b_p| return a_p.equal(b_p),
                else => return false,
            },
        }
    }

    pub fn greater(a: @This(), b: @This()) !bool {
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

    pub fn less(a: @This(), b: @This()) bool {
        return greater(b, a);
    }
};

pub const ObjType = enum(u4) {
    STR,
};

pub const Obj = struct {
    meta: u8, //TODO RC etc
    data: ObjData,

    pub fn equal(a: *@This(), b: *@This()) bool {
        return false; //TODO
    }

    pub fn greater(a: *@This(), b: *@This()) !bool {
        return false; //TODO
    }
};

pub const ObjData = union(ObjType) {
    STR: []u8,
};
