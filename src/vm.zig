const std = @import("std");
const chunk = @import("chunk.zig");
const conf = @import("config.zig");
const value = @import("value.zig");
//const gc = @import("gc.zig");
const droplist = @import("util/droplist.zig");
const DropList = droplist.DropList;

const OpCode = chunk.OpCode;
const OpData = chunk.OpData;
const Value = value.Value;
const Obj = value.Obj;
const ObjData = value.ObjData;
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

const VarMap = std.ArrayHashMap([]const u8, Value, std.array_hash_map.StringContext, false);

pub const VMError = error{
    COMPILE_ERROR,
    RUN_ERROR,
    MATH_ON_NON_NUMBER,
    NON_STRING_GLOBAL,
    GLOBAL_NON_EXISTENT,
    GLOBAL_NON_EXISTENT_NOT_SET,
    JumpOutOfBound,
    OutOfMemory,
    NegatingObject,
} || value.ValueError;

pub const VM = struct {
    ip: usize,
    chunki: chunk.ChunkIter,
    stack: std.ArrayList(Value),
    freelist: DropList(*Obj),
    alloc: *std.mem.Allocator,
    globals: VarMap,
    pub fn init(ch: *chunk.Chunk, alloc: *std.mem.Allocator) VM {
        return VM{
            .chunki = chunk.ChunkIter.init(ch),
            .ip = 0,
            .stack = std.ArrayList(Value).init(alloc),
            .alloc = alloc,
            .freelist = DropList(*Obj).init(alloc),
            .globals = VarMap.init(alloc),
        };
    }

    pub fn run(
        self: *VM,
    ) VMError!Value {
        while (self.chunki.readOp()) |op| {
            switch (op) {
                .EXIT => {
                    return Value{ .NUMBER = 0 };
                },
                .RETURN => {
                    var cval = self.readStack();
                    std.debug.print("RETURN = {}\n", .{cval});
                    return cval;
                },

                .CONSTANT => {
                    var cval = self.chunki.readConst();
                    if (conf.DEBUG_TRACE_EXECUTION) {
                        std.debug.print("CONSTANT = {}\n", .{cval});
                    }
                    try self.stack.append(cval);
                },
                .FALSE => try self.stack.append(.{ .BOOL = false }),
                .TRUE => try self.stack.append(.{ .BOOL = true }),
                .NIL => try self.stack.append(.NIL),
                .NOT => {
                    var cval = self.readStack();
                    self.stack.append(.{ .BOOL = !cval.as_bool() }) catch unreachable;
                },
                .NEGATE => {
                    var cval = self.readStack();
                    var neg: Value = switch (cval) {
                        .NUMBER => |n| .{ .NUMBER = -n },
                        .BOOL => |b| .{ .BOOL = !b },
                        .NIL => |_| .NIL,
                        .OBJ => |_| return error.NegatingObject,
                    };
                    if (conf.DEBUG_TRACE_EXECUTION) {
                        std.debug.print("NEGATE {} => {}\n", .{ cval, neg });
                    }
                    self.stack.append(neg) catch unreachable;
                },
                .ADD => {
                    try self.stack.append(try self.binaryOp(OpCode.ADD));
                },
                .DIV => {
                    try self.stack.append(try self.binaryOp(OpCode.DIV));
                },
                .MUL => {
                    try self.stack.append(try self.binaryOp(OpCode.MUL));
                },
                .SUB => {
                    try self.stack.append(try self.binaryOp(OpCode.SUB));
                },
                .EQUAL => {
                    try self.stack.append(try self.boolOp(Value.equal));
                },
                .GREATER => {
                    try self.stack.append(try self.boolOp(Value.greater));
                },
                .LESS => {
                    try self.stack.append(try self.boolOp(Value.less));
                },
                .PRINT => {
                    const top = self.readStack();
                    try top.printTo(std.debug);
                    std.debug.print("\n", .{});
                },
                .POP => {
                    _ = self.readStack();
                },
                .DEFINE_GLOBAL => {
                    const cval = self.chunki.readConst();
                    const s = cval.asStr() orelse return error.NON_STRING_GLOBAL;
                    const top = self.readStack();
                    try self.globals.put(s, top);
                },
                .GET_GLOBAL => {
                    const cval = self.chunki.readConst();
                    const s = cval.asStr() orelse return error.NON_STRING_GLOBAL;
                    const gval = self.globals.get(s) orelse return error.GLOBAL_NON_EXISTENT;
                    try self.stack.append(gval);
                },
                .SET_GLOBAL => {
                    const cval = self.chunki.readConst();
                    const k = cval.asStr() orelse return error.NON_STRING_GLOBAL;
                    const v = self.peekStack();
                    var prev = try self.globals.fetchPut(k, v);
                    if (prev == null) {
                        return error.GLOBAL_NON_EXISTENT_NOT_SET;
                    }
                },
                .SET_LOCAL => {
                    const slot = self.chunki.readSlot();
                    self.stack.items[slot] = self.peekStack();
                },
                .GET_LOCAL => {
                    const slot = self.chunki.readSlot();
                    try self.stack.append(self.stack.items[slot]);
                },
                .JUMP => {
                    var target = self.chunki.readJump();
                    try self.chunki.jump(target);
                },
                .JUMP_IF_FALSE => {
                    var target = self.chunki.readJump();
                    if (!self.peekStack().as_bool()) {
                        try self.chunki.jump(target);
                    }
                },
            }
        }
        return Value{ .NUMBER = 0 };
    }

    fn newObjectValue(self: *VM, data: ObjData) !Value {
        var res = try self.alloc.create(Obj);
        res.meta = 0;
        res.data = data;
        try self.freelist.push(res);
        return Value{ .OBJ = res };
    }

    fn readStack(self: *VM) Value {
        const newlen = self.stack.items.len - 1;
        var res = self.stack.items[newlen];
        self.stack.items.len -= 1;
        return res;
    }

    fn peekStack(self: *VM) Value {
        return self.stack.items[self.stack.items.len - 1];
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();

        self.freelist.deinit(self.alloc, Obj.deinit);
        self.globals.deinit();
    }

    fn boolOp(self: *VM, comptime op: fn (Value, Value) value.ValueError!bool) value.ValueError!Value {
        const b = self.readStack();
        const a = self.readStack();
        return Value{ .BOOL = try @call(.{ .modifier = .always_inline }, op, .{ a, b }) };
    }

    fn binaryOp(self: *VM, comptime op: OpCode) !Value {
        const b = self.readStack();
        const a = self.readStack();

        if (op == .ADD) {
            if (a.asStr()) |a_s| {
                if (b.asStr()) |b_s| {
                    const s = try value.concatStr(a_s, b_s, self.alloc);
                    return try self.newObjectValue(.{ .STR = s });
                }
            }
        }

        var bval: f64 = switch (b) {
            .NUMBER => |n| n,
            else => return error.MATH_ON_NON_NUMBER,
        };

        var aval: f64 = switch (a) {
            .NUMBER => |n| n,
            else => return error.MATH_ON_NON_NUMBER,
        };
        const res = switch (op) {
            OpCode.ADD => aval + bval,
            OpCode.DIV => aval / bval,
            OpCode.MUL => aval * bval,
            OpCode.SUB => aval - bval,
            else => unreachable,
        };
        if (conf.DEBUG_TRACE_EXECUTION) {
            std.debug.print("BIN_OP {} __ {} => {}\n", .{ a, b, res });
        }
        return Value{ .NUMBER = res };
    }
};
