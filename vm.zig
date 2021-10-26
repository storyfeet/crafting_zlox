const std = @import("std");
const chunk = @import("chunk.zig");
const conf = @import("config.zig");
const OpCode = chunk.OpCode;
const OpData = chunk.OpData;
const Value = chunk.Value;
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPAlloc{};
    var chk = chunk.Chunk.init(&gpa.allocator);
    defer chk.deinit();
    try chk.addOp(OpData{ .CONSTANT = 3 });
    try chk.addOp(OpData{ .CONSTANT = 4 });
    try chk.addOp(OpData.NEGATE);
    try chk.addOp(OpData.ADD);
    try chk.addOp(OpData{ .CONSTANT = 7 });
    try chk.addOp(OpData.DIV);
    try chk.addOp(OpData.RETURN);

    var vm: VM = VM.init(&chk, &gpa.allocator);
    defer vm.deinit();
    var res = vm.run();

    if (res) |_| {
        std.debug.print("RUN OK\n", .{});
    } else |err| {
        switch (err) {
            error.COMPILE_ERROR => std.debug.print("COMPILE ERROR {}\n", .{err}),
            error.RUN_ERROR => std.debug.print("RUN ERROR {}\n", .{err}),
            error.OutOfMemory => std.debug.print("Out of Memory {}\n", .{err}),
        }
        return;
    }
}

pub const VMError = error{
    COMPILE_ERROR,
    RUN_ERROR,
    MATH_ON_NON_NUMBER,
    OutOfMemory,
};

pub const VM = struct {
    ip: usize,
    chunk: *chunk.Chunk,
    stack: std.ArrayList(Value),
    pub fn init(ch: *chunk.Chunk, alloc: *std.mem.Allocator) VM {
        return VM{
            .chunk = ch,
            .ip = 0,
            .stack = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn run(
        self: *VM,
    ) VMError!Value {
        while (true) {
            switch (self.readInstruction()) {
                .RETURN => {
                    var cval = self.readStack();
                    std.debug.print("RETURN = {}\n", .{cval});
                    return cval;
                },

                .CONSTANT => {
                    var cval = self.readConst();
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
                    };
                    if (conf.DEBUG_TRACE_EXECUTION) {
                        std.debug.print("NEGATE {} => {}\n", .{ cval, neg });
                    }
                    self.stack.append(neg) catch unreachable;
                },
                OpCode.ADD => {
                    try self.stack.append(try self.binaryOp(OpCode.ADD));
                },
                OpCode.DIV => {
                    try self.stack.append(try self.binaryOp(OpCode.DIV));
                },
                OpCode.MUL => {
                    try self.stack.append(try self.binaryOp(OpCode.MUL));
                },
                OpCode.SUB => {
                    try self.stack.append(try self.binaryOp(OpCode.SUB));
                },
            }
        }
    }

    fn readInstruction(self: *VM) OpCode {
        var op = @intToEnum(OpCode, self.chunk.ins.items[self.ip]);
        self.ip += 1;
        return op;
    }

    fn readConst(self: *VM) Value {
        var pos = @intCast(usize, self.chunk.ins.items[self.ip]);
        self.ip += 1;
        return self.chunk.consts.items[pos];
    }

    fn readStack(self: *VM) Value {
        const newlen = self.stack.items.len - 1;
        var res = self.stack.items[newlen];
        self.stack.items.len -= 1;
        return res;
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }

    fn binaryOp(self: *VM, op: comptime OpCode) !Value {
        const b = self.readStack();
        const a = self.readStack();
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
