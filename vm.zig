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
        }
        return;
    }
}

pub const VMError = error{
    COMPILE_ERROR,
    RUN_ERROR,
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
    ) VMError!void {
        while (true) {
            switch (self.readInstruction()) {
                OpCode.RETURN => {
                    var cval = self.readStack();
                    std.debug.print("RETURN = {}\n", .{cval});
                    return;
                },

                OpCode.CONSTANT => {
                    var cval = self.readConst();
                    if (conf.DEBUG_TRACE_EXECUTION) {
                        std.debug.print("CONSTANT = {}\n", .{cval});
                    }
                    self.stack.append(cval) catch |err| return error.RUN_ERROR;
                },
                OpCode.NEGATE => {
                    var cval = self.readStack();
                    var neg = -cval;
                    if (conf.DEBUG_TRACE_EXECUTION) {
                        std.debug.print("NEGATE {} => {}\n", .{ cval, neg });
                    }
                    self.stack.append(neg) catch unreachable;
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
};
