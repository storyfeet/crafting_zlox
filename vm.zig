const std = @import("std");
const chunk = @import("chunk.zig");
const OpCode = chunk.OpCode;
const OpData = chunk.OpData;
const GPAlloc = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = GPAlloc{};
    var chk = chunk.Chunk.init(&gpa.allocator);
    try chk.addOp(OpData{ .OP_CONSTANT = 3 });
    try chk.addOp(OpData{ .OP_CONSTANT = 4 });
    try chk.addOp(OpData.OP_RETURN);

    var vm: VM = VM.init(&chk, null);
    var res = vm.run();

    switch (res) {
        InterpretResult.OK => std.debug.print("RUN OK\n", .{}),
        InterpretResult.COMPILE_ERROR => std.debug.print("COMPILE ERROR\n", .{}),
        InterpretResult.RUN_ERROR => std.debug.print("RUN ERROR\n", .{}),
    }
}

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUN_ERROR,
};

pub const VM = struct {
    ip: usize,
    chunk: *chunk.Chunk,
    pub fn init(ch: *chunk.Chunk, ip: ?usize) VM {
        return VM{
            .chunk = ch,
            .ip = ip orelse 0,
        };
    }

    pub fn run(
        v: *VM,
    ) InterpretResult {
        while (true) {
            var op = @intToEnum(OpCode, v.chunk.ins.items[v.ip]);
            v.ip += 1;
            switch (op) {
                OpCode.OP_RETURN => return InterpretResult.OK,
                OpCode.OP_CONSTANT => {
                    var cid = v.chunk.ins.items[v.ip];
                    v.ip += 1;
                    std.debug.print("CONSTANT = {}\n", .{v.chunk.consts.items[cid]});
                },
            }
        }
    }

    fn readByte(vm: *VM) !OpCode {
        b = ip;
    }

    pub fn deinit() void {}
};
