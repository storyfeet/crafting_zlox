const std = @import("std");
const chunk = @import("chunk.zig");
const OpCode = chunk.OpCode;

pub fn main() !void {}

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUN_ERROR,
};

pub const VM = struct {
    ip: *u8,
    chunk: *chunk.Chunk,
    pub fn init() VM {}

    pub fn interpret(ch: *chunk.Chunk) InterpretResult {}

    pub fn run(v: VM) InterpretResult {
        while (true) {
            op = @intToEnum(v.it.*);
            ip += 1;
            switch (op) {
                OpCode.OP_RETURN => return InterpretResult.OK,
            }
        }
    }

    fn readByte(vm: *VM) !OpCode {
        b = ip;
    }

    pub fn deinit() void {}
};
