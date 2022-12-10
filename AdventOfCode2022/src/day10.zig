const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Instr = union(enum) {
    addx: i64,
    noop,

    fn cycleCount(self: Instr) u64 {
        return switch (self) {
            .addx => 2,
            .noop => 1,
        };
    }

    fn value(self: Instr) i64 {
        return switch (self) {
            .addx => |x| x,
            .noop => 0,
        };
    }
};

const Screen = struct {
    const width = 40;
    const height = 6;

    buffer: sr.Array2D(u8) = .{},
    output_x: usize = 0,
    output_y: usize = 0,

    fn init(allocator: Allocator) !Screen {
        var buffer = sr.Array2D(u8){};
        try buffer.resize(allocator, width, height);
        std.mem.set(u8, buffer.data, '.');
        return Screen{
            .buffer = buffer,
        };
    }

    fn deinit(self: *Screen, allocator: Allocator) void {
        self.buffer.deinit(allocator);
    }

    fn advance(self: *Screen) void {
        self.output_x += 1;
        if (self.output_x == width) {
            self.output_x = 0;
            self.output_y += 1;
        }
    }

    fn pixelOn(self: *Screen) void {
        self.buffer.at(self.output_x, self.output_y).* = '#';
    }

    fn toString(self: Screen) ![width * height + height]u8 {
        var buf: [width * height + height]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        try sr.formatArrayMultiline(self.buffer, width, fbs.writer());
        return buf;
    }
};

const Cpu = struct {
    program: []Instr = &[_]Instr{},
    ip: usize = 0,
    cycle_counter: usize = 1,
    x: i64 = 1,
    /// If `null`, nothing will be rendered.
    screen: ?*Screen = null,
    cycles_on_ip: usize = 0,

    /// Returns `true` if the cycle counter advanced, `false` if there's nothing
    /// left to execute.
    fn runOneCycle(self: *Cpu) bool {
        const instr = self.program[self.ip];
        if (self.screen) |scr| {
            const r_min = self.x - 1;
            const r_max = self.x + 1;
            const s_output_x = @intCast(i64, scr.output_x);
            if (s_output_x >= r_min and s_output_x <= r_max) {
                scr.pixelOn();
            }
            scr.advance();
        }
        self.cycles_on_ip += 1;
        self.cycle_counter += 1;
        if (self.cycles_on_ip == instr.cycleCount()) {
            self.x += instr.value();
            self.cycles_on_ip = 0;
            self.ip += 1;
        }
        return self.ip != self.program.len;
    }

    fn runToEnd(self: *Cpu) void {
        while (self.runOneCycle()) {}
    }

    fn signalStrength(self: Cpu) i64 {
        return @intCast(i64, self.cycle_counter) * self.x;
    }
};

fn loadProgram(allocator: Allocator, str: []const u8) ![]Instr {
    var instrs = std.ArrayListUnmanaged(Instr){};
    var it = std.mem.tokenize(u8, str, "\r\n");
    while (it.next()) |line| {
        var parts = std.mem.tokenize(u8, line, " ");

        const instr_name = parts.next() orelse break;
        const operand_str = parts.next(); // null if noop

        if (std.mem.eql(u8, instr_name, "addx")) {
            const val = try std.fmt.parseInt(i64, operand_str.?, 10);
            try instrs.append(allocator, .{ .addx = val });
        } else if (std.mem.eql(u8, instr_name, "noop")) {
            try instrs.append(allocator, .noop);
        }
    }
    return try instrs.toOwnedSlice(allocator);
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var instrs = try loadProgram(ps.allocator, ps.input_text);
    var screen = try Screen.init(ps.allocator);
    var cpu = Cpu{ .program = instrs, .screen = &screen };

    const breakpoints = [_]usize{ 20, 60, 100, 140, 180, 220 };
    var signal_sum: i64 = 0;
    for (breakpoints) |bp| {
        while (cpu.cycle_counter != bp) {
            _ = cpu.runOneCycle();
        }
        signal_sum += cpu.signalStrength();
    }
    cpu.runToEnd();

    try ps.solution(signal_sum);
    try ps.solution(try screen.toString());
}
