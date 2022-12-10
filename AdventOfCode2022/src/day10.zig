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

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var instrs = std.ArrayListUnmanaged(Instr){};
    var x_map = std.AutoArrayHashMapUnmanaged(u64, i64){};
    var x_val: i64 = 1;

    var it = std.mem.tokenize(u8, ps.input_text, "\r\n");
    while (it.next()) |line| {
        var parts = std.mem.tokenize(u8, line, " ");

        const instr_name = parts.next() orelse break;
        const operand_str = parts.next(); // null if noop

        if (std.mem.eql(u8, instr_name, "addx")) {
            const val = try std.fmt.parseInt(i64, operand_str.?, 10);
            var i = try instrs.addOne(ps.allocator);
            i.* = .{ .addx = val };
        } else if (std.mem.eql(u8, instr_name, "noop")) {
            var i = try instrs.addOne(ps.allocator);
            i.* = .noop;
        }
    }

    var scan_output_x: usize = 0;
    var scan_output_y: usize = 0;
    var screen = sr.Array2D(u8).init(ps.allocator);
    try screen.resize(40, 6);
    std.mem.set(u8, screen.data, '.');

    var cycle_count: u64 = 1;
    for (instrs.items) |inst| {
        const cycle_start = cycle_count;
        const cycle_end = cycle_start + inst.cycleCount();
        var c: u64 = cycle_start;
        while (c < cycle_end) : (c += 1) {
            try x_map.put(ps.allocator, c, x_val * @intCast(i64, c));
            var render_x_min: i64 = x_val - 1;
            var render_x_max: i64 = x_val + 1;
            if (@intCast(i64, scan_output_x) >= render_x_min and @intCast(i64, scan_output_x) <= render_x_max) {
                screen.at(scan_output_x, scan_output_y).* = '#';
            }
            scan_output_x += 1;
            if (scan_output_x == 40) {
                scan_output_x = 0;
                scan_output_y += 1;
            }
        }
        x_val += inst.value();
        cycle_count += inst.cycleCount();
    }

    try ps.solution(x_map.get(20).? +
        x_map.get(60).? +
        x_map.get(100).? +
        x_map.get(140).? +
        x_map.get(180).? +
        x_map.get(220).?);

    var part2_text: [40 * 6 + 6]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&part2_text);
    try sr.formatArrayMultiline(screen, 40, fbs.writer());
    try ps.solution(part2_text);
}
