const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var top: [3]u64 = .{ 0, 0, 0 };
    var current_cals: u64 = 0;

    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        if (line.len == 0) {
            if (current_cals > top[0]) {
                top[0] = current_cals;
            }
            std.sort.sort(u64, &top, {}, std.sort.asc(u64));
            current_cals = 0;
        } else {
            current_cals += try std.fmt.parseInt(u64, line, 10);
        }
    }
    if (current_cals > top[0]) {
        top[0] = current_cals;
    }
    std.sort.sort(u64, &top, {}, std.sort.asc(u64));
    try ps.solution(top[2]);
    try ps.solution(top[0] + top[1] + top[2]);
}

const example_input =
    \\1000
    \\2000
    \\3000
    \\
    \\4000
    \\
    \\5000
    \\6000
    \\
    \\7000
    \\8000
    \\9000
    \\
    \\10000
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 24000), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 45000), solutions[1].integer);
}
