const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Range = sr.InclusiveRange1D(u32);

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var part1: u32 = 0;
    var part2: u32 = 0;
    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var x0: u32 = 0;
        var x1: u32 = 0;
        var y0: u32 = 0;
        var y1: u32 = 0;
        _ = try sr.parse("{d}-{d},{d}-{d}", line, .{ &x0, &x1, &y0, &y1 });
        var r1 = Range{ .min = x0, .max = x1 };
        var r2 = Range{ .min = y0, .max = y1 };
        if (r1.contains(r2) or r2.contains(r1)) {
            part1 += 1;
        }
        if (r1.overlaps(r2)) {
            part2 += 1;
        }
    }
    try ps.solution(part1);
    try ps.solution(part2);
}

const example_input =
    \\2-4,6-8
    \\2-3,4-5
    \\5-7,7-9
    \\2-8,3-7
    \\6-6,4-6
    \\2-6,4-8
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 2), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 4), solutions[1].integer);
}
