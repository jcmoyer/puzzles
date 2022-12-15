const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Range = sr.InclusiveRange1D(u32);

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    const s = try Solution.calcFromStream(ps.getPuzzleInputReader());
    try ps.solution(s.sum1);
    try ps.solution(s.sum2);
}

const Solution = struct {
    sum1: u32 = 0,
    sum2: u32 = 0,

    fn calcFromStream(reader: anytype) !Solution {
        var buf: [256]u8 = undefined;

        var solution = Solution{};

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");

            var it = std.mem.tokenize(u8, stripped_line, "-,");
            const x0 = it.next().?;
            const x1 = it.next().?;
            const y0 = it.next().?;
            const y1 = it.next().?;

            const x0i = try std.fmt.parseInt(u32, x0, 10);
            const x1i = try std.fmt.parseInt(u32, x1, 10);
            const y0i = try std.fmt.parseInt(u32, y0, 10);
            const y1i = try std.fmt.parseInt(u32, y1, 10);

            var r1 = Range{ .min = x0i, .max = x1i };
            var r2 = Range{ .min = y0i, .max = y1i };
            if (r1.contains(r2) or r2.contains(r1)) {
                solution.sum1 += 1;
            }
            if (r1.overlaps(r2)) {
                solution.sum2 += 1;
            }
        }

        return solution;
    }
};

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
