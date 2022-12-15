const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var part1: u64 = 0;
    var part2: u64 = 0;
    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var game_id: u8 = 0;
        game_id += line[0] - 'A';
        game_id += 3 * (line[2] - 'X');
        part1 += lut_part1[game_id];
        part2 += lut_part2[game_id];
    }
    try ps.solution(part1);
    try ps.solution(part2);
}

const lut_part1 = [_]u32{
    1 + 3, // A X; choice points + win/lose points
    1 + 0, // B X
    1 + 6, // C X
    2 + 6, // A Y
    2 + 3, // B Y
    2 + 0, // C Y
    3 + 0, // A Z
    3 + 6, // B Z
    3 + 3, // C Z
};

const lut_part2 = [_]u32{
    3 + 0, // same as above
    1 + 0,
    2 + 0,
    1 + 3,
    2 + 3,
    3 + 3,
    2 + 6,
    3 + 6,
    1 + 6,
};

const example_input =
    \\A Y
    \\B X
    \\C Z
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 15), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 12), solutions[1].integer);
}
