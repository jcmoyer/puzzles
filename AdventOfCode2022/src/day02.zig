const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    const s = try Solution.calcFromStream(ps.getPuzzleInputReader());
    try ps.solution(s.sum1);
    try ps.solution(s.sum2);
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

const Solution = struct {
    sum1: u32 = 0,
    sum2: u32 = 0,

    fn calcFromStream(reader: anytype) !Solution {
        var buf: [256]u8 = undefined;
        var solution = Solution{};

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");

            var game_id: u8 = 0;
            game_id += stripped_line[0] - 'A';
            game_id += 3 * (stripped_line[2] - 'X');
            solution.sum1 += lut_part1[game_id];
            solution.sum2 += lut_part2[game_id];
        }

        return solution;
    }
};

const example_input =
    \\A Y
    \\B X
    \\C Z
;

test "part 1" {
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    const s = try Solution.calcFromStream(reader);
    try std.testing.expectEqual(@as(u64, 15), s.sum1);
}

test "part 2" {
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    const s = try Solution.calcFromStream(reader);
    try std.testing.expectEqual(@as(u64, 12), s.sum2);
}
