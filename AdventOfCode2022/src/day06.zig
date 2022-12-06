const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var s = try Solution.calcFromString(ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const CharSet = u64;

fn putCharSetAssumeCapacity(set: *CharSet, str: []const u8) void {
    var new_set: CharSet = 0;
    for (str) |ch| {
        new_set |= (@as(u64, 1) << @intCast(u6, ch - 'a'));
    }
    set.* = new_set;
}

const Solution = struct {
    const no_solution = std.math.maxInt(usize);
    part1: usize,
    part2: usize,

    fn calcFromString(str: []const u8) !Solution {
        var set_4: u64 = 0;
        var set_14: u64 = 0;
        var start: usize = 0;
        var part1: usize = no_solution;
        var part2: usize = no_solution;
        while (start < str.len) : (start += 1) {
            if (part1 == no_solution) {
                putCharSetAssumeCapacity(&set_4, str[start .. start + 4]);
                if (@popCount(set_4) == 4) {
                    part1 = start + 4;
                }
            }
            if (part2 == no_solution) {
                putCharSetAssumeCapacity(&set_14, str[start .. start + 14]);
                if (@popCount(set_14) == 14) {
                    part2 = start + 14;
                }
            }
            if (part1 != no_solution and part2 != no_solution) {
                break;
            }
        }
        if (part1 == no_solution) {
            return error.NoPart1;
        }
        if (part2 == no_solution) {
            return error.NoPart2;
        }
        return Solution{
            .part1 = part1,
            .part2 = part2,
        };
    }
};

const example_input =
    \\mjqjpqmgbljsphdztnvjfqwrcgsmlb
;

test "parts 1 and 2" {
    var s = try Solution.calcFromString(example_input);
    try std.testing.expectEqual(@as(usize, 7), s.part1);
    try std.testing.expectEqual(@as(usize, 19), s.part2);
}
