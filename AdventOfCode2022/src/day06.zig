const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var s = Solution.calcFromString(ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const CharSet = u26;

fn toCharSetLower(str: []const u8) CharSet {
    var set: CharSet = 0;
    for (str) |ch| {
        std.debug.assert(std.ascii.isLower(ch));
        set |= (@as(u26, 1) << @intCast(u5, ch - 'a'));
    }
    return set;
}

const Solution = struct {
    const no_solution = std.math.maxInt(usize);
    part1: usize = no_solution,
    part2: usize = no_solution,

    fn isPacketStart(packet: []const u8, packet_width: usize) bool {
        return @popCount(toCharSetLower(packet[0..packet_width])) == packet_width;
    }

    fn calcFromString(str: []const u8) Solution {
        var sol = Solution{};
        for (str) |_, start| {
            const slice = str[start..];
            if (sol.part1 == no_solution and isPacketStart(slice, 4)) {
                sol.part1 = start + 4;
            }
            if (sol.part2 == no_solution and isPacketStart(slice, 14)) {
                sol.part2 = start + 14;
            }
            if (sol.part1 != no_solution and sol.part2 != no_solution) {
                break;
            }
        }
        return sol;
    }
};

const example_input =
    \\mjqjpqmgbljsphdztnvjfqwrcgsmlb
;

test "parts 1 and 2" {
    var s = Solution.calcFromString(example_input);
    try std.testing.expectEqual(@as(usize, 7), s.part1);
    try std.testing.expectEqual(@as(usize, 19), s.part2);
}
