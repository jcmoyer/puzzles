const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var s = try Solution.calcFromString(ps.allocator, ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const Solution = struct {
    part1: usize = 0,
    part2: usize = 0,

    fn calcFromString(allocator: Allocator, str: []const u8) !Solution {
        var set_4 = std.AutoHashMapUnmanaged(u8, void){};
        defer set_4.clearAndFree(allocator);
        var set_14 = std.AutoHashMapUnmanaged(u8, void){};
        defer set_14.clearAndFree(allocator);
        var start: usize = 0;
        var part1: ?usize = null;
        var part2: ?usize = null;
        while (start < str.len) : (start += 1) {
            if (part1 == null) {
                set_4.clearRetainingCapacity();
                for (str[start .. start + 4]) |c| {
                    try set_4.put(allocator, c, {});
                }
                if (set_4.count() == 4) {
                    part1 = start + 4;
                }
            }
            if (part2 == null) {
                set_14.clearRetainingCapacity();
                for (str[start .. start + 14]) |c| {
                    try set_14.put(allocator, c, {});
                }
                if (set_14.count() == 14) {
                    part2 = start + 14;
                }
            }
        }
        return Solution{
            .part1 = part1.?,
            .part2 = part2.?,
        };
    }
};

const example_input =
    \\mjqjpqmgbljsphdztnvjfqwrcgsmlb
;

test "part 1" {
    var allocator = std.testing.allocator;
    var s = try Solution.calcFromString(allocator, example_input);
    try std.testing.expectEqual(@as(usize, 7), s.part1);
}

test "part 2" {
    var allocator = std.testing.allocator;
    var s = try Solution.calcFromString(allocator, example_input);
    try std.testing.expectEqual(@as(usize, 19), s.part2);
}
