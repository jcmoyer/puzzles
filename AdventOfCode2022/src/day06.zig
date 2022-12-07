const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var s = Solution.calcFromString(ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const Solution = struct {
    part1: usize = 0,
    part2: usize = 0,

    fn calcFromString(s: []const u8) Solution {
        var result = Solution{};

        var start: usize = 0;
        p1: while (start < s.len - 4) : (start += 1) {
            const v = @as(@Vector(4, u8), s[start .. start + 4][0..4].*);
            comptime var i: u8 = 0;
            inline while (i < 4) : (i += 1) {
                const vi = @splat(4, v[i]);
                if (@popCount(@bitCast(u4, vi == v)) >= 2) {
                    continue :p1;
                }
            }
            result.part1 = start + 4;
            break;
        }
        p2: while (start < s.len - 14) : (start += 1) {
            const v = @as(@Vector(14, u8), s[start .. start + 14][0..14].*);
            comptime var i: u8 = 0;
            inline while (i < 14) : (i += 1) {
                const vi = @splat(14, v[i]);
                if (@popCount(@bitCast(u14, vi == v)) >= 2) {
                    continue :p2;
                }
            }
            result.part2 = start + 14;
            break;
        }
        return result;
    }
};

// another possible implementation
// pub fn solve(ps: *runner.PuzzleSolverState) !void {
//     var masks: [4096]u32 align(32) = undefined;

//     var start: usize = 0;
//     while (start < 4096) : (start += 1) {
//         masks[start] = @as(u32, 1) << @intCast(u5, ps.input_text[start] & 0b11111);
//     }
//     start = 0;

//     while (start < 4096 - 4) : (start += 1) {
//         const v = @as(@Vector(4, u32), masks[start .. start + 4][0..4].*);
//         const b = @reduce(.Or, v);
//         if (@popCount(b) == 4) {
//             try ps.solution(start + 4);
//             break;
//         }
//     }
//     while (start < 4096 - 14) : (start += 1) {
//         const v = @as(@Vector(14, u32), masks[start .. start + 14][0..14].*);
//         const b = @reduce(.Or, v);
//         if (@popCount(b) == 14) {
//             try ps.solution(start + 14);
//             break;
//         }
//     }
// }

const example_input =
    \\mjqjpqmgbljsphdztnvjfqwrcgsmlb
;

test "parts 1 and 2" {
    var s = Solution.calcFromString(example_input);
    try std.testing.expectEqual(@as(usize, 7), s.part1);
    try std.testing.expectEqual(@as(usize, 19), s.part2);
}
