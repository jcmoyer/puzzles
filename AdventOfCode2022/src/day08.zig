const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const RaycastResult = struct {
    dist_up: usize = 0,
    dist_left: usize = 0,
    dist_right: usize = 0,
    dist_down: usize = 0,
    num_visible_edges: usize = 4,

    fn isVisibleFromEdge(self: RaycastResult) bool {
        return self.num_visible_edges > 0;
    }

    fn scenicScore(self: RaycastResult) usize {
        return self.dist_down * self.dist_left * self.dist_right * self.dist_up;
    }
};

fn raycast(tm: *sr.Array2D(u8), pos: sr.Vec2us) RaycastResult {
    var result = RaycastResult{};
    const x = pos.x;
    const y = pos.y;

    var ox: usize = 0;
    var oy: usize = 0;
    const orig = tm.at(x, y);

    ox = x -% 1;
    oy = y;
    while (ox < x) : (ox -%= 1) {
        result.dist_left += 1;
        if (tm.at(ox, oy) >= orig) {
            result.num_visible_edges -= 1;
            break;
        }
    }

    ox = x + 1;
    oy = y;
    while (ox < tm.width) : (ox += 1) {
        result.dist_right += 1;
        if (tm.at(ox, oy) >= orig) {
            result.num_visible_edges -= 1;
            break;
        }
    }

    ox = x;
    oy = y -% 1;
    while (oy < y) : (oy -%= 1) {
        result.dist_up += 1;
        if (tm.at(ox, oy) >= orig) {
            result.num_visible_edges -= 1;
            break;
        }
    }

    ox = x;
    oy = y + 1;
    while (oy < tm.height) : (oy += 1) {
        result.dist_down += 1;
        if (tm.at(ox, oy) >= orig) {
            result.num_visible_edges -= 1;
            break;
        }
    }

    return result;
}

const Solution = struct {
    part1: usize,
    part2: usize,

    fn calcFromStream(allocator: Allocator, reader: anytype) !Solution {
        var tm = try sr.readTilemap(allocator, reader);
        defer tm.deinit(allocator);

        var y: usize = 0;
        var x: usize = 0;

        var sum_visible_from_edge: usize = 0;
        var best_scenic_score: usize = 0;

        while (y < tm.height) : (y += 1) {
            x = 0;
            while (x < tm.width) : (x += 1) {
                const result = raycast(&tm, .{ .x = x, .y = y });
                sum_visible_from_edge += @boolToInt(result.isVisibleFromEdge());
                best_scenic_score = std.math.max(best_scenic_score, result.scenicScore());
            }
        }

        return Solution{
            .part1 = sum_visible_from_edge,
            .part2 = best_scenic_score,
        };
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    const s = try Solution.calcFromStream(ps.allocator, ps.getPuzzleInputReader());
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const example_input =
    \\30373
    \\25512
    \\65332
    \\33549
    \\35390
;

test "parts 1 and 2" {
    var fbs = std.io.fixedBufferStream(example_input);
    const s = try Solution.calcFromStream(std.testing.allocator, fbs.reader());
    try std.testing.expectEqual(@as(usize, 21), s.part1);
    try std.testing.expectEqual(@as(usize, 8), s.part2);
}
