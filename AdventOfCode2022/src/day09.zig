const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Rope = struct {
    allocator: Allocator,
    knots: []sr.Vec2i = &[_]sr.Vec2i{},
    tail_visited: std.AutoArrayHashMapUnmanaged(sr.Vec2i, void),

    fn initCount(allocator: Allocator, knot_count: usize) !Rope {
        var knots = try allocator.alloc(sr.Vec2i, knot_count);
        for (knots) |*knot| {
            knot.* = .{};
        }
        errdefer allocator.free(knots);
        var tail_visited = std.AutoArrayHashMapUnmanaged(sr.Vec2i, void){};
        try tail_visited.put(allocator, .{}, {});
        return Rope{
            .allocator = allocator,
            .knots = knots,
            .tail_visited = tail_visited,
        };
    }

    fn deinit(self: *Rope) void {
        self.allocator.free(self.knots);
        self.tail_visited.deinit(self.allocator);
    }

    fn moveHead(self: *Rope, dir: sr.Direction) !void {
        var head = &self.knots[0];
        var tail: *sr.Vec2i = undefined;
        head.* = head.add(dir.toVector());

        var knot: usize = 1;
        while (knot < self.knots.len) : (knot += 1) {
            head = &self.knots[knot - 1];
            tail = &self.knots[knot];

            var diff = head.sub(tail.*);
            const ax = try std.math.absInt(diff.x);
            const ay = try std.math.absInt(diff.y);
            if (ax <= 1 and ay <= 1) {
                // touching
                continue;
            } else if ((ax == 2 and ay == 0) or (ax == 0 and ay == 2)) {
                // movement along a row or column
                tail.* = tail.add(diff.divTrunc(2));
            } else {
                // movement along a diagonal
                tail.* = tail.add(diff.clamp(-1, 1));
            }
        }

        try self.tail_visited.put(self.allocator, self.knots[self.knots.len - 1], {});
    }

    fn moveHeadCount(self: *Rope, dir: sr.Direction, count: usize) !void {
        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.moveHead(dir);
        }
    }
};

const Solution = struct {
    part1: usize,
    part2: usize,

    fn calcFromString(allocator: Allocator, str: []const u8) !Solution {
        var rope1 = try Rope.initCount(allocator, 2);
        defer rope1.deinit();
        var rope2 = try Rope.initCount(allocator, 10);
        defer rope2.deinit();

        var it = std.mem.tokenize(u8, str, " \r\n");
        while (true) {
            const dir_str = it.next() orelse break;
            const dir = try sr.Direction.parseLRUD(dir_str[0]);
            const amt_str = it.next().?;
            const amt_int = try std.fmt.parseInt(usize, amt_str, 10);

            try rope1.moveHeadCount(dir, amt_int);
            try rope2.moveHeadCount(dir, amt_int);
        }

        return Solution{
            .part1 = rope1.tail_visited.count(),
            .part2 = rope2.tail_visited.count(),
        };
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    const s = try Solution.calcFromString(ps.allocator, ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const example_input =
    \\R 4
    \\U 4
    \\L 3
    \\D 1
    \\R 4
    \\D 1
    \\L 5
    \\R 2
;

test "parts 1 and 2" {
    const s = try Solution.calcFromString(std.testing.allocator, example_input);
    try std.testing.expectEqual(@as(usize, 13), s.part1);
    try std.testing.expectEqual(@as(usize, 1), s.part2);
}
