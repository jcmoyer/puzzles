const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const PointSet = std.AutoArrayHashMap(sr.Vec2i, void);

const sand_spawn_point = sr.Vec2i{ .x = 500, .y = 0 };

const Sand = struct {
    pos: sr.Vec2i,
};

const World = struct {
    rock: *PointSet,
    settled_sand: *PointSet,
    bounds: Bounds,
    current_sand: ?Sand = null,
    floor: bool = false,

    fn spawnSand(self: *World) bool {
        if (self.current_sand != null) {
            return false;
        }
        if (self.solid(sand_spawn_point)) {
            return false;
        }
        self.current_sand = Sand{
            .pos = sand_spawn_point,
        };
        return true;
    }

    fn tick(self: *World) !bool {
        if (!self.spawnSand() and self.current_sand == null) {
            return false;
        }

        if (self.current_sand) |*s| {
            const down = s.pos.add(sr.Vec2i{ .x = 0, .y = 1 });
            const down_left = s.pos.add(sr.Vec2i{ .x = -1, .y = 1 });
            const down_right = s.pos.add(sr.Vec2i{ .x = 1, .y = 1 });

            if (!self.solid(down)) {
                s.pos = down;
                if (s.pos.y > self.bounds.max.y) {
                    return false;
                }
            } else if (!self.solid(down_left)) {
                s.pos = down_left;
            } else if (!self.solid(down_right)) {
                s.pos = down_right;
            } else {
                try self.settled_sand.put(s.pos, {});
                self.current_sand = null;
            }
            return true;
        } else {
            return false;
        }
    }

    fn solid(self: World, pos: sr.Vec2i) bool {
        return self.isRock(pos) or self.settled_sand.contains(pos);
    }

    fn isRock(self: World, pos: sr.Vec2i) bool {
        if (self.floor and self.bounds.max.y == pos.y) {
            return true;
        }
        return self.rock.contains(pos);
    }
};

const Bounds = struct {
    min: sr.Vec2i = .{
        .x = std.math.maxInt(i32),
        .y = std.math.maxInt(i32),
    },
    max: sr.Vec2i = .{
        .x = std.math.minInt(i32),
        .y = std.math.minInt(i32),
    },

    fn width(self: Bounds) i32 {
        return self.max.x - self.min.x;
    }

    fn height(self: Bounds) i32 {
        return self.max.y - self.min.y;
    }

    fn contains(self: Bounds, p: sr.Vec2i) bool {
        return p.x >= self.min.x and p.y >= self.min.y and p.x <= self.max.x and p.y <= self.max.y;
    }
};

fn computeBounds(m: PointSet) Bounds {
    var b: Bounds = .{};
    for (m.keys()) |p| {
        b.min.x = std.math.min(b.min.x, p.x);
        b.min.y = std.math.min(b.min.y, p.y);
        b.max.x = std.math.max(b.max.x, p.x);
        b.max.y = std.math.max(b.max.y, p.y);
    }
    b.min.x = std.math.min(b.min.x, sand_spawn_point.x);
    b.min.y = std.math.min(b.min.y, sand_spawn_point.y);
    b.max.x = std.math.max(b.max.x, sand_spawn_point.x);
    b.max.y = std.math.max(b.max.y, sand_spawn_point.y);
    return b;
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var rock_map = std.AutoArrayHashMap(sr.Vec2i, void).init(ps.allocator);
    var sand_map = std.AutoArrayHashMap(sr.Vec2i, void).init(ps.allocator);

    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var numbers = std.mem.tokenize(u8, line, " ->");
        var last_p: ?sr.Vec2i = null;
        while (numbers.next()) |pair| {
            var p = sr.Vec2i{};
            if (!try sr.parse("{d},{d}", pair, .{ &p.x, &p.y })) {
                return error.BadParse;
            }
            if (last_p) |last| {
                if (p.x == last.x) {
                    var ymin: i32 = std.math.min(p.y, last.y);
                    var ymax: i32 = std.math.max(p.y, last.y);
                    while (ymin <= ymax) : (ymin += 1) {
                        try rock_map.put(sr.Vec2i{ .x = p.x, .y = ymin }, {});
                    }
                } else if (p.y == last.y) {
                    var xmin: i32 = std.math.min(p.x, last.x);
                    var xmax: i32 = std.math.max(p.x, last.x);
                    while (xmin <= xmax) : (xmin += 1) {
                        try rock_map.put(sr.Vec2i{ .x = xmin, .y = p.y }, {});
                    }
                } else {
                    unreachable;
                }
            }
            last_p = p;
        }
    }

    var b = computeBounds(rock_map);
    var w = World{
        .bounds = b,
        .rock = &rock_map,
        .settled_sand = &sand_map,
    };

    while (try w.tick()) {}
    try ps.solution(w.settled_sand.count());

    sand_map.clearRetainingCapacity();
    b.max.y += 2;
    w.bounds = b;
    w.floor = true;
    while (try w.tick()) {}
    try ps.solution(w.settled_sand.count());
}

const example_input =
    \\498,4 -> 498,6 -> 496,6
    \\503,4 -> 502,4 -> 502,9 -> 494,9
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 24), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 93), solutions[1].integer);
}
