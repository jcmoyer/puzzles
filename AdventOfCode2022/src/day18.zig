const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const V3 = sr.vector.Vec3(i32);
const dirs = [6]V3{
    V3.init(1, 0, 0),
    V3.init(-1, 0, 0),
    V3.init(0, 1, 0),
    V3.init(0, -1, 0),
    V3.init(0, 0, 1),
    V3.init(0, 0, -1),
};

fn isEdge(pos: V3) bool {
    return pos.x == 0 or pos.y == 0 or pos.z == 0 or pos.x == 20 or pos.y == 20 or pos.z == 20;
}

fn anyPathToEdge(allocator: Allocator, start: V3, cubes: std.AutoArrayHashMap(V3, void)) !bool {
    var look = std.ArrayList(V3).init(allocator);
    try look.append(start);
    var seen = std.AutoArrayHashMap(V3, void).init(allocator);
    while (look.items.len > 0) {
        const next = look.pop();
        if (seen.contains(next)) {
            continue;
        }
        try seen.put(next, {});
        for (dirs) |d| {
            const adj = next.add(d);
            if (seen.contains(adj)) {
                continue;
            }
            if (cubes.contains(adj)) {
                continue;
            }
            if (isEdge(adj)) {
                return true;
            }
            try look.append(adj);
        }
    }
    return false;
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var cubes = std.AutoArrayHashMap(V3, void).init(ps.allocator);
    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var cube = V3{};
        _ = try sr.parse("{d},{d},{d}", line, .{ &cube.x, &cube.y, &cube.z });
        try cubes.put(cube, {});
    }
    var area1: usize = cubes.count() * 6;
    var area2 = area1;
    for (cubes.keys()) |c| {
        for (dirs) |d| {
            const v = c.add(d);
            if (cubes.contains(v)) {
                area1 -= 1;
                area2 -= 1;
            } else if (!try anyPathToEdge(ps.allocator, v, cubes)) {
                area2 -= 1;
            }
        }
    }
    try ps.solution(area1);
    try ps.solution(area2);
}
