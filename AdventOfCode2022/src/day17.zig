const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const PointSet = std.AutoArrayHashMap(sr.Vec2i, void);

fn solidAt(set: PointSet, pos: sr.Vec2i) bool {
    if (pos.x == 0) return true;
    if (pos.x == 8) return true;
    if (pos.y == 0) return true;
    return set.get(pos) != null;
}

const Shape = [4][4]u8;

const shapes = [5]Shape{
    .{
        .{ 1, 1, 1, 1 },
        .{ 0, 0, 0, 0 },
        .{ 0, 0, 0, 0 },
        .{ 0, 0, 0, 0 },
    },
    .{
        .{ 0, 1, 0, 0 },
        .{ 1, 1, 1, 0 },
        .{ 0, 1, 0, 0 },
        .{ 0, 0, 0, 0 },
    },
    .{
        .{ 0, 0, 1, 0 },
        .{ 0, 0, 1, 0 },
        .{ 1, 1, 1, 0 },
        .{ 0, 0, 0, 0 },
    },
    .{
        .{ 1, 0, 0, 0 },
        .{ 1, 0, 0, 0 },
        .{ 1, 0, 0, 0 },
        .{ 1, 0, 0, 0 },
    },
    .{
        .{ 1, 1, 0, 0 },
        .{ 1, 1, 0, 0 },
        .{ 0, 0, 0, 0 },
        .{ 0, 0, 0, 0 },
    },
};

const shape_dims = [5]sr.Vec2us{
    .{ .x = 4, .y = 1 },
    .{ .x = 3, .y = 3 },
    .{ .x = 3, .y = 3 },
    .{ .x = 1, .y = 4 },
    .{ .x = 2, .y = 2 },
};

fn nextShape(shape: usize) usize {
    return (shape + 1) % shapes.len;
}

const Piece = struct {
    shape: usize,
    x: usize,
    y: usize,

    fn canMoveVec(self: Piece, set: PointSet, dv: sr.Vec2i) bool {
        var y: i32 = 0;
        var x: i32 = 0;
        while (y < shape_dims[self.shape].y) : (y += 1) {
            x = 0;
            while (x < shape_dims[self.shape].x) : (x += 1) {
                if (shapes[self.shape][@intCast(usize, y)][@intCast(usize, x)] != 0) {
                    var dest = sr.Vec2i{ .x = @intCast(i32, self.x) + x, .y = @intCast(i32, self.y) - y };
                    dest = dest.add(dv);
                    if (solidAt(set, dest)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    fn canMoveRight(self: Piece, set: PointSet) bool {
        return self.canMoveVec(set, .{ .x = 1, .y = 0 });
    }

    fn canMoveLeft(self: Piece, set: PointSet) bool {
        return self.canMoveVec(set, .{ .x = -1, .y = 0 });
    }

    fn canMoveDown(self: Piece, set: PointSet) bool {
        return self.canMoveVec(set, .{ .x = 0, .y = -1 });
    }

    fn tryMoveLeft(self: *Piece, set: PointSet) bool {
        if (self.canMoveLeft(set)) {
            self.x -= 1;
            return true;
        }
        return false;
    }

    fn tryMoveRight(self: *Piece, set: PointSet) bool {
        if (self.canMoveRight(set)) {
            self.x += 1;
            return true;
        }
        return false;
    }

    fn tryMoveDown(self: *Piece, set: PointSet) bool {
        if (self.canMoveDown(set)) {
            self.y -= 1;
            return true;
        }
        return false;
    }

    fn addToSet(self: Piece, set: *PointSet) !void {
        var y: i32 = 0;
        var x: i32 = 0;
        while (y < shape_dims[self.shape].y) : (y += 1) {
            x = 0;
            while (x < shape_dims[self.shape].x) : (x += 1) {
                if (shapes[self.shape][@intCast(usize, y)][@intCast(usize, x)] != 0) {
                    var dest = sr.Vec2i{ .x = @intCast(i32, self.x) + x, .y = @intCast(i32, self.y) - y };
                    try set.putNoClobber(dest, {});
                }
            }
        }
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var solid = PointSet.init(ps.allocator);
    const jets = std.mem.trim(u8, ps.input_text, "\r\n");
    var jet_index: usize = 0;
    var shape_id: usize = 0;
    var spawn_y: usize = 3;
    var spawn_x: usize = 3;
    var current_piece = Piece{ .shape = shape_id, .x = spawn_x, .y = spawn_y };
    var cycle_map = std.ArrayList(usize).init(ps.allocator);
    var last_height: usize = 0;
    var height: usize = 0;
    var i: usize = 0;
    while (i < 20000) : (i += 1) {
        while (true) {
            switch (jets[jet_index]) {
                '<' => _ = current_piece.tryMoveLeft(solid),
                '>' => _ = current_piece.tryMoveRight(solid),
                else => unreachable,
            }
            jet_index = (jet_index + 1) % jets.len;

            if (!current_piece.tryMoveDown(solid)) {
                try current_piece.addToSet(&solid);

                height = std.math.max(height, @intCast(usize, current_piece.y));

                shape_id = nextShape(shape_id);
                const next_piece = Piece{
                    .shape = shape_id,
                    .x = spawn_x,
                    .y = height + 3 + shape_dims[shape_id].y,
                };

                current_piece = next_piece;

                try cycle_map.append(height - last_height);
                last_height = height;

                break;
            }
        }
    }

    try ps.solution(1 + sr.sum(usize, cycle_map.items[0..2022]));

    // basically, pick a long sequence and we should see it again, assuming we
    // recorded enough data
    //
    // not sure if there's a better method, need to research this
    const cycle_search_start = 1500;
    const cycle_search_width = 2500;
    const cycle_search_slice = cycle_map.items[cycle_search_start..];
    const cycle_search_want = cycle_search_slice[0..cycle_search_width];
    const cycle_start = std.mem.indexOfPos(usize, cycle_map.items, cycle_search_start + 1, cycle_search_want) orelse return error.NoCycleFound;

    const cycle_width = cycle_start - cycle_search_start;

    const req_cycles = 1000000000000;
    const t0_cycles = req_cycles - (req_cycles - cycle_start);
    const tn_cycles = req_cycles - t0_cycles;

    const full_cycles = @intCast(usize, tn_cycles) / cycle_width;
    const partial_cycles = @intCast(usize, tn_cycles) % cycle_width;

    const part2_ans =
        // from top of rock
        1 +
        // sum up to the start of the cycle
        sr.sum(usize, cycle_map.items[0..cycle_start]) +
        // simulate as many full cycles as req
        full_cycles * sr.sum(usize, cycle_map.items[cycle_start .. cycle_start + cycle_width]) +
        // then there will be a partial cycle
        sr.sum(usize, cycle_map.items[cycle_start .. cycle_start + partial_cycles]);

    try ps.solution(part2_ans);
}
