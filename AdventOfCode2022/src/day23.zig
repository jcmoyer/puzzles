const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const ElfMap = std.AutoArrayHashMap(Vec2i, Elf);
const Vec2i = sr.Vec2i;
const Direction = sr.Direction;

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

fn computeBounds(m: ElfMap) Bounds {
    var b: Bounds = .{};
    for (m.keys()) |p| {
        b.min.x = std.math.min(b.min.x, p.x);
        b.min.y = std.math.min(b.min.y, p.y);
        b.max.x = std.math.max(b.max.x, p.x);
        b.max.y = std.math.max(b.max.y, p.y);
    }
    return b;
}

const Elf = struct {
    pos: Vec2i,
    consider_order: [4]sr.Direction = .{ .north, .south, .west, .east },
    // will_move: bool = true,
    proposal: ?sr.Vec2i = null,
    name: u8 = 0,

    fn advanceConsiderOrder(self: *Elf) void {
        std.mem.rotate(sr.Direction, &self.consider_order, 1);
    }

    fn getProposal(self: *Elf, m: ElfMap) ?sr.Vec2i {
        if (countAdjacent(m, self.pos) == 0) {
            // std.debug.print("{c} counted nothing\n", .{ self.name });
            self.proposal = null;
            return null;
        }
        for (self.consider_order) |dir| {
            // const cnt = countAdjacentDirection(m, self.pos, dir);
            // std.debug.print("{c} counted {d} to {any}\n", .{ self.name, cnt, dir });

            if (countAdjacentDirection(m, self.pos, dir) == 0) {
                self.proposal = self.pos.add(dir.toVector());
                // std.debug.print("{c} proposes {any} {any}\n", .{ self.name, dir, self.proposal });
                return self.proposal;
            }
        }
        // unreachable;
        self.proposal = null;
        return null;
    }
};

fn countAdjacentDirection(m: ElfMap, pos: Vec2i, dir: Direction) usize {
    return switch (dir) {
        .north, .south => countHorizontal(m, pos.add(dir.toVector()), 1),
        .west, .east => countVertical(m, pos.add(dir.toVector()), 1),
    };
}

fn countAdjacent(m: ElfMap, pos: Vec2i) usize {
    var x: i32 = -1;
    var y: i32 = -1;
    var count: usize = 0;
    while (y <= 1) : (y += 1) {
        x = -1;
        while (x <= 1) : (x += 1) {
            if (x == 0 and y == 0) continue;
            const key = pos.add(Vec2i.init(x, y));
            if (m.contains(key)) {
                count += 1;
            }
        }
    }
    return count;
}

fn countHorizontal(m: ElfMap, pos: Vec2i, radius: i32) usize {
    var x: i32 = -radius;
    var count: usize = 0;
    while (x <= radius) : (x += 1) {
        const key = pos.add(Vec2i.init(x, 0));
        if (m.contains(key)) {
            count += 1;
        }
    }
    return count;
}

fn countVertical(m: ElfMap, pos: Vec2i, radius: i32) usize {
    var y: i32 = -radius;
    var count: usize = 0;
    while (y <= radius) : (y += 1) {
        const key = pos.add(Vec2i.init(0, y));
        if (m.contains(key)) {
            count += 1;
        }
    }
    return count;
}

fn step(a: Allocator, m: *ElfMap) !usize {
    var moves: usize = 0;

    var proposal_map = std.AutoArrayHashMap(Vec2i, u16).init(a);
    defer proposal_map.deinit();

    var new_map = ElfMap.init(a);

    for (m.values()) |*elf| {
        if (elf.getProposal(m.*)) |pos| {
            var gop = try proposal_map.getOrPut(pos);
            if (gop.found_existing) {
                gop.value_ptr.* += 1;
            } else {
                gop.value_ptr.* = 1;
            }
        } else {
            try new_map.put(elf.pos, elf.*);
        }
    }

    for (proposal_map.keys()) |vec| {
        if (proposal_map.get(vec).? == 1) {
            var it = m.iterator();
            while (it.next()) |ent| {
                if (ent.value_ptr.proposal != null and ent.value_ptr.proposal.?.x == vec.x and ent.value_ptr.proposal.?.y == vec.y) {
                    var new_elf = ent.value_ptr.*;
                    new_elf.pos = vec;
                    try new_map.putNoClobber(vec, new_elf);
                    moves += 1;
                    break;
                }
            }
        } else {
            var it = m.iterator();
            while (it.next()) |ent| {
                if (ent.value_ptr.proposal != null and ent.value_ptr.proposal.?.x == vec.x and ent.value_ptr.proposal.?.y == vec.y) {
                    var new_elf = ent.value_ptr.*;
                    try new_map.putNoClobber(new_elf.pos, new_elf);
                }
            }
        }
    }

    for (new_map.values()) |*elf| {
        elf.advanceConsiderOrder();
    }

    std.debug.assert(new_map.count() == m.count());

    m.* = new_map;
    return moves;
}

fn print(m: ElfMap) !void {
    const b = computeBounds(m);

    var y: i32 = b.max.y;
    var x: i32 = b.min.x;
    while (y >= b.min.y) : (y -= 1) {
        x = b.min.x;
        while (x <= b.max.x) : (x += 1) {
            if (m.get(Vec2i.init(x, y))) |elf| {
                std.debug.print("{c}", .{elf.name});
            } else {
                std.debug.print(".", .{});
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

fn countTiles(m: ElfMap) usize {
    const b = computeBounds(m);
    var sum: usize = 0;

    var y: i32 = b.max.y;
    var x: i32 = b.min.x;
    while (y >= b.min.y) : (y -= 1) {
        x = b.min.x;
        while (x <= b.max.x) : (x += 1) {
            if (!m.contains(Vec2i.init(x, y))) {
                sum += 1;
            }
        }
    }
    return sum;
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var map = try sr.readTilemap(ps.allocator, ps.getPuzzleInputReader());

    var sparse = ElfMap.init(ps.allocator);
    var x: usize = 0;
    var y: usize = 0;
    var name: u8 = 'A';
    while (y < map.height) {
        x = 0;
        while (x < map.width) {
            if (map.at(x, y) == '#') {
                const int_pos = Vec2i.init(@intCast(i32, x), -@intCast(i32, y));
                try sparse.put(int_pos, Elf{
                    .pos = int_pos,
                    .name = name,
                });
                name = 'X';
            }
            x += 1;
        }
        y += 1;
    }

    // try print(sparse);
    var steps: usize = 1;

    var p1: usize = 0;
    var p2: usize = 0;

    while (true) {
        // std.debug.print("step {d}:\n\n", .{steps});

        const cnt = try step(ps.allocator, &sparse);

        // std.debug.print("\n", .{});

        // try print(sparse);

        if (steps == 10) {
            p1 = countTiles(sparse);
        }

        if (cnt == 0) {
            p2 = steps;
            break;
        }
        steps += 1;
    }

    try ps.solution(p1);
    try ps.solution(p2);
}
