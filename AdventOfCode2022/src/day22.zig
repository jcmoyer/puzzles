const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Vec2i = sr.vector.Vec2(i32);

fn isValidTilemapChar(ch: u8) bool {
    return ch == ' ' or ch == '#' or ch == '.';
}

fn isNum(ch: u8) bool {
    return (ch >= '0' and ch <= '9');
}

fn isValidInstrChar(ch: u8) bool {
    return (ch >= '0' and ch <= '9') or ch == 'L' or ch == 'R';
}

const Instruction = union(enum) {
    move: u32,
    turn_left,
    turn_right,
};

fn parseInstructions(a: Allocator, line: []const u8) ![]Instruction {
    var xs = std.ArrayList(Instruction).init(a);
    errdefer xs.deinit();
    var sc = sr.Scanner.init(line);
    while (!sc.atEnd()) {
        if (isNum(sc.peek().?)) {
            var n = try sc.readInt(u32);
            try xs.append(Instruction{ .move = n });
        } else {
            var turn = sc.readOne().?;
            if (turn == 'L') {
                try xs.append(.turn_left);
            } else {
                try xs.append(.turn_right);
            }
        }
    }
    return try xs.toOwnedSlice();
}

fn facingScore(d: sr.Direction) i32 {
    return switch (d) {
        .east => 0,
        .south => 1,
        .west => 2,
        .north => 3,
    };
}

const Region = struct {
    min: sr.Vec2i,
    max: sr.Vec2i,
    adjacencies: [4]usize,

    fn contains(self: Region, v: sr.Vec2i) bool {
        return v.x >= self.min.x and v.y >= self.min.y and v.x <= self.max.x and v.y <= self.max.y;
    }

    fn isOnEdge(self: Region, v: sr.Vec2i) bool {
        return v.x == self.min.x or v.y == self.min.y or v.x == self.max.x or v.y == self.max.y;
    }

    fn getEdge(self: Region, d: sr.Direction) i32 {
        return switch (d) {
            .west => self.min.x,
            .east => self.max.x,
            .north => self.min.y,
            .south => self.max.y,
        };
    }

    /// Distance from left
    fn relativeX(self: Region, v: sr.Vec2i) i32 {
        std.debug.assert(self.contains(v));
        return v.x - self.min.x;
    }

    /// Distance from right
    fn invRelativeX(self: Region, v: sr.Vec2i) i32 {
        std.debug.assert(self.contains(v));
        return self.max.x - v.x;
    }

    fn width(self: Region) i32 {
        return 1 + self.max.x - self.min.x;
    }

    fn relativeY(self: Region, v: sr.Vec2i) i32 {
        std.debug.assert(self.contains(v));
        return v.y - self.min.y;
    }

    /// Distance from bottom
    fn invRelativeY(self: Region, v: sr.Vec2i) i32 {
        std.debug.assert(self.contains(v));
        return self.max.y - v.y;
    }

    fn directionTo(self: Region, region_id: usize) sr.Direction {
        var dir_int: usize = 0;
        while (dir_int < 4) : (dir_int += 1) {
            const dir = @intToEnum(sr.Direction, dir_int);
            if (self.adjacencies[dir_int] == region_id) {
                return dir;
            }
        }
        @panic("Region.directionTo: region not connected to region_id!");
    }
};

fn fillRegionsMyInput(regions: *[6]Region) !void {
    //  01
    //  2
    // 43
    // 5
    regions[0] = .{
        .min = .{ .x = 50, .y = 0 },
        .max = .{ .x = 50 + 49, .y = 49 },
        .adjacencies = .{ 5, 1, 2, 4 },
    };
    regions[1] = .{
        .min = .{ .x = 100, .y = 0 },
        .max = .{ .x = 100 + 49, .y = 49 },
        .adjacencies = .{ 5, 3, 2, 0 },
    };
    regions[2] = .{
        .min = .{ .x = 50, .y = 50 },
        .max = .{ .x = 50 + 49, .y = 50 + 49 },
        .adjacencies = .{ 0, 1, 3, 4 },
    };
    regions[3] = .{
        .min = .{ .x = 50, .y = 100 },
        .max = .{ .x = 50 + 49, .y = 100 + 49 },
        .adjacencies = .{ 2, 1, 5, 4 },
    };
    regions[4] = .{
        .min = .{ .x = 0, .y = 100 },
        .max = .{ .x = 0 + 49, .y = 100 + 49 },
        .adjacencies = .{ 2, 3, 5, 0 },
    };
    regions[5] = .{
        .min = .{ .x = 0, .y = 150 },
        .max = .{ .x = 0 + 49, .y = 150 + 49 },
        .adjacencies = .{ 4, 3, 1, 0 },
    };
}

const CubeMap = struct {
    tilemap: sr.Array2D(u8),
    regions: [6]Region,

    fn path(self: *CubeMap, instrs: []Instruction) i64 {
        var pos = self.findSpawnPosition();
        var dir = sr.Direction.east;
        var current_region: usize = 0;

        // var f = std.fs.cwd().createFile("output.txt", .{}) catch unreachable;
        // defer f.close();

        for (instrs) |instr| {
            std.debug.print("BEGIN {any}\n", .{instr});
            // f.writer().print("BEGIN {any}\n", .{instr}) catch unreachable;
            switch (instr) {
                .move => |n| {
                    var i: u32 = 0;
                    while (i < n) : (i += 1) {
                        // printVicinity(self.tilemap, pos, dir, f.writer()) catch unreachable;

                        const want_pos = pos.add(dir.toVectorYDown());
                        if (self.regions[current_region].contains(want_pos)) {

                            // if (self.regions[current_region])
                            if (self.tilemap.atVec(want_pos.cast(sr.Vec2us)) == '#') {
                                break;
                            } else {
                                std.debug.print("move ({d}/{d}) to {any}\n", .{ i + 1, n, want_pos });
                                std.debug.print("  (relative in region: {d},{d})\n", .{
                                    self.regions[current_region].relativeX(want_pos),
                                    self.regions[current_region].relativeY(want_pos),
                                });

                                pos = want_pos;
                            }
                        } else {
                            const new_region = self.regions[current_region].adjacencies[@enumToInt(dir)];

                            const result = self.transferRegion(pos, dir, current_region, new_region);
                            std.debug.print("{any} r {d} to {d}\n", .{ result, current_region, new_region });

                            if (self.tilemap.atVec(result.new_pos.cast(sr.Vec2us)) == '#') {
                                std.debug.print("wanted to change region {d} to {d} but blocked\n", .{ current_region, new_region });
                                break;
                            } else {
                                std.debug.print("transfer-move ({d}/{d}) to {any}, dir={any}\n", .{ i + 1, n, result.new_pos, result.new_dir });
                                std.debug.print("  (relative in region: {d},{d})\n", .{
                                    self.regions[new_region].relativeX(result.new_pos),
                                    self.regions[new_region].relativeY(result.new_pos),
                                });
                                pos = result.new_pos;
                                dir = result.new_dir;
                            }

                            std.debug.print("changed region {d} to {d}\n", .{ current_region, new_region });
                            current_region = new_region;
                        }
                    }
                },
                .turn_left => {
                    dir = dir.rotateCCW();
                },
                .turn_right => {
                    dir = dir.rotateCW();
                },
            }
        }

        return ((pos.y + 1) * 1000) + ((pos.x + 1) * 4) + facingScore(dir);
    }

    const TransferRegionResult = struct {
        new_pos: Vec2i,
        new_dir: sr.Direction,

        fn withPos(self: TransferRegionResult, p: Vec2i) TransferRegionResult {
            return TransferRegionResult{ .new_pos = p, .new_dir = self.new_dir };
        }
    };

    fn transferRegion(self: CubeMap, pos: Vec2i, dir: sr.Direction, from: usize, to: usize) TransferRegionResult {
        const backdir = self.regions[to].directionTo(from);

        const r_from = self.regions[from];
        const r_to = self.regions[to];

        var result = TransferRegionResult{
            .new_pos = undefined,
            .new_dir = backdir.opposite(),
        };

        if (dir == backdir and dir.isVertical()) {
            return result.withPos(.{
                .x = r_to.max.x - r_from.relativeX(pos),
                .y = r_to.getEdge(backdir),
            });
        }

        if (dir == .east and backdir == .north) {
            return result.withPos(.{
                .x = r_to.max.x - r_from.relativeY(pos),
                .y = r_to.getEdge(.north),
            });
        } else if (dir == .east and backdir == .south) {
            return result.withPos(.{
                .x = r_to.min.x + r_from.relativeY(pos),
                .y = r_to.getEdge(.south),
            });
        } else if (dir == .north and backdir == .west) {
            return result.withPos(.{
                .x = r_to.getEdge(.west),
                .y = r_to.min.y + r_from.relativeX(pos),
            });
        } else if (dir == .west and backdir == .north) {
            return result.withPos(.{
                .x = r_to.min.x + r_from.relativeY(pos),
                .y = r_to.getEdge(.north),
            });
        } else if (dir == .south and backdir == .east) {
            return result.withPos(.{
                .x = r_to.getEdge(.east),
                .y = r_to.min.y + r_from.relativeX(pos),
            });
        } else if (dir == .east and backdir == .east) {
            return result.withPos(.{
                .x = r_to.getEdge(.east),
                .y = r_to.min.y + r_from.invRelativeY(pos),
            });
        } else if (dir == .west and backdir == .west) {
            return result.withPos(.{
                .x = r_to.getEdge(.west),
                .y = r_to.min.y + r_from.invRelativeY(pos),
            });
        } else if (dir == .south and backdir == .north) {
            return result.withPos(.{
                .x = r_to.min.x + r_from.relativeX(pos),
                .y = r_to.getEdge(.north),
            });
        } else if (dir == .north and backdir == .south) {
            return result.withPos(.{
                .x = r_to.min.x + r_from.relativeX(pos),
                .y = r_to.getEdge(.south),
            });
        } else if (dir == .east and backdir == .west) {
            return result.withPos(.{
                .x = r_to.getEdge(.west),
                .y = r_to.min.y + r_from.relativeY(pos),
            });
        } else if (dir == .west and backdir == .east) {
            return result.withPos(.{
                .x = r_to.getEdge(.east),
                .y = r_to.min.y + r_from.relativeY(pos),
            });
        }

        std.debug.print("transferRegion not implemented for dir={any} -> backdir={any}\n", .{ dir, backdir });
        std.debug.print("from={any}, to={any}\n", .{ from, to });

        unreachable;
    }

    fn findRegionForPosition(self: CubeMap, pos: Vec2i) usize {
        for (self.regions) |r, i| {
            if (r.contains(pos)) {
                return i;
            }
        }
        @panic("no region for position");
    }

    fn findSpawnPosition(self: CubeMap) Vec2i {
        const x = std.mem.indexOfScalar(u8, self.tilemap.rowSlice(0), '.').?;
        return Vec2i.init(@intCast(i32, x), 0);
    }
};

const Input = struct {
    tilemap: sr.Array2D(u8),
    instructions: []Instruction,

    fn deinit(self: *Input, allocator: Allocator) void {
        self.tilemap.deinit(allocator);
        allocator.free(self.instructions);
    }

    fn loadFromSlice(allocator: Allocator, text: []const u8) !Input {
        var instrs: []Instruction = &[_]Instruction{};
        errdefer if (instrs.len != 0) allocator.free(instrs);

        var width: usize = 0;
        var height: usize = 0;

        var lines = sr.sliceLines(text);
        while (lines.next()) |line| {
            if (line.len > 0) {
                if (isValidTilemapChar(line[0])) {
                    width = std.math.max(width, line.len);
                    height += 1;
                } else {
                    instrs = try parseInstructions(allocator, line);
                }
            }
        }

        lines = sr.sliceLines(text);
        var tilemap = sr.Array2D(u8){};
        errdefer tilemap.deinit(allocator);

        try tilemap.resize(allocator, width, height);
        tilemap.fill(' ');
        var current_row: usize = 0;
        while (lines.next()) |line| {
            if (line.len > 0) {
                if (isValidTilemapChar(line[0])) {
                    std.mem.copy(u8, tilemap.rowSlice(current_row), line);
                    current_row += 1;
                }
            }
        }

        return Input{
            .tilemap = tilemap,
            .instructions = instrs,
        };
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var input = try Input.loadFromSlice(ps.allocator, ps.input_text);

    var cube = CubeMap{
        .tilemap = input.tilemap,
        .regions = undefined,
    };
    try fillRegionsMyInput(&cube.regions);

    const score = cube.path(input.instructions);

    try ps.solution(score);
}

fn printVicinity(tilemap: sr.Array2D(u8), pos: Vec2i, dir: sr.Direction, writer: anytype) !void {
    const y_min = @intCast(usize, std.math.max(0, pos.y - 5));
    const y_max = @intCast(usize, std.math.min(@intCast(i32, tilemap.height - 1), pos.y + 5));
    const x_min = @intCast(usize, std.math.max(0, pos.x - 5));
    const x_max = @intCast(usize, std.math.min(@intCast(i32, tilemap.width - 1), pos.x + 5));

    var y: usize = y_min;
    var x: usize = x_min;
    while (y <= y_max) : (y += 1) {
        x = x_min;
        while (x <= x_max) : (x += 1) {
            var p = pos.cast(sr.Vec2us);
            if (p.x == x and p.y == y) {
                switch (dir) {
                    .east => _ = try writer.writeByte('>'),
                    .west => _ = try writer.writeByte('<'),
                    .north => _ = try writer.writeByte('^'),
                    .south => _ = try writer.writeByte('v'),
                }
            } else {
                try writer.print("{c}", .{tilemap.at(x, y)});
            }
        }
        try writer.print("\n", .{});
    }
    try writer.print("\n", .{});
}

fn printWorld(tilemap: sr.Array2D(u8), pos: Vec2i, dir: sr.Direction, writer: anytype) !void {
    var y: usize = 0;
    var x: usize = 0;
    while (y < tilemap.height) : (y += 1) {
        x = 0;
        while (x < tilemap.width) : (x += 1) {
            var p = pos.cast(sr.Vec2us);

            if (p.x == x and p.y == y) {
                switch (dir) {
                    .east => _ = try writer.writeByte('>'),
                    .west => _ = try writer.writeByte('<'),
                    .north => _ = try writer.writeByte('^'),
                    .south => _ = try writer.writeByte('v'),
                }
            } else {
                try writer.print("{c}", .{tilemap.at(x, y)});
            }
        }
        try writer.print("\n", .{});
    }
    try writer.print("\n", .{});
}

const example_input =
    \\        ...#
    \\        .#..
    \\        #...
    \\        ....
    \\...#.......#
    \\........#...
    \\..#....#....
    \\..........#.
    \\        ...#....
    \\        .....#..
    \\        .#......
    \\        ......#.
    \\
    \\10R5L5R10L4R5L5
;

test "CubeMap with example input" {
    var input = try Input.loadFromSlice(std.testing.allocator, example_input);
    defer input.deinit(std.testing.allocator);

    var cube = CubeMap{
        .tilemap = input.tilemap,
        .regions = undefined,
    };
    // north,
    // east,
    // south,
    // west,
    cube.regions[0].min = Vec2i.init(8, 0);
    cube.regions[0].max = cube.regions[0].min.add(Vec2i.init(3, 3));
    cube.regions[0].adjacencies = .{ 1, 5, 3, 2 };

    cube.regions[1].min = Vec2i.init(0, 4);
    cube.regions[1].max = cube.regions[1].min.add(Vec2i.init(3, 3));
    cube.regions[1].adjacencies = .{ 0, 2, 4, 5 };

    cube.regions[2].min = Vec2i.init(4, 4);
    cube.regions[2].max = cube.regions[2].min.add(Vec2i.init(3, 3));
    cube.regions[2].adjacencies = .{ 0, 3, 4, 1 };

    cube.regions[3].min = Vec2i.init(8, 4);
    cube.regions[3].max = cube.regions[3].min.add(Vec2i.init(3, 3));
    cube.regions[3].adjacencies = .{ 0, 5, 4, 2 };

    cube.regions[4].min = Vec2i.init(8, 8);
    cube.regions[4].max = cube.regions[4].min.add(Vec2i.init(3, 3));
    cube.regions[4].adjacencies = .{ 3, 5, 1, 2 };

    cube.regions[5].min = Vec2i.init(12, 8);
    cube.regions[5].max = cube.regions[5].min.add(Vec2i.init(3, 3));
    cube.regions[5].adjacencies = .{ 3, 0, 1, 4 };

    const score = cube.path(input.instructions);

    try std.testing.expect(score == 5031);
}
