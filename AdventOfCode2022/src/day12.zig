const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Direction = sr.Direction;
const Tilemap = sr.Array2D(u8);
const TileCoord = sr.Vec2us;

fn isValidMove(tm: Tilemap, current: sr.Vec2i, dir: Direction) bool {
    const new = current.add(dir.toVector());

    if (!tm.inBoundsSignedVec(new.cast(sr.Vec2is))) {
        return false;
    }

    var s = tm.atVec(current.cast(sr.Vec2us));
    var d = tm.atVec(new.cast(sr.Vec2us));

    return d -| s <= 1;
}

const PathingCoords = struct {
    start: TileCoord,
    end: TileCoord,

    fn init(start: TileCoord, end: TileCoord) PathingCoords {
        return .{
            .start = start,
            .end = end,
        };
    }
};

const PathfindingCache = struct {
    arena: std.heap.ArenaAllocator,
    entries: std.AutoArrayHashMapUnmanaged(PathingCoords, []TileCoord) = .{},

    fn init(allocator: Allocator) PathfindingCache {
        _ = allocator;
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        return .{
            .arena = arena,
        };
    }

    fn deinit(self: *PathfindingCache) void {
        self.arena.deinit();
    }

    fn reserve(self: *PathfindingCache, coord_count: usize) !void {
        var allocator = self.arena.allocator();
        try self.entries.ensureTotalCapacity(allocator, coord_count);
    }

    fn clear(self: *PathfindingCache) void {
        const my_cap = self.entries.capacity();
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var allocator = self.arena.allocator();
        self.entries = .{};
        self.entries.ensureTotalCapacity(allocator, my_cap) catch unreachable;
    }

    fn setExpandPath(self: *PathfindingCache, coords: PathingCoords, path: []TileCoord) void {
        if (path.len == 0) {
            self.entries.putAssumeCapacity(coords, &[_]TileCoord{});
            return;
        }

        var my_path = self.copyPath(path) catch unreachable;
        for (my_path) |start, i| {
            const key = PathingCoords{
                .start = start,
                .end = coords.end,
            };
            var gop = self.entries.getOrPutAssumeCapacity(key);
            if (!gop.found_existing) {
                gop.value_ptr.* = my_path[i..];
            }
        }
    }

    fn get(self: *PathfindingCache, coords: PathingCoords) ?[]TileCoord {
        return self.entries.get(coords);
    }

    fn hasPathFrom(self: *PathfindingCache, coords: PathingCoords) bool {
        return self.entries.contains(coords);
    }

    fn copyPath(self: *PathfindingCache, path: []TileCoord) ![]TileCoord {
        var allocator = self.arena.allocator();
        return try allocator.dupe(TileCoord, path);
    }
};

const PathfindingState = struct {
    const Score = struct {
        fscore: f32,
        gscore: f32,
        from: TileCoord,

        const infinity = Score{
            .fscore = std.math.inf_f32,
            .gscore = std.math.inf_f32,
            .from = undefined,
        };
    };

    const Context = struct {
        map: *const Tilemap,
        score_map: []Score,
    };

    allocator: Allocator,
    frontier: std.PriorityQueue(TileCoord, Context, orderFScore),
    frontier_set: std.AutoArrayHashMapUnmanaged(TileCoord, void),
    score_map: []Score,
    // won't know length until we walk the result, though upper bound is tile map size
    result: std.ArrayListUnmanaged(TileCoord),

    fn init(allocator: Allocator) PathfindingState {
        return PathfindingState{
            .allocator = allocator,
            .frontier = std.PriorityQueue(TileCoord, Context, orderFScore).init(allocator, undefined),
            .frontier_set = .{},
            .score_map = &[_]Score{},
            .result = .{},
        };
    }

    fn deinit(self: *PathfindingState) void {
        self.frontier.deinit();
        self.frontier_set.deinit(self.allocator);
        if (self.score_map.len != 0) {
            self.allocator.free(self.score_map);
        }
        self.result.deinit(self.allocator);
    }

    fn reserve(self: *PathfindingState, count: usize) !void {
        try self.frontier.ensureTotalCapacity(count);
        try self.frontier_set.ensureTotalCapacity(self.allocator, count);
        if (self.score_map.len != 0) {
            self.allocator.free(self.score_map);
        }
        self.score_map = try self.allocator.alloc(Score, count);
    }

    fn orderFScore(ctx: Context, lhs: TileCoord, rhs: TileCoord) std.math.Order {
        return std.math.order(
            ctx.score_map[ctx.map.scalarIndexVec(lhs)].fscore,
            ctx.score_map[ctx.map.scalarIndexVec(rhs)].fscore,
        );
    }

    fn findPath(self: *PathfindingState, coords: PathingCoords, map: Tilemap, cache: ?*PathfindingCache) !bool {
        // sus, upstream interface needs some love
        self.frontier.len = 0;
        self.frontier.context = Context{
            .map = &map,
            .score_map = self.score_map,
        };

        self.frontier_set.clearRetainingCapacity();

        try self.frontier.add(coords.start);
        try self.frontier_set.put(self.allocator, coords.start, {});
        std.mem.set(Score, self.score_map, Score.infinity);

        self.score_map[map.scalarIndexVec(coords.start)] = .{
            .fscore = 0,
            .gscore = 0,
            .from = undefined,
        };

        while (self.frontier.removeOrNull()) |current| {
            if (std.meta.eql(current, coords.end)) {
                if (cache) |c| {
                    self.result.clearRetainingCapacity();
                    var coord = coords.end;
                    while (!std.meta.eql(coord, coords.start)) {
                        try self.result.append(self.allocator, coord);
                        coord = self.score_map[map.scalarIndexVec(coord)].from;
                    }
                    try self.result.append(self.allocator, coord);
                    std.mem.reverse(TileCoord, self.result.items);
                    c.setExpandPath(coords, self.result.items);
                }
                return true;
            }

            // examine neighbors
            var d_int: u8 = 0;
            while (d_int < 4) : (d_int += 1) {
                const dir = @intToEnum(Direction, d_int);
                if (isValidMove(map, current.cast(sr.Vec2i), dir)) {
                    // 1 here is the graph edge weight, basically hardcoding manhattan distance
                    const tentative_score = self.score_map[map.scalarIndexVec(current)].gscore + 1;
                    const neighbor = current.offsetWrap(dir);
                    const neighbor_score = self.score_map[map.scalarIndexVec(neighbor)].gscore;
                    if (tentative_score < neighbor_score) {
                        self.score_map[map.scalarIndexVec(neighbor)] = .{
                            .from = current,
                            .gscore = tentative_score,
                            .fscore = tentative_score + self.heuristic(neighbor, coords.end),
                        };
                        if (!self.frontier_set.contains(neighbor)) {
                            try self.frontier_set.put(self.allocator, neighbor, {});
                            try self.frontier.add(neighbor);
                        }
                    }
                }
            }
        }

        if (cache) |c| {
            c.setExpandPath(coords, &[_]TileCoord{});
        }

        return false;
    }

    fn heuristic(self: *PathfindingState, from: TileCoord, to: TileCoord) f32 {
        _ = self;
        return sr.vectorEuclidean(from.cast(sr.Vec2f), to.cast(sr.Vec2f));
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var tm = try sr.readTilemap(ps.allocator, ps.getPuzzleInputReader());
    const start_i = std.mem.indexOfScalar(u8, tm.data, 'S').?;
    const end_i = std.mem.indexOfScalar(u8, tm.data, 'E').?;
    tm.data[start_i] = 'a';
    tm.data[end_i] = 'z';

    const end_tc = TileCoord{
        .x = end_i % tm.width,
        .y = end_i / tm.width,
    };

    var p = PathfindingState.init(ps.allocator);
    var c = PathfindingCache.init(ps.allocator);
    try c.reserve(tm.width * tm.height);
    try p.reserve(tm.width * tm.height);

    var y: usize = 0;
    var x: usize = 0;
    var min: usize = 1000000;
    var s: usize = 0;
    while (y < tm.height) : (y += 1) {
        x = 0;
        while (x < tm.width) : (x += 1) {
            if (tm.at(x, y) == 'a') {
                const start_tc = TileCoord{
                    .x = x,
                    .y = y,
                };
                if (try p.findPath(PathingCoords.init(start_tc, end_tc), tm, &c)) {
                    const path = c.get(PathingCoords.init(start_tc, end_tc));
                    if (tm.scalarIndex(x, y) == start_i) {
                        s = path.?.len - 1;
                    }
                    min = std.math.min(min, path.?.len - 1);
                }
            }
        }
    }
    try ps.solution(s);
    try ps.solution(min);
}

test {
    _ = sr;
}
