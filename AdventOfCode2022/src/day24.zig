const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Vec2i = sr.Vec2i;
const Vec2us = sr.Vec2us;
const Direction = sr.Direction;
const Tilemap = sr.Array2D(u8);
const SnowflakeSet = std.AutoArrayHashMap(Vec2i, void);

fn stepSnowflakes(snowflakes: []Snowflake, map: Tilemap) void {
    for (snowflakes) |*s| {
        const new_pos = s.pos.add(s.dir.toVectorYDown());
        if (map.atVec(new_pos.cast(Vec2us)) == '#') {
            switch (s.dir) {
                .south => s.pos.y = 1,
                .east => s.pos.x = 1,
                .north => s.pos.y = @intCast(i32, map.height) - 2,
                .west => s.pos.x = @intCast(i32, map.width) - 2,
            }
        } else {
            s.pos = new_pos;
        }
    }
}

fn makeSnowflakeSet(a: Allocator, snowflakes: []Snowflake) !SnowflakeSet {
    var set = SnowflakeSet.init(a);
    errdefer set.deinit();
    for (snowflakes) |s| {
        try set.put(s.pos, {});
    }
    return set;
}

const SnowflakeConfigurations = struct {
    configs: [][]Snowflake = &[_][]Snowflake{},
    sets: []SnowflakeSet = &[_]SnowflakeSet{},

    fn build(self: *SnowflakeConfigurations, a: Allocator, init_config: []Snowflake, map: Tilemap) !void {
        const num_configs = (map.width - 2) * (map.height - 2);
        self.configs = try a.alloc([]Snowflake, num_configs);
        self.configs[0] = try a.dupe(Snowflake, init_config);
        self.sets = try a.alloc(SnowflakeSet, num_configs);
        self.sets[0] = try makeSnowflakeSet(a, self.configs[0]);
        var i: usize = 1;
        while (i < self.configs.len) : (i += 1) {
            self.configs[i] = try a.dupe(Snowflake, self.configs[i - 1]);
            stepSnowflakes(self.configs[i], map);
            self.sets[i] = try makeSnowflakeSet(a, self.configs[i]);
        }
    }

    fn isSnowflakeAt(self: SnowflakeConfigurations, config: usize, pos: Vec2i) bool {
        return self.sets[config].contains(pos);
    }
};

const StateCacheKey = struct {
    pos: Vec2i,
    cycle: usize,
};

const PathState = struct {
    pos: Vec2i,
    steps: u32 = 0,

    fn key(self: PathState, configs: SnowflakeConfigurations) StateCacheKey {
        return StateCacheKey{
            .pos = self.pos,
            .cycle = (self.steps % configs.configs.len),
        };
    }

    fn fork(self: PathState, buf: *std.BoundedArray(PathState, 5), map: sr.Array2D(u8), configs: SnowflakeConfigurations) !void {
        buf.len = 0;

        // const new_snowflakes = configs.configs[(self.steps + 1) % configs.configs.len];
        const config = (self.steps + 1) % configs.configs.len;

        var dir_int: usize = 0;
        while (dir_int < 4) : (dir_int += 1) {
            const dir = @intToEnum(Direction, dir_int);
            const new_pos = self.pos.add(dir.toVectorYDown());
            if (new_pos.y < 0 or new_pos.y >= map.height or map.atVec(new_pos.cast(Vec2us)) == '#') {
                continue;
            }
            if (!configs.isSnowflakeAt(config, new_pos)) {
                try buf.append(PathState{
                    .pos = new_pos,
                    .steps = self.steps + 1,
                });
            }
        }
        // also consider not stepping
        if (!configs.isSnowflakeAt(config, self.pos)) {
            try buf.append(PathState{
                .pos = self.pos,
                .steps = self.steps + 1,
            });
        }
    }

    fn compare(goal: Vec2i, a: PathState, b: PathState) std.math.Order {
        return std.math.order(Vec2i.manhattan(a.pos, goal) catch unreachable, Vec2i.manhattan(b.pos, goal) catch unreachable);
    }
};

const PathingOptions = struct {
    configs: SnowflakeConfigurations,
    initial_config: usize = 0,
    flip_start_goal: bool = false,
};

fn findShortestPath(a: Allocator, map: sr.Array2D(u8), opts: PathingOptions) !?PathState {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa_a = gpa.allocator();

    var start = Vec2i.init(1, 0);
    var goal = Vec2us.init(map.width - 2, map.height - 1).cast(Vec2i);
    if (opts.flip_start_goal) {
        std.mem.swap(Vec2i, &start, &goal);
    }

    var look = std.PriorityQueue(PathState, Vec2i, PathState.compare).init(a, goal);
    try look.add(PathState{
        .pos = start,
    });

    // map of state:min steps, so we can cull states that are at this configuration with more steps
    var seen = std.AutoArrayHashMap(StateCacheKey, u32).init(gpa_a);
    var best_state: ?PathState = null;

    var adj_states = try std.BoundedArray(PathState, 5).init(0);

    while (look.len > 0) {
        var next = look.remove();
        if (next.steps > 500) {
            continue;
        }
        if (seen.get(next.key(opts.configs))) |score| {
            if (next.steps >= score) {
                continue;
            }
        }
        if (std.meta.eql(next.pos, goal)) {
            if (best_state) |s| {
                if (next.steps < s.steps) {
                    best_state = next;
                }
            } else {
                best_state = next;
            }
            continue;
        }
        try seen.put(next.key(opts.configs), next.steps);

        try next.fork(&adj_states, map, opts.configs);
        for (adj_states.slice()) |s| {
            try look.add(s);
        }
    }

    return best_state;
}

const Snowflake = struct {
    pos: Vec2i,
    dir: Direction,
};

const SnowflakeList = std.ArrayList(Snowflake);

fn charToDir(ch: u8) Direction {
    return switch (ch) {
        '>' => .east,
        '<' => .west,
        '^' => .north,
        'v' => .south,
        else => @panic("not a valid directional char"),
    };
}

fn extractSnowflakes(a: Allocator, map: sr.Array2D(u8)) ![]Snowflake {
    var y: usize = 0;
    var x: usize = 0;
    var list = SnowflakeList.init(a);
    errdefer list.deinit();
    while (y < map.height) : (y += 1) {
        x = 0;
        while (x < map.width) : (x += 1) {
            const pos_vec = Vec2us.init(x, y);
            const ch = map.atVec(pos_vec);
            switch (ch) {
                '>', '<', '^', 'v' => {
                    try list.append(Snowflake{ .pos = pos_vec.cast(sr.Vec2i), .dir = charToDir(ch) });
                    map.atVecPtr(pos_vec).* = '.';
                },
                else => {},
            }
        }
    }
    return try list.toOwnedSlice();
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var map = try sr.readTilemap(ps.allocator, ps.getPuzzleInputReader());
    var sf = try extractSnowflakes(ps.allocator, map);

    var configs = SnowflakeConfigurations{
        .configs = undefined,
    };
    try configs.build(ps.allocator, sf, map);

    const first_path = try findShortestPath(ps.allocator, map, .{ .configs = configs }) orelse return error.NoPath;

    try ps.solution(first_path.steps);

    const second_path = try findShortestPath(ps.allocator, map, .{
        .configs = configs,
        .flip_start_goal = true,
        .initial_config = first_path.key(configs).cycle,
    }) orelse return error.NoPath;

    const third_path = try findShortestPath(ps.allocator, map, .{
        .configs = configs,
        .initial_config = second_path.key(configs).cycle,
    }) orelse return error.NoPath;

    try ps.solution(first_path.steps + second_path.steps + third_path.steps);
}

fn printState(s: PathState, m: Tilemap) void {
    var y: usize = 0;
    var x: usize = 0;
    while (y < m.height) : (y += 1) {
        x = 0;
        while (x < m.width) : (x += 1) {
            if (m.at(x, y) == '#') {
                std.debug.print("#", .{});
            } else if (std.meta.eql(s.pos, Vec2us.init(x, y).cast(Vec2i))) {
                std.debug.print("E", .{});
            } else {
                var cnt_snowflakes: usize = 0;
                for (s.snowflakes) |snow| {
                    if (std.meta.eql(snow.pos, Vec2us.init(x, y).cast(Vec2i))) {
                        cnt_snowflakes += 1;
                    }
                }
                if (cnt_snowflakes > 0) {
                    std.debug.print("{d}", .{cnt_snowflakes});
                } else {
                    std.debug.print(".", .{});
                }
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}
