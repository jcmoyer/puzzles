const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

// https://oeis.org/A000124 Central polygonal numbers (the Lazy Caterer's
// sequence): n(n+1)/2 + 1; or, maximal number of pieces formed when slicing a
// pancake with n cuts.
//
// Higher values clamped to the range of u8
const central_polygonal_numbers = [32]u8{ 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121, 137, 154, 172, 191, 211, 232, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 };

const Blueprint = struct {
    id: u32 = 0,

    /// Ore robot, ore cost
    r_ore_ore: u8 = 0,
    /// Clay robot, ore cost
    r_clay_ore: u8 = 0,
    /// Obsidian robot, ore and clay costs
    r_obsid_ore: u8 = 0,
    r_obsid_clay: u8 = 0,
    /// Geode robot, ore and obsidian costs
    r_geode_ore: u8 = 0,
    r_geode_obsid: u8 = 0,
};

// none of these fields exceed u8 bounds, so we try to reduce memory consumption as much as possible
const Pathfinder = struct {
    time: u8,

    r_ore: u8 = 1,
    r_clay: u8 = 0,
    r_obsid: u8 = 0,
    r_geode: u8 = 0,

    /// Resource totals
    ore: u8 = 0,
    clay: u8 = 0,
    obsidian: u8 = 0,
    geode: u8 = 0,

    fn incremented(self: Pathfinder) Pathfinder {
        return .{
            .time = self.time - 1,
            .r_ore = self.r_ore,
            .r_clay = self.r_clay,
            .r_obsid = self.r_obsid,
            .r_geode = self.r_geode,
            .ore = self.ore + self.r_ore,
            .clay = self.clay + self.r_clay,
            .obsidian = self.obsidian + self.r_obsid,
            .geode = self.geode + self.r_geode,
        };
    }

    fn findMaxGeodes(a: Allocator, bp: Blueprint, comptime max_time: u8) !u32 {
        var buf = std.ArrayList(Pathfinder).init(a);
        defer buf.deinit();
        try buf.append(Pathfinder{ .time = max_time });
        var seen = std.AutoArrayHashMap(Pathfinder, void).init(a);
        defer seen.deinit();
        var best: u8 = 0;

        while (buf.items.len > 0) {
            var next = buf.pop();
            if (seen.contains(next) or !next.canCompete(best)) {
                continue;
            }
            try seen.put(next, {});
            try next.adjacentStates(bp, &buf);
            best = std.math.max(best, next.geode);
        }

        return best;
    }

    fn canCompete(self: Pathfinder, target_geodes: u8) bool {
        var max_we_could_get = self.geode + central_polygonal_numbers[self.time + self.r_geode - 1];
        return max_we_could_get > target_geodes;
    }

    fn adjacentStates(self: Pathfinder, bp: Blueprint, buf: *std.ArrayList(Pathfinder)) !void {
        if (self.time == 0) {
            return;
        }

        const max_bots_ore = self.ore / bp.r_ore_ore;
        const max_bots_clay = self.ore / bp.r_clay_ore;
        const max_bots_obsid = std.math.min(self.ore / bp.r_obsid_ore, self.clay / bp.r_obsid_clay);
        const max_bots_geode = std.math.min(self.ore / bp.r_geode_ore, self.obsidian / bp.r_geode_obsid);

        // time checks inserted here to avoid using too much memory
        const inc = self.incremented();
        if (inc.time > 0) {
            try buf.append(inc);
        }
        if (max_bots_ore > 0) {
            var with_ore_bot = self.incremented();
            with_ore_bot.ore -= bp.r_ore_ore;
            with_ore_bot.r_ore += 1;
            try buf.append(with_ore_bot);
        }
        if (max_bots_clay > 0) {
            var with_clay_bot = self.incremented();
            with_clay_bot.ore -= bp.r_clay_ore;
            with_clay_bot.r_clay += 1;
            try buf.append(with_clay_bot);
        }
        if (max_bots_obsid > 0) {
            var with_obsid_bot = self.incremented();
            with_obsid_bot.ore -= bp.r_obsid_ore;
            with_obsid_bot.clay -= bp.r_obsid_clay;
            with_obsid_bot.r_obsid += 1;
            try buf.append(with_obsid_bot);
        }
        // it's important this comes last, we want to search geode building
        // states first (note this goes onto the end of the stack so it will be processed next)
        // so that we can cull as many possible future states with canCompete
        if (max_bots_geode > 0) {
            var with_geode_bot = self.incremented();
            with_geode_bot.ore -= bp.r_geode_ore;
            with_geode_bot.obsidian -= bp.r_geode_obsid;
            with_geode_bot.r_geode += 1;
            try buf.append(with_geode_bot);
        }
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    // can't use arena for this, too much memory
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var sum: u32 = 0;
    var prod: u32 = 1;

    var lines = sr.sliceLines(ps.input_text);
    var i: usize = 0;
    while (lines.next()) |line| {
        var bp = Blueprint{};

        _ = try sr.parse(
            "Blueprint {d}: Each ore robot costs {d} ore. Each clay robot costs {d} ore. Each obsidian robot costs {d} ore and {d} clay. Each geode robot costs {d} ore and {d} obsidian.",
            line,
            .{
                &bp.id,
                &bp.r_ore_ore,
                &bp.r_clay_ore,
                &bp.r_obsid_ore,
                &bp.r_obsid_clay,
                &bp.r_geode_ore,
                &bp.r_geode_obsid,
            },
        );

        const geodes = try Pathfinder.findMaxGeodes(allocator, bp, 24);
        sum += geodes * bp.id;

        if (0 <= i and i <= 2) {
            prod *= try Pathfinder.findMaxGeodes(allocator, bp, 32);
        }
        i += 1;
    }

    try ps.solution(sum);
    try ps.solution(prod);
}
