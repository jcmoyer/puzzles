const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Sensor = struct {
    pos: sr.Vec2is,
    nearest_distance: isize,
};

const Range = struct {
    min: isize,
    max: isize,

    fn contains(self: Range, other: Range) bool {
        return self.min <= other.min and other.max <= self.max;
    }

    fn containsScalar(self: Range, other: isize) bool {
        return self.min <= other and other <= self.max;
    }

    fn overlaps(self: Range, other: Range) bool {
        return self.containsScalar(other.min) or self.containsScalar(other.max) or other.containsScalar(self.min) or other.containsScalar(self.max);
    }

    fn width(self: Range) isize {
        return self.max - self.min + 1;
    }

    fn merge(self: Range, other: Range) Range {
        return Range{
            .min = std.math.min(self.min, other.min),
            .max = std.math.max(self.max, other.max),
        };
    }
};

/// iteratively reduce ranges
fn reduceRanges(ranges: *std.ArrayList(Range)) void {
    var last_ranges_size: usize = 0;
    while (last_ranges_size != ranges.items.len) {
        last_ranges_size = ranges.items.len;
        var i: usize = 0;
        next_range: while (i < ranges.items.len) : (i += 1) {
            var j: usize = i + 1;
            while (j < ranges.items.len) : (j += 1) {
                if (ranges.items[i].overlaps(ranges.items[j])) {
                    ranges.items[i] = ranges.items[i].merge(ranges.items[j]);
                    _ = ranges.swapRemove(j);
                    break :next_range;
                }
            }
        }
    }
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var sensors = std.ArrayList(Sensor).init(ps.allocator);
    var beacons = std.AutoArrayHashMap(sr.Vec2is, void).init(ps.allocator);

    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var sensor_pos = sr.Vec2is{};
        var beacon_pos = sr.Vec2is{};
        _ = try sr.parse("Sensor at x={d}, y={d}: closest beacon is at x={d}, y={d}", line, .{
            &sensor_pos.x,
            &sensor_pos.y,
            &beacon_pos.x,
            &beacon_pos.y,
        });
        try sensors.append(Sensor{
            .pos = sensor_pos,
            .nearest_distance = try sensor_pos.manhattan(beacon_pos),
        });
        try beacons.put(beacon_pos, {});
    }

    var sum_p1: isize = 0;
    var ans_p2: isize = 0;

    var y: isize = 0;
    while (y <= 4000000) : (y += 1) {
        var ranges = std.ArrayList(Range).init(ps.allocator);
        next_sensor: for (sensors.items) |*s| {
            const dy = try std.math.absInt(s.pos.y - y);
            var left: isize = s.pos.x - s.nearest_distance;
            var right: isize = s.pos.x + s.nearest_distance;
            left += dy;
            right -= dy;
            if (right - left >= 0) {
                const new_r = Range{ .min = left, .max = right };
                for (ranges.items) |*r| {
                    if (r.overlaps(new_r)) {
                        r.* = r.merge(new_r);
                        continue :next_sensor;
                    }
                    if (r.contains(new_r)) {
                        continue :next_sensor;
                    }
                }
                try ranges.append(new_r);
            }
        }

        reduceRanges(&ranges);

        if (y == 2000000) {
            for (ranges.items) |mr| {
                sum_p1 += mr.width();
            }
            for (beacons.keys()) |b| {
                for (ranges.items) |mr| {
                    if (b.y == 2000000 and mr.containsScalar(b.x)) {
                        sum_p1 -= 1;
                    }
                }
            }
            // early out
            if (ans_p2 != 0) {
                break;
            }
        }
        if (ranges.items.len == 2) {
            const x = if (ranges.items[0].max < ranges.items[1].min)
                ranges.items[0].max + 1
            else
                ranges.items[0].min - 1;
            ans_p2 = x * 4000000 + y;
            // early out
            if (sum_p1 != 0) {
                break;
            }
        }
    }

    try ps.solution(sum_p1);
    try ps.solution(ans_p2);
}
