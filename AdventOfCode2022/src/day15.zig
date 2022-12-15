const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Range = sr.InclusiveRange1D(isize);

const Sensor = struct {
    pos: sr.Vec2is,
    nearest_distance: isize,
};

/// Iteratively reduce ranges by checking for overlaps and merging.
fn reduceRanges(ranges: []Range) []Range {
    var working_size = ranges.len;
    var last_ranges_size: usize = 0;
    while (last_ranges_size != working_size) {
        last_ranges_size = working_size;
        var i: usize = 0;
        while (i < working_size) : (i += 1) {
            var j: usize = i + 1;
            while (j < working_size) {
                if (ranges[i].overlaps(ranges[j])) {
                    ranges[i] = ranges[i].merge(ranges[j]);
                    ranges[j] = ranges[working_size - 1];
                    working_size -= 1;
                } else {
                    j += 1;
                }
            }
        }
    }
    return ranges[0..working_size];
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
        var ranges_list = std.ArrayListUnmanaged(Range){};
        for (sensors.items) |*s| {
            // If we have a sensor S, with a distance to beacon B = 3:
            //
            //        #
            //       ###
            //      ####B
            //     ###S###
            //      #####
            //       ###
            //        #
            //
            // Note that with manhattan distance, each step away from the sensor
            // along the Y axis shrinks the width of the sensor diamond by one
            // on the left and right sides.
            //
            // At dy=0 (the same Y as the sensor), the left and right sides
            // have width equal to the manhattan distance.
            //
            // At dy=1, they have width equal to the manhattan distance minus 1,
            // and so on.
            //
            // Since the diamond must be centered on the sensor, we can find a
            // line that represents a horizontal slice of the diamond `dy` units
            // away on the Y axis:
            const dy = try std.math.absInt(s.pos.y - y);
            const range = Range{
                .min = s.pos.x - s.nearest_distance + dy,
                .max = s.pos.x + s.nearest_distance - dy,
            };
            // If the line has a negative width, it is out of range for this sensor.
            if (range.width() > 0) {
                try ranges_list.append(ps.allocator, range);
            }
        }
        // Then we can merge all lines that overlap.
        const ranges = reduceRanges(ranges_list.items);

        // Part 1 is simply a measurement of the width of all lines at Y=2000000
        // minus the number of unique beacons on that line.
        if (y == 2000000) {
            for (ranges) |r| {
                sum_p1 += r.width();
            }
            for (beacons.keys()) |b| {
                for (ranges) |r| {
                    if (b.y == 2000000 and r.containsScalar(b.x)) {
                        sum_p1 -= 1;
                    }
                }
            }
            // early out
            if (ans_p2 != 0) {
                break;
            }
        }
        // Part 2 states there is only a single point in the range
        // [0..4000000],[0..4000000] that is not covered by a sensor, so if we
        // found two lines at this Y, there is a gap between them of one unit.
        // That gap must be the solution. For every other Y, there should only
        // be one line that covers the entire range [0..4000000],Y.
        if (ranges.len == 2) {
            const x = if (ranges[0].max < ranges[1].min)
                ranges[0].max + 1
            else
                ranges[0].min - 1;
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
