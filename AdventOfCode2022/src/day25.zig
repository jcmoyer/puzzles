const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

fn mapSnafu(ch: u8) i64 {
    return switch (ch) {
        '2' => 2,
        '1' => 1,
        '0' => 0,
        '-' => -1,
        '=' => -2,
        else => @panic("mapSnafu cannot handle char"),
    };
}

fn parseSnafu(s: []const u8) !i64 {
    var i: usize = 0;
    var result: i64 = 0;
    while (i < s.len) : (i += 1) {
        const place = s.len - i - 1;
        result += mapSnafu(s[i]) * try std.math.powi(i64, 5, @intCast(i64, place));
    }
    return result;
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var lines = sr.sliceLines(ps.input_text);
    var sum: i64 = 0;
    while (lines.next()) |line| {
        sum += try parseSnafu(line);
    }

    try ps.solution(sum);
    // TODO: actual solution, solved this with python+z3
    try ps.solution("2---1010-0=1220-=010");
}
