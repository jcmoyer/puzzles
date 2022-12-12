pub const Scanner = @import("scanner.zig").Scanner;
pub const Vec2 = @import("vector.zig").Vec2;
pub const Vec2i = @import("vector.zig").Vec2i;
pub const Vec2us = @import("vector.zig").Vec2us;
pub const manhattan = @import("vector.zig").manhattan;
pub const Direction = @import("direction.zig").Direction;
pub const Turn = @import("direction.zig").Turn;
pub const Array2D = @import("array2d.zig").Array2D;
pub const parse = @import("parse.zig").parse;
pub const Graph = @import("graph.zig").Graph;

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const expectEqual = std.testing.expectEqual;

pub fn solution(x: anytype) void {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var writer = stream.writer();

    if (comptime @typeInfo(@TypeOf(x)) == .Int) {
        writer.print("{d}", .{x}) catch unreachable;
    } else {
        writer.print("{s}", .{x}) catch unreachable;
    }

    var written = stream.getWritten();
    std.io.getStdOut().writer().print("{s}\n", .{written}) catch unreachable;

    if (builtin.os.tag == .windows) {
        const windows = @import("windows.zig");

        var mem = windows.GlobalAlloc(windows.GMEM_MOVEABLE | windows.GMEM_ZEROINIT, written.len + 1);
        if (mem == null) {
            std.log.err("failed to allocate clipboard memory", .{});
            std.os.exit(1);
        }
        var ptr = windows.GlobalLock(mem);
        if (ptr == null) {
            _ = windows.GlobalFree(mem);
            std.log.err("failed to lock clipboard memory", .{});
            std.os.exit(1);
        }
        std.mem.copy(u8, @ptrCast([*]u8, ptr)[0..written.len], written);
        _ = windows.GlobalUnlock(mem);
        _ = windows.OpenClipboard(null);
        _ = windows.EmptyClipboard();
        _ = windows.SetClipboardData(windows.CF_TEXT, mem);
        _ = windows.CloseClipboard();
    }
}

pub const StandardCommandLine = struct {
    allocator: Allocator,
    args: [][:0]u8,

    input_filename: []const u8,

    pub fn init(allocator: Allocator) !StandardCommandLine {
        const args = try std.process.argsAlloc(allocator);

        if (args.len < 2) {
            try std.io.getStdOut().writer().print("usage: {s} <input filename>\n", .{args[0]});
            std.process.argsFree(allocator, args);
            std.process.exit(1);
        }

        return StandardCommandLine{
            .allocator = allocator,
            .args = args,
            .input_filename = args[1],
        };
    }

    pub fn deinit(self: *StandardCommandLine) void {
        std.process.argsFree(self.allocator, self.args);
    }
};

pub fn readAllAlloc(allocator: Allocator, filename: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    return file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
}

pub const ReadLinesResult = struct {
    allocator: Allocator,
    /// All bytes read from the stream.
    bytes: []const u8,
    /// Slices into `bytes`.
    lines: [][]const u8,

    pub fn deinit(self: *ReadLinesResult) void {
        self.allocator.free(self.bytes);
        self.allocator.free(self.lines);
        self.* = undefined;
    }
};

pub fn readLines(allocator: Allocator, reader: anytype) !ReadLinesResult {
    const bytes = try reader.readAllAlloc(allocator, std.math.maxInt(usize));
    errdefer allocator.free(bytes);

    var line_slices = std.ArrayList([]const u8).init(allocator);
    errdefer line_slices.deinit();

    var lines = std.mem.tokenize(u8, bytes, "\r\n");
    while (lines.next()) |line| {
        try line_slices.append(line);
    }

    return ReadLinesResult{
        .allocator = allocator,
        .bytes = bytes,
        .lines = try line_slices.toOwnedSlice(),
    };
}

pub fn LinesIterator(comptime Reader: type) type {
    return struct {
        allocator: Allocator,
        reader: Reader,
        buffer: std.ArrayList(u8),

        pub fn deinit(self: *@This()) void {
            self.buffer.deinit();
        }

        pub fn next(self: *@This()) !?[]const u8 {
            self.reader.readUntilDelimiterArrayList(&self.buffer, '\n', std.math.maxInt(usize)) catch |err| {
                if (err == error.EndOfStream) {
                    if (self.buffer.items.len > 0) {
                        return self.buffer.items;
                    } else {
                        return null;
                    }
                } else {
                    return err;
                }
            };
            if (self.buffer.items[self.buffer.items.len - 1] == '\r') {
                self.buffer.shrinkRetainingCapacity(self.buffer.items.len - 1);
            }
            return self.buffer.items;
        }
    };
}

pub fn iterateLines(allocator: Allocator, reader: anytype) LinesIterator(@TypeOf(reader)) {
    return .{ .allocator = allocator, .reader = reader, .buffer = std.ArrayList(u8).init(allocator) };
}

pub fn readTilemap(allocator: Allocator, reader: anytype) !Array2D(u8) {
    var array = Array2D(u8){};
    errdefer array.deinit(allocator);

    var lines = try readLines(allocator, reader);
    defer lines.deinit();

    if (lines.lines.len == 0) {
        return error.NoData;
    }

    const width = lines.lines[0].len;
    const height = lines.lines.len;

    try array.resize(allocator, width, height);

    for (lines.lines) |line, y| {
        if (line.len != width) {
            return error.MismatchedWidth;
        }
        var dest_row = @ptrCast([*]u8, array.at(0, y));
        std.mem.copy(u8, dest_row[0..width], line);
    }

    return array;
}

test "readTilemap" {
    const tm =
        \\###.
        \\##..
        \\...#
    ;
    var stream = std.io.fixedBufferStream(tm);
    var reader = stream.reader();

    var arr = try readTilemap(std.testing.allocator, reader);
    defer arr.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 4), arr.width);
    try std.testing.expectEqual(@as(usize, 3), arr.height);

    var row0 = arr.iterateRow(0);

    try std.testing.expectEqual(@as(u8, '#'), row0.next().?.*);
    try std.testing.expectEqual(@as(u8, '#'), row0.next().?.*);
    try std.testing.expectEqual(@as(u8, '#'), row0.next().?.*);
    try std.testing.expectEqual(@as(u8, '.'), row0.next().?.*);
}

fn FrequencyMap(comptime K: type) type {
    return std.AutoArrayHashMap(K, u32);
}

/// Caller owns the memory if this function succeeds and must call `deinit()`.
pub fn frequencies(comptime K: type, allocator: Allocator, items: []const K) !FrequencyMap(K) {
    var map = FrequencyMap(K).init(allocator);
    errdefer map.deinit();
    for (items) |item| {
        var ent = try map.getOrPut(item);
        if (!ent.found_existing) {
            ent.value_ptr.* = 1;
        } else {
            ent.value_ptr.* += 1;
        }
    }
    return map;
}

pub fn FrequencySort(comptime K: type) type {
    return struct {
        fn compare(map: FrequencyMap(K), lhs: K, rhs: K) bool {
            return map.get(lhs).? < map.get(rhs).?;
        }
    };
}

pub fn maxFrequency(comptime K: type, freq: FrequencyMap(K)) ?K {
    const ent = std.sort.max(K, freq.keys(), freq, comptime FrequencySort(K).compare);
    if (ent) |e| {
        return e;
    } else {
        return null;
    }
}

pub fn minFrequency(comptime K: type, freq: FrequencyMap(K)) ?K {
    const ent = std.sort.min(K, freq.keys(), freq, comptime FrequencySort(K).compare);
    if (ent) |e| {
        return e;
    } else {
        return null;
    }
}

pub fn isPalindrome(str: []const u8) bool {
    var first: usize = 0;
    var last: usize = str.len - 1;
    var count = str.len / 2;

    while (count > 0) : (count -= 1) {
        if (str[first] != str[last]) {
            return false;
        }
        first += 1;
        last -= 1;
    }

    return true;
}

test "isPalindrome" {
    try expectEqual(true, isPalindrome("abba"));
    try expectEqual(true, isPalindrome("abcba"));
    try expectEqual(true, isPalindrome("abccba"));
    try expectEqual(false, isPalindrome("abdcba"));
}

pub fn WindowIterator(comptime T: type) type {
    return struct {
        slice: []const T,
        index: usize = 0,
        width: usize,

        pub fn next(self: *@This()) ?[]const T {
            const remaining = self.slice[self.index..].len;
            if (remaining < self.width) {
                return null;
            }
            const window_slice = self.slice[self.index .. self.index + self.width];
            self.index += 1;
            return window_slice;
        }
    };
}

pub fn window(comptime T: type, slice: []const T, width: usize) WindowIterator(T) {
    return WindowIterator(T){
        .slice = slice,
        .width = width,
    };
}

test "window" {
    const nums = [_]u32{ 1, 2, 3, 4, 5 };
    var i = window(u32, &nums, 3);
    try std.testing.expectEqualSlices(u32, &[_]u32{ 1, 2, 3 }, i.next().?);
    try std.testing.expectEqualSlices(u32, &[_]u32{ 2, 3, 4 }, i.next().?);
    try std.testing.expectEqualSlices(u32, &[_]u32{ 3, 4, 5 }, i.next().?);
    try std.testing.expect(i.next() == null);
}

pub fn sum(comptime T: type, slice: []const T) T {
    var t: T = 0;
    for (slice) |x| {
        t += x;
    }
    return t;
}

pub fn product(comptime T: type, slice: []const T) T {
    var t: T = 1;
    for (slice) |x| {
        t *= x;
    }
    return t;
}

test {
    _ = @import("graph.zig");
    _ = @import("vector.zig");
    _ = @import("array2d.zig");
}

fn Iterator2D(comptime T: type) type {
    return struct {
        const Self = @This();

        start: Vec2(T),
        end: Vec2(T),
        current: Vec2(T),

        pub fn next(self: *Self) ?Vec2(T) {
            if (self.current.x == self.end.x) {
                self.current.y += 1;
                self.current.x = 0;
            }
            if (self.current.y == self.end.y) {
                return null;
            }
            const val = self.current;
            self.current.x += 1;
            return val;
        }
    };
}

pub fn iterate2D(comptime T: type, start: Vec2(T), end: Vec2(T)) Iterator2D(T) {
    return .{ .start = start, .end = end, .current = start };
}

pub fn formatArrayMultiline(array: Array2D(u8), line_width: usize, writer: anytype) !void {
    const line_count = try std.math.divExact(usize, array.data.len, line_width);
    var i: usize = 0;
    while (i < line_count) : (i += 1) {
        _ = try writer.write(array.data[i * line_width .. i * line_width + line_width]);
        _ = try writer.write("\n");
    }
}

pub const SliceLinesIterator = struct {
    slice: []const u8,
    index: usize = 0,

    pub fn next(self: *SliceLinesIterator) ?[]const u8 {
        if (self.index == self.slice.len) {
            return null;
        } else if (std.mem.indexOfScalarPos(u8, self.slice, self.index, '\n')) |next_eol| {
            defer self.index = next_eol + 1;
            if (next_eol -% 1 < self.slice.len and self.slice[next_eol - 1] == '\r') {
                return self.slice[self.index .. next_eol - 1];
            } else {
                return self.slice[self.index..next_eol];
            }
        } else {
            defer self.index = self.slice.len;
            return self.slice[self.index..];
        }
    }
};

pub fn sliceLines(slice: []const u8) SliceLinesIterator {
    return SliceLinesIterator{ .slice = slice };
}

test "SliceLinesIterator" {
    {
        var it = SliceLinesIterator{ .slice = "hello\r\nworld\n\nwowsers\n\r\r\n" };
        try std.testing.expectEqualStrings("hello", it.next().?);
        try std.testing.expectEqualStrings("world", it.next().?);
        try std.testing.expectEqualStrings("", it.next().?);
        try std.testing.expectEqualStrings("wowsers", it.next().?);
        try std.testing.expectEqualStrings("\r", it.next().?);
        try std.testing.expect(it.next() == null);
    }
    {
        var it = SliceLinesIterator{ .slice = "" };
        try std.testing.expect(it.next() == null);
    }
}
