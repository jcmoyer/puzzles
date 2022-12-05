const std = @import("std");
const Allocator = std.mem.Allocator;

pub const PuzzleSolverState = struct {
    arena: std.heap.ArenaAllocator,
    allocator: Allocator,
    input_text: []const u8,
    input_reader: std.io.FixedBufferStream([]const u8),

    fn createWithFilename(allocator: Allocator, filename: []const u8) !*PuzzleSolverState {
        var self = try allocator.create(PuzzleSolverState);
        self.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        self.allocator = self.arena.allocator();
        errdefer self.arena.deinit();

        var file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        self.input_text = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        self.input_reader = std.io.fixedBufferStream(self.input_text);

        return self;
    }

    fn destroy(self: *PuzzleSolverState, allocator: Allocator) void {
        self.arena.deinit();
        allocator.destroy(self);
    }

    pub fn getPuzzleInputReader(self: *PuzzleSolverState) @TypeOf(self.input_reader.reader()) {
        return self.input_reader.reader();
    }

    pub fn solution(self: *PuzzleSolverState, val: anytype) !void {
        _ = self;
        const writer = std.io.getStdOut().writer();

        if (@typeInfo(@TypeOf(val)) == .Int) {
            try writer.print("{d}\n", .{val});
        } else {
            try writer.print("{s}\n", .{val});
        }
    }
};

const root = @import("root");

pub fn defaultMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        try std.io.getStdErr().writer().print("usage: {s} <input-filename>", .{args[0]});
        std.process.exit(1);
    }

    var ps = try PuzzleSolverState.createWithFilename(allocator, args[1]);
    defer ps.destroy(allocator);
    try root.solve(ps);
}
