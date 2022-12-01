const std = @import("std");
const Allocator = std.mem.Allocator;

pub const PuzzleSolverState = struct {
    arena: std.heap.ArenaAllocator,
    allocator: Allocator,
    input_file: std.fs.File,
    input_file_reader: std.io.BufferedReader(4096, std.fs.File.Reader),

    fn createWithFilename(allocator: Allocator, filename: []const u8) !*PuzzleSolverState {
        var self = try allocator.create(PuzzleSolverState);
        self.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        self.allocator = self.arena.allocator();
        errdefer self.arena.deinit();

        self.input_file = try std.fs.cwd().openFile(filename, .{});
        errdefer self.input_file.close();

        self.input_file_reader = std.io.bufferedReader(self.input_file.reader());

        return self;
    }

    fn destroy(self: *PuzzleSolverState, allocator: Allocator) void {
        self.arena.deinit();
        self.input_file.close();
        allocator.destroy(self);
    }

    pub fn getPuzzleInputReader(self: *PuzzleSolverState) std.io.BufferedReader(4096, std.fs.File.Reader).Reader {
        return self.input_file_reader.reader();
    }

    pub fn solution(self: *PuzzleSolverState, val: anytype) !void {
        _ = self;
        const writer = std.io.getStdOut().writer();

        if (@TypeOf(val) == []const u8) {
            try writer.print("{s}\n", .{val});
        } else {
            try writer.print("{d}\n", .{val});
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
