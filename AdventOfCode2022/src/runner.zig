const std = @import("std");
const Allocator = std.mem.Allocator;
const argparse = @import("argparse.zig");
const ArgParseState = argparse.ArgParseState;
const root = @import("root");

pub const PuzzleSolverState = struct {
    arena: std.heap.ArenaAllocator,
    allocator: Allocator,
    // aligned for SIMD shenanigans
    input_text: []align(64) const u8,
    input_reader: std.io.FixedBufferStream([]align(64) const u8),
    times_solution_called: u32 = 0,
    stdout: std.fs.File,
    stderr: std.fs.File,

    fn createWithFilename(allocator: Allocator, filename: []const u8) !*PuzzleSolverState {
        var self = try allocator.create(PuzzleSolverState);
        self.* = .{
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .stdout = std.io.getStdOut(),
            .stderr = std.io.getStdErr(),
            // initialized below
            .allocator = undefined,
            .input_text = undefined,
            .input_reader = undefined,
        };
        errdefer self.arena.deinit();

        self.allocator = self.arena.allocator();

        var file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        var input_text_storage = try self.allocator.alignedAlloc(u8, 64, 1024 * 1024);
        const input_text_size = try file.readAll(input_text_storage);

        self.input_text = input_text_storage[0..input_text_size];
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
        if (self.times_solution_called >= 2) {
            return;
        }
        const writer = std.io.getStdOut().writer();

        if (@typeInfo(@TypeOf(val)) == .Int) {
            try writer.print("{d}\n", .{val});
        } else {
            try writer.print("{s}\n", .{val});
        }
        self.times_solution_called += 1;
    }
};

const Parameters = struct {
    input_filename: ?[]const u8 = null,
    benchmark: bool = false,
    help: bool = false,

    pub fn handlePositionalArg(self: *Parameters, state: *ArgParseState, arg: []const u8) !void {
        _ = state;

        self.input_filename = arg;
    }

    pub fn handleShortArg(self: *Parameters, state: *ArgParseState, shortname: u8) !void {
        _ = state;

        switch (shortname) {
            'h', '?' => {
                self.help = true;
            },
            else => return error.UnknownArg,
        }
    }

    pub fn handleLongArg(self: *Parameters, state: *ArgParseState, longname: []const u8) !void {
        _ = state;

        if (std.mem.eql(u8, longname, "benchmark")) {
            self.benchmark = true;
        } else if (std.mem.eql(u8, longname, "help")) {
            self.help = true;
        } else {
            return error.UnknownArg;
        }
    }
};

const usage =
    \\Usage: {s} <input-filename> [options]
    \\
    \\Options:
    \\
    \\    -?, -h, --help         Print this help and exit
    \\    --benchmark            Run for a time and print benchmark information
    \\
    \\
;

pub fn defaultMain() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const params = argparse.parseDefault(Parameters, args) catch {
        try std.io.getStdErr().writer().print(usage, .{args[0]});
        std.process.exit(1);
    };
    const input_filename = params.input_filename orelse {
        try std.io.getStdErr().writer().print(usage, .{args[0]});
        std.process.exit(1);
    };

    var ps = try PuzzleSolverState.createWithFilename(allocator, input_filename);
    defer ps.destroy(allocator);

    if (params.benchmark) {
        try runBenchmark(allocator, ps);
    } else {
        try root.solve(ps);
    }
}

fn runBenchmark(gpa: Allocator, ps: *PuzzleSolverState) !void {
    const sec = 5;

    var time_buffer = std.ArrayListUnmanaged(u64){};
    defer time_buffer.deinit(gpa);

    var timer = try std.time.Timer.start();
    var runs: usize = 0;
    var total_ns_elapsed: u64 = 0;
    while (true) {
        ps.input_reader.reset();
        timer.reset();
        try root.solve(ps);
        const ns_elapsed = timer.read();
        total_ns_elapsed += ns_elapsed;
        try time_buffer.append(gpa, ns_elapsed);
        runs += 1;
        if (total_ns_elapsed >= sec * std.time.ns_per_s) {
            break;
        }
    }
    const stderr = ps.stderr.writer();
    try stderr.print("runs:                   {d}\n", .{runs});
    try stderr.print("average elapsed:        {d}ns\n", .{total_ns_elapsed / runs});
    try stderr.print("median elapsed:         {d}ns\n", .{time_buffer.items[time_buffer.items.len / 2]});
    try stderr.print("total elapsed:          {d}ms\n", .{(total_ns_elapsed / std.time.ns_per_ms)});

    const bytes_processed = runs * ps.input_text.len;
    try stderr.print("processed:              {:.2}\n", .{(std.fmt.fmtIntSizeBin(bytes_processed))});
    try stderr.print("throughput:             {:.2}/s\n", .{(std.fmt.fmtIntSizeBin(bytes_processed / (total_ns_elapsed / std.time.ns_per_s)))});
}
