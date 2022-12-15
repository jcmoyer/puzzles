const std = @import("std");
const Allocator = std.mem.Allocator;
const argparse = @import("argparse.zig");
const ArgParseState = argparse.ArgParseState;
const root = @import("root");
const ResettableArenaAllocator = @import("resettable_arena_allocator.zig").ResettableArenaAllocator;

pub const PuzzleSolution = union(enum) {
    integer: u64,
    // TODO: implement this in a way that doesn't overly complicate testing
    // text: []u8,
};

pub const PuzzleSolverState = struct {
    const max_input_size = 1024 * 1024;

    arena: ResettableArenaAllocator,
    allocator: Allocator,
    // aligned for SIMD shenanigans
    input_text: []align(64) const u8,
    input_text_storage: []align(64) u8,
    input_reader: std.io.FixedBufferStream([]align(64) const u8),
    times_solution_called: u32 = 0,
    solutions: [2]PuzzleSolution,
    stdout: std.fs.File,
    stderr: std.fs.File,

    fn createWithBuffer(allocator: Allocator, buffer: []const u8) !*PuzzleSolverState {
        var self = try allocator.create(PuzzleSolverState);
        self.* = .{
            .arena = ResettableArenaAllocator.init(std.heap.page_allocator),
            .stdout = std.io.getStdOut(),
            .stderr = std.io.getStdErr(),
            // initialized below
            .allocator = undefined,
            .input_text = undefined,
            .input_text_storage = undefined,
            .input_reader = undefined,
            .solutions = undefined,
        };
        errdefer self.arena.deinit();

        self.allocator = self.arena.allocator();

        // input text allocated with parent allocator since arena may be reset
        self.input_text_storage = try allocator.alignedAlloc(u8, 64, max_input_size);
        errdefer allocator.free(self.input_text_storage);
        const input_text_size = buffer.len;
        std.mem.copy(u8, self.input_text_storage, buffer);

        self.input_text = self.input_text_storage[0..input_text_size];
        self.input_reader = std.io.fixedBufferStream(self.input_text);

        return self;
    }

    fn createWithFilename(allocator: Allocator, filename: []const u8) !*PuzzleSolverState {
        var self = try allocator.create(PuzzleSolverState);
        self.* = .{
            .arena = ResettableArenaAllocator.init(std.heap.page_allocator),
            .stdout = std.io.getStdOut(),
            .stderr = std.io.getStdErr(),
            // initialized below
            .allocator = undefined,
            .input_text = undefined,
            .input_text_storage = undefined,
            .input_reader = undefined,
            .solutions = undefined,
        };
        errdefer self.arena.deinit();

        self.allocator = self.arena.allocator();

        var file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        // input text allocated with parent allocator since arena may be reset
        self.input_text_storage = try allocator.alignedAlloc(u8, 64, max_input_size);
        errdefer allocator.free(self.input_text_storage);
        const input_text_size = try file.readAll(self.input_text_storage);

        self.input_text = self.input_text_storage[0..input_text_size];
        self.input_reader = std.io.fixedBufferStream(self.input_text);

        return self;
    }

    fn destroy(self: *PuzzleSolverState, allocator: Allocator) void {
        self.arena.deinit();
        allocator.free(self.input_text_storage);
        allocator.destroy(self);
    }

    pub fn getPuzzleInputReader(self: *PuzzleSolverState) @TypeOf(self.input_reader.reader()) {
        return self.input_reader.reader();
    }

    pub fn solution(self: *PuzzleSolverState, val: anytype) !void {
        if (self.times_solution_called >= 2) {
            return;
        }
        var ptr = &self.solutions[self.times_solution_called];
        const writer = std.io.getStdOut().writer();

        if (@typeInfo(@TypeOf(val)) == .Int) {
            try writer.print("{d}\n", .{val});
            ptr.* = .{ .integer = @intCast(u64, val) };
        } else {
            try writer.print("{s}\n", .{val});
            // ptr.* = .{.text= self.allocator.dupe(u8,val)};
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

pub fn testSolver(module: anytype, input_text: []const u8) ![2]PuzzleSolution {
    var ps = try PuzzleSolverState.createWithBuffer(std.testing.allocator, input_text);
    defer ps.destroy(std.testing.allocator);
    try module.solve(ps);
    return ps.solutions;
}

fn runBenchmark(gpa: Allocator, ps: *PuzzleSolverState) !void {
    const stderr = ps.stderr.writer();
    const sec = 5;

    var time_buffer = std.ArrayListUnmanaged(u64){};
    defer time_buffer.deinit(gpa);

    var timer = try std.time.Timer.start();
    var runs: usize = 0;
    var total_ns_elapsed: u64 = 0;
    while (true) {
        if (ps.arena.reset(.retain_capacity) == false) {
            try stderr.print("failed to reset arena\n", .{});
            break;
        }
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

    std.sort.sort(u64, time_buffer.items, {}, std.sort.asc(u64));
    const median_ns = if (time_buffer.items.len % 2 == 0)
        (time_buffer.items[time_buffer.items.len / 2 - 1] + time_buffer.items[time_buffer.items.len / 2]) / 2
    else
        time_buffer.items[time_buffer.items.len / 2];

    try stderr.print("runs:                   {d}\n", .{runs});
    try stderr.print("average elapsed:        {d}ns\n", .{total_ns_elapsed / runs});
    try stderr.print("median elapsed:         {d}ns\n", .{median_ns});
    try stderr.print("total elapsed:          {d}ms\n", .{(total_ns_elapsed / std.time.ns_per_ms)});

    const bytes_processed = runs * ps.input_text.len;
    try stderr.print("processed:              {:.2}\n", .{(std.fmt.fmtIntSizeBin(bytes_processed))});
    try stderr.print("throughput:             {:.2}/s\n", .{(std.fmt.fmtIntSizeBin(bytes_processed / (total_ns_elapsed / std.time.ns_per_s)))});
}
