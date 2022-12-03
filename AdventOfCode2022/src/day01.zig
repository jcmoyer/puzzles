const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var elves = try ElfList.loadFromStream(ps.allocator, ps.getPuzzleInputReader());
    try ps.solution(elves.maxCalories());
    try ps.solution(elves.sumTopThree());
}

const example_input =
    \\1000
    \\2000
    \\3000
    \\
    \\4000
    \\
    \\5000
    \\6000
    \\
    \\7000
    \\8000
    \\9000
    \\
    \\10000
;

const Elf = struct {
    calorie_entries: []u64,

    fn deinit(self: *Elf, allocator: Allocator) void {
        allocator.free(self.calorie_entries);
        self.* = undefined;
    }

    fn sum(self: Elf) u64 {
        var s: u64 = 0;
        for (self.calorie_entries) |ent| {
            s += ent;
        }
        return s;
    }
};

const ElfList = struct {
    elves: []Elf,

    fn deinit(self: *ElfList, allocator: Allocator) void {
        for (self.elves) |*e| {
            e.deinit(allocator);
        }
        allocator.free(self.elves);
        self.* = undefined;
    }

    fn loadFromStream(allocator: Allocator, reader: anytype) !ElfList {
        var buf: [256]u8 = undefined;

        var elves = std.ArrayListUnmanaged(Elf){};
        errdefer elves.deinit(allocator);
        var entry_buf = std.ArrayListUnmanaged(u64){};
        defer entry_buf.deinit(allocator);

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");
            if (stripped_line.len == 0) {
                // commit elf
                var elf = Elf{
                    .calorie_entries = try entry_buf.toOwnedSlice(allocator),
                };
                try elves.append(allocator, elf);
                entry_buf = .{};
            } else {
                // add calories to current elf
                const calories = try std.fmt.parseInt(u64, stripped_line, 10);
                try entry_buf.append(allocator, calories);
            }
        }
        if (entry_buf.items.len != 0) {
            // last elf in input
            var elf = Elf{
                .calorie_entries = try entry_buf.toOwnedSlice(allocator),
            };
            try elves.append(allocator, elf);
            entry_buf = .{};
        }

        return ElfList{
            .elves = try elves.toOwnedSlice(allocator),
        };
    }

    fn maxCalories(self: ElfList) u64 {
        var max: u64 = 0;
        for (self.elves) |e| {
            max = std.math.max(max, e.sum());
        }
        return max;
    }

    fn sumTopThree(self: ElfList) u64 {
        self.sortByMaxCalories();
        return self.elves[self.elves.len - 1].sum() + self.elves[self.elves.len - 2].sum() + self.elves[self.elves.len - 3].sum();
    }

    fn sortByMaxCalories(self: ElfList) void {
        const SortImpl = struct {
            fn lessThan(_: void, a: Elf, b: Elf) bool {
                return a.sum() < b.sum();
            }
        };
        std.sort.sort(Elf, self.elves, {}, SortImpl.lessThan);
    }
};

test "part 1" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var elves = try ElfList.loadFromStream(allocator, reader);
    defer elves.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 24000), elves.maxCalories());
}

test "part 2" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var elves = try ElfList.loadFromStream(allocator, reader);
    defer elves.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 45000), elves.sumTopThree());
}
