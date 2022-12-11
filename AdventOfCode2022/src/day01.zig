const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var elves = try ElfList.loadFromSlice(ps.allocator, ps.input_text);
    try ps.solution(elves.maxCalories());
    try ps.solution(elves.sumTopThree());
}

const ElfList = struct {
    elf_calories: []u64,

    fn deinit(self: *ElfList, allocator: Allocator) void {
        allocator.free(self.elf_calories);
        self.* = undefined;
    }

    fn loadFromSlice(allocator: Allocator, slice: []const u8) !ElfList {
        var cals = std.ArrayListUnmanaged(u64){};
        errdefer cals.deinit(allocator);
        var current_cals: u64 = 0;

        var lines = sr.sliceLines(slice);
        while (lines.next()) |line| {
            if (line.len == 0) {
                try cals.append(allocator, current_cals);
                current_cals = 0;
            } else {
                current_cals += try std.fmt.parseInt(u64, line, 10);
            }
        }
        try cals.append(allocator, current_cals);

        std.sort.sort(u64, cals.items, {}, std.sort.desc(u64));

        return ElfList{
            .elf_calories = try cals.toOwnedSlice(allocator),
        };
    }

    fn maxCalories(self: ElfList) u64 {
        return self.elf_calories[0];
    }

    fn sumTopThree(self: ElfList) u64 {
        return sr.sum(u64, self.elf_calories[0..3]);
    }
};

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

test "part 1" {
    var allocator = std.testing.allocator;
    var elves = try ElfList.loadFromSlice(allocator, example_input);
    defer elves.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 24000), elves.maxCalories());
}

test "part 2" {
    var allocator = std.testing.allocator;
    var elves = try ElfList.loadFromSlice(allocator, example_input);
    defer elves.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 45000), elves.sumTopThree());
}
