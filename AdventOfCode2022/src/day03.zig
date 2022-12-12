const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var rs = try RucksackList.loadFromStream(ps.allocator, ps.getPuzzleInputReader());
    try ps.solution(rs.sumCommonPrio());
    try ps.solution(rs.sumCommonPrio2());
}

const letter_count = 52;
const LetterSet = std.StaticBitSet(letter_count);

/// Maps ascii a-zA-Z to indices [0..52)
fn letterIndex(ch: u8) u64 {
    if (std.ascii.isLower(ch)) {
        return ch - 'a';
    } else if (std.ascii.isUpper(ch)) {
        return 26 + ch - 'A';
    }
    unreachable;
}

fn stringToSet(str: []const u8) LetterSet {
    var s = LetterSet.initEmpty();
    for (str) |ch| {
        s.set(letterIndex(ch));
    }
    return s;
}

const Rucksack = struct {
    whole_string: []u8,
    compartments: [2][]u8,

    fn commonPrio(self: Rucksack) u64 {
        var a = stringToSet(self.compartments[0]);
        var b = stringToSet(self.compartments[1]);
        a.setIntersection(b);
        const i = a.findFirstSet() orelse @panic("bad rucksack: no common letter");
        return 1 + i;
    }

    fn toSet(self: Rucksack) LetterSet {
        return stringToSet(self.whole_string);
    }
};

const RucksackList = struct {
    rucksacks: []Rucksack,

    fn deinit(self: RucksackList, allocator: Allocator) void {
        for (self.rucksacks) |*r| {
            allocator.free(r.whole_string);
        }
        allocator.free(self.rucksacks);
    }

    fn loadFromStream(allocator: Allocator, reader: anytype) !RucksackList {
        var buf: [256]u8 = undefined;

        var rs = std.ArrayListUnmanaged(Rucksack){};
        errdefer rs.deinit(allocator);

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");
            const compartment_len = stripped_line.len / 2;
            std.debug.assert(stripped_line.len % 2 == 0);

            var ruck = try rs.addOne(allocator);
            ruck.*.whole_string = try allocator.dupe(u8, stripped_line);
            ruck.*.compartments[0] = ruck.*.whole_string[0..compartment_len];
            ruck.*.compartments[1] = ruck.*.whole_string[compartment_len .. compartment_len + compartment_len];
        }

        return RucksackList{
            .rucksacks = try rs.toOwnedSlice(allocator),
        };
    }

    fn sumCommonPrio(self: RucksackList) u64 {
        var sum: u64 = 0;
        for (self.rucksacks) |*r| {
            sum += r.commonPrio();
        }
        return sum;
    }

    fn sumCommonPrio2(self: RucksackList) u64 {
        var sum: u64 = 0;
        var window_start: usize = 0;
        while (window_start < self.rucksacks.len) : (window_start += 3) {
            const group_sacks = self.rucksacks[window_start .. window_start + 3];
            var group_set = LetterSet.initFull();

            for (group_sacks) |sack| {
                group_set.setIntersection(sack.toSet());
            }

            const i = group_set.findFirstSet() orelse @panic("bad rucksack window: no common letter");
            sum += 1 + i;
        }
        return sum;
    }
};

const example_input =
    \\vJrwpWtwJgWrhcsFMMfFFhFp
    \\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    \\PmmdzqPrVvPwwTWBwg
    \\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    \\ttgJtRGJQctTZtZT
    \\CrZsJsPPZsGzwwsLwLmpwMDw
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 157), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 70), solutions[1].integer);
}
