const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var s = try Solution.parseFromStream(ps.allocator, ps.getPuzzleInputReader());
    try ps.solution(try s.part1.getTopCrates(ps.allocator));
    try ps.solution(try s.part2.getTopCrates(ps.allocator));
}

const StackList = struct {
    const CrateStack = std.ArrayListUnmanaged(u8);

    stacks: []CrateStack = &[_]CrateStack{},

    fn init(allocator: Allocator, num_stacks: usize) !StackList {
        const stacks = try allocator.alloc(CrateStack, num_stacks);
        for (stacks) |*st| {
            st.* = .{};
        }
        return StackList{
            .stacks = stacks,
        };
    }

    fn deinit(self: *StackList, allocator: Allocator) void {
        for (self.stacks) |*st| {
            st.deinit(allocator);
        }
        allocator.free(self.stacks);
    }

    fn moveCratesOneByOne(self: *StackList, allocator: Allocator, src: u32, dst: u32, amt: u32) !void {
        var i: u32 = 0;
        while (i < amt) : (i += 1) {
            const c = self.stacks[src].pop();
            try self.stacks[dst].append(allocator, c);
        }
    }

    fn moveCratesContiguous(self: *StackList, allocator: Allocator, src: u32, dst: u32, amt: u32) !void {
        const mv_start = self.stacks[src].items.len - amt;
        try self.stacks[dst].appendSlice(allocator, self.stacks[src].items[mv_start..]);
        self.popNum(src, amt);
    }

    fn popNum(self: *StackList, src: u32, amt: u32) void {
        var i: u32 = 0;
        while (i < amt) : (i += 1) {
            _ = self.stacks[src].pop();
        }
    }

    fn getTopCrates(self: *StackList, allocator: Allocator) ![]const u8 {
        var str = try allocator.alloc(u8, self.stacks.len);
        for (self.stacks) |*st, i| {
            str[i] = st.items[st.items.len - 1];
        }
        return str;
    }

    fn reverse(self: *StackList) void {
        for (self.stacks) |*st| {
            std.mem.reverse(u8, st.items);
        }
    }

    fn isEmpty(self: StackList) bool {
        return self.stacks.len == 0;
    }

    fn push(self: StackList, allocator: Allocator, id: u32, val: u8) !void {
        try self.stacks[id].append(allocator, val);
    }
};

const Solution = struct {
    part1: StackList = .{},
    part2: StackList = .{},

    fn deinit(self: *Solution, allocator: Allocator) void {
        self.part1.deinit(allocator);
        self.part2.deinit(allocator);
    }

    fn parseFromStream(allocator: Allocator, reader: anytype) !Solution {
        var buf: [256]u8 = undefined;

        var solution = Solution{};
        errdefer solution.deinit(allocator);

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");

            if (std.mem.startsWith(u8, stripped_line, "move")) {
                var i = std.mem.tokenize(u8, stripped_line, " ");
                _ = i.next().?; // "move"
                const amt = try std.fmt.parseInt(u32, i.next().?, 10);
                _ = i.next().?; // "from"
                var src = try std.fmt.parseInt(u32, i.next().?, 10);
                _ = i.next().?; // "to"
                var dst = try std.fmt.parseInt(u32, i.next().?, 10);

                // fixup 1-based indices
                src -= 1;
                dst -= 1;

                try solution.part1.moveCratesOneByOne(allocator, src, dst, amt);
                try solution.part2.moveCratesContiguous(allocator, src, dst, amt);
            } else if (std.mem.startsWith(u8, stripped_line, "[") or std.mem.startsWith(u8, stripped_line, " ")) {
                // a number at this position is identifying the stacks
                const numtest = stripped_line[1];
                if (numtest >= '0' and numtest <= '9') {
                    // done reading the stacks
                    solution.part1.reverse();
                    solution.part2.reverse();
                    continue;
                }
                // otherwise, we have a line of stack data
                const num_stacks = (stripped_line.len + 1) / 4;
                if (solution.part1.isEmpty()) {
                    solution.part1 = try StackList.init(allocator, num_stacks);
                    solution.part2 = try StackList.init(allocator, num_stacks);
                }
                var i: u32 = 0;
                while (i < num_stacks) : (i += 1) {
                    const stack_ch = stripped_line[1 + i * 4];
                    if (stack_ch == ' ') {
                        continue;
                    }
                    try solution.part1.push(allocator, i, stack_ch);
                    try solution.part2.push(allocator, i, stack_ch);
                }
            }
        }

        return solution;
    }
};

const example_input =
    \\    [D]    
    \\[N] [C]    
    \\[Z] [M] [P]
    \\ 1   2   3 
    \\
    \\move 1 from 2 to 1
    \\move 3 from 1 to 3
    \\move 2 from 2 to 1
    \\move 1 from 1 to 2
;

test "part 1" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var s = try Solution.parseFromStream(allocator, reader);
    defer s.deinit(allocator);
    const a = try s.part1.getTopCrates(allocator);
    defer allocator.free(a);

    try std.testing.expectEqualStrings("CMZ", a);
}

test "part 2" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var s = try Solution.parseFromStream(allocator, reader);
    defer s.deinit(allocator);
    const a = try s.part2.getTopCrates(allocator);
    defer allocator.free(a);

    try std.testing.expectEqualStrings("MCD", a);
}
