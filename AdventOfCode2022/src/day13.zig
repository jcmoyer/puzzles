const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const ExprError = error{
    ParseError,
} || Allocator.Error;

const Expr = union(enum) {
    integer: u64,
    list: []Expr,

    fn parse(allocator: Allocator, text: []const u8) ExprError!Expr {
        var sc = sr.Scanner.init(text);
        return parseOne(allocator, &sc);
    }

    fn parseOne(allocator: Allocator, sc: *sr.Scanner) ExprError!Expr {
        if (sc.peek() == @as(u8, '[')) {
            return try parseList(allocator, sc);
        } else {
            const int = sc.readInt(u64) catch {
                std.debug.print("could not read integer at {d}\n", .{sc.pos});
                return ExprError.ParseError;
            };
            return Expr{ .integer = int };
        }
    }

    fn parseList(allocator: Allocator, sc: *sr.Scanner) ExprError!Expr {
        var vals = std.ArrayList(Expr).init(allocator);
        defer vals.deinit();

        const lbracket = sc.readOne();
        if (lbracket != @as(u8, '[')) {
            std.debug.print("expected '[' at {d}, found {c}\n", .{ sc.pos - 1, lbracket.? });
            return ExprError.ParseError;
        }

        while (true) {
            const comma_or_bracket = sc.peek();
            if (comma_or_bracket == @as(u8, ',')) {
                _ = sc.readOne();
            } else if (comma_or_bracket == @as(u8, ']')) {
                break;
            } else {
                try vals.append(try parseOne(allocator, sc));
            }
        }

        if (sc.readOne() != @as(u8, ']')) {
            std.debug.print("expected ']' at {d}\n", .{sc.pos - 1});
            return ExprError.ParseError;
        }

        return Expr{ .list = try vals.toOwnedSlice() };
    }

    fn lessThan(a: Expr, b: Expr) bool {
        if (a == .integer and b == .integer) {
            return a.integer < b.integer;
        } else if (a == .list and b == .list) {
            const compare_count = std.math.min(a.list.len, b.list.len);
            var i: usize = 0;
            while (i < compare_count) : (i += 1) {
                if (lessThan(a.list[i], b.list[i])) {
                    return true;
                }
                if (lessThan(b.list[i], a.list[i])) {
                    return false;
                }
            }
            return a.list.len < b.list.len;
        } else if (a == .list and b == .integer) {
            const promoted_b = Expr{ .list = &[_]Expr{b} };
            return lessThan(a, promoted_b);
        } else if (a == .integer and b == .list) {
            const promoted_a = Expr{ .list = &[_]Expr{a} };
            return lessThan(promoted_a, b);
        } else {
            unreachable;
        }
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var packets = std.ArrayList(Expr).init(ps.allocator);
    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        try packets.append(try Expr.parse(ps.allocator, line));
    }

    var i: usize = 0;
    var sum: usize = 0;
    while (i < packets.items.len) : (i += 2) {
        const pair_id = i / 2 + 1;
        if (Expr.lessThan(packets.items[i], packets.items[i + 1])) {
            sum += pair_id;
        }
    }
    try ps.solution(sum);

    // we'll use the list pointers to find where the dividers ended up
    const divider1 = try Expr.parse(ps.allocator, "[[2]]");
    const divider2 = try Expr.parse(ps.allocator, "[[6]]");

    try packets.append(divider1);
    try packets.append(divider2);
    std.sort.sort(Expr, packets.items, {}, lessThanContext);

    var divider1_id: usize = 0;
    var divider2_id: usize = 0;

    for (packets.items) |p, index| {
        switch (p) {
            .list => |xs| if (xs.ptr == divider1.list.ptr) {
                divider1_id = index + 1;
            } else if (xs.ptr == divider2.list.ptr) {
                divider2_id = index + 1;
            },
            else => {},
        }
    }

    try ps.solution(divider1_id * divider2_id);
}

fn lessThanContext(_: void, a: Expr, b: Expr) bool {
    return Expr.lessThan(a, b);
}

const example_input =
    \\[1,1,3,1,1]
    \\[1,1,5,1,1]
    \\
    \\[[1],[2,3,4]]
    \\[[1],4]
    \\
    \\[9]
    \\[[8,7,6]]
    \\
    \\[[4,4],4,4]
    \\[[4,4],4,4,4]
    \\
    \\[7,7,7,7]
    \\[7,7,7]
    \\
    \\[]
    \\[3]
    \\
    \\[[[]]]
    \\[[]]
    \\
    \\[1,[2,[3,[4,[5,6,7]]]],8,9]
    \\[1,[2,[3,[4,[5,6,0]]]],8,9]
;

test "parts 1 and 2" {
    const solutions = try runner.testSolver(@This(), example_input);
    try std.testing.expectEqual(@as(u64, 13), solutions[0].integer);
    try std.testing.expectEqual(@as(u64, 140), solutions[1].integer);
}
