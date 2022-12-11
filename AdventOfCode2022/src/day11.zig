const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Operation = union(enum) {
    times: u64,
    plus: u64,
    square,

    fn eval(self: Operation, old: u64) u64 {
        return switch (self) {
            .times => |x| old * x,
            .plus => |x| old + x,
            .square => old * old,
        };
    }

    fn evalModular(self: Operation, old: u64, divisor: u64) u64 {
        return switch (self) {
            .times => |x| (old * x) % divisor,
            .plus => |x| (old + x) % divisor,
            .square => (old * old) % divisor,
        };
    }
};

const Monkey = struct {
    id: usize,
    items: std.ArrayListUnmanaged(u64),
    op: Operation,
    test_div: u64,
    true_id: usize,
    false_id: usize,
    items_inspected: usize = 0,

    fn isDivisible(self: Monkey, val: u64) bool {
        return val % self.test_div == 0;
    }
};

const MonkeySet = struct {
    monkeys: []Monkey,
    mod: ?u64 = null,

    fn setModFromMonkeys(self: *MonkeySet) void {
        var prod: u64 = 1;
        for (self.monkeys) |m| {
            prod *= m.test_div;
        }
        self.mod = prod;
    }

    fn doRound(self: *MonkeySet, allocator: Allocator) !void {
        for (self.monkeys) |*m| {
            for (m.items.items) |*val| {
                var new_val: u64 = 0;
                if (self.mod) |mod| {
                    new_val = m.op.evalModular(val.*, mod);
                } else {
                    new_val = m.op.eval(val.*);
                    new_val /= 3;
                }
                if (m.isDivisible(new_val)) {
                    try self.monkeys[m.true_id].items.append(allocator, new_val);
                } else {
                    try self.monkeys[m.false_id].items.append(allocator, new_val);
                }
            }
            m.items_inspected += m.items.items.len;
            m.items.clearRetainingCapacity();
        }
    }

    fn doRounds(self: *MonkeySet, allocator: Allocator, n: usize) !void {
        var i: usize = 0;
        while (i < n) : (i += 1) {
            try self.doRound(allocator);
        }
    }

    fn monkeyBusiness(self: *MonkeySet) u64 {
        const InspectionsDesc = struct {
            fn f(_: void, a: Monkey, b: Monkey) bool {
                return a.items_inspected > b.items_inspected;
            }
        };
        std.sort.sort(Monkey, self.monkeys, {}, InspectionsDesc.f);
        return self.monkeys[0].items_inspected * self.monkeys[1].items_inspected;
    }

    fn clone(self: MonkeySet, allocator: Allocator) !MonkeySet {
        const new_monkeys = try allocator.alloc(Monkey, self.monkeys.len);
        for (new_monkeys) |*m, i| {
            m.* = self.monkeys[i];
            m.items = try m.items.clone(allocator);
        }
        return MonkeySet{
            .monkeys = new_monkeys,
        };
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var monkeys = std.ArrayListUnmanaged(Monkey){};
    var it = std.mem.tokenize(u8, ps.input_text, "\r\n");
    while (it.next()) |line| {
        const id_str = line[7..8];
        const inventory_str = it.next().?[18..];
        const op_str = it.next().?[23..];
        const div_str = it.next().?[21..];
        const true_str = it.next().?[29..];
        const false_str = it.next().?[30..];

        const id = try std.fmt.parseInt(usize, id_str, 10);
        var inv = std.ArrayListUnmanaged(u64){};

        var inv_it = std.mem.tokenize(u8, inventory_str, ", ");
        while (inv_it.next()) |item| {
            const item_int = try std.fmt.parseInt(u64, item, 10);
            try inv.append(ps.allocator, item_int);
        }

        var op: Operation = undefined;
        if (std.mem.eql(u8, op_str, "* old")) {
            op = .square;
        } else if (std.mem.indexOfScalar(u8, op_str, '*') != null) {
            const k = try std.fmt.parseInt(u64, op_str[2..], 10);
            op = .{ .times = k };
        } else if (std.mem.indexOfScalar(u8, op_str, '+') != null) {
            const k = try std.fmt.parseInt(u64, op_str[2..], 10);
            op = .{ .plus = k };
        }

        const div = try std.fmt.parseInt(u64, div_str, 10);
        const true_id = try std.fmt.parseInt(usize, true_str, 10);
        const false_id = try std.fmt.parseInt(usize, false_str, 10);

        const m = Monkey{
            .id = id,
            .items = inv,
            .op = op,
            .test_div = div,
            .true_id = true_id,
            .false_id = false_id,
        };
        try monkeys.append(ps.allocator, m);
    }

    var ms_part1 = MonkeySet{
        .monkeys = monkeys.items,
    };
    var ms_part2 = try ms_part1.clone(ps.allocator);
    ms_part2.setModFromMonkeys();

    try ms_part1.doRounds(ps.allocator, 20);
    try ms_part2.doRounds(ps.allocator, 10000);

    try ps.solution(ms_part1.monkeyBusiness());
    try ps.solution(ms_part2.monkeyBusiness());
}
