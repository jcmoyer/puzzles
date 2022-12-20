const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

fn printList(q: std.TailQueue(i64)) void {
    var i = q.first;
    while (i != null) : (i = i.?.next) {
        std.debug.print("{d}, ", .{i.?.data});
    }
    std.debug.print("\n", .{});
}

fn index(
    list: std.TailQueue(i64),
    index_to_node: *std.ArrayList(*std.TailQueue(i64).Node),
    node_to_index: *std.AutoArrayHashMap(*std.TailQueue(i64).Node, usize),
) !void {
    index_to_node.clearRetainingCapacity();
    node_to_index.clearRetainingCapacity();
    var n = list.first;
    var i: usize = 0;
    while (n != null) : (n = n.?.next) {
        try index_to_node.append(n.?);
        try node_to_index.put(n.?, i);
        i += 1;
    }
}

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var list = std.TailQueue(i64){};
    var ints = std.ArrayList(i64).init(ps.allocator);
    var node_lookup = std.AutoArrayHashMap(struct { i64, usize }, *std.TailQueue(i64).Node).init(ps.allocator);
    var zero: ?*std.TailQueue(i64).Node = null;

    var index_to_node = std.ArrayList(*std.TailQueue(i64).Node).init(ps.allocator);
    var node_to_index = std.AutoArrayHashMap(*std.TailQueue(i64).Node, usize).init(ps.allocator);

    var lines = sr.sliceLines(ps.input_text);
    var index_lookup: usize = 0;
    while (lines.next()) |line| {
        var k = try std.fmt.parseInt(i64, line, 10);
        k *= 811589153;

        var n = try ps.allocator.create(std.TailQueue(i64).Node);
        n.* = .{
            .data = k,
        };
        if (k == 0) {
            zero = n;
        }
        list.append(n);
        try ints.append(k);
        try node_lookup.putNoClobber(.{ k, index_lookup }, n);
        index_lookup += 1;
    }

    try index(list, &index_to_node, &node_to_index);

    var mix: usize = 0;
    while (mix < 10) : (mix += 1) {
        for (ints.items) |k, k_ind| {
            // printList(list);
            var n = node_lookup.get(.{ k, k_ind }).?;
            if (n.data < 0) {
                const left = n.prev orelse list.last.?;
                list.remove(n);
                try index(list, &index_to_node, &node_to_index);
                const before_index = @mod(@intCast(i64, node_to_index.get(left).?) + n.data + 1, @intCast(i64, list.len));
                // std.debug.print("left={d}, before_index={d}\n",.{node_to_index.get(left).?,before_index});
                list.insertBefore(index_to_node.items[@intCast(usize, before_index)], n);
            } else if (n.data > 0) {
                const right = n.next orelse list.first.?;
                list.remove(n);
                try index(list, &index_to_node, &node_to_index);
                const after_index = @mod(@intCast(i64, node_to_index.get(right).?) + n.data - 1, @intCast(i64, list.len));
                // std.debug.print("right={d}, after_index={d}\n",.{node_to_index.get(right).?,after_index});
                list.insertAfter(index_to_node.items[@intCast(usize, after_index)], n);
            }
            try index(list, &index_to_node, &node_to_index);
        }
        // printList(list);

        // var zero = z //node_lookup.get(0).?;
        var j: usize = 1;
        var n = zero.?.next orelse list.first.?;
        var sum: i64 = 0;
        while (j < 1000) : (j += 1) {
            n = n.next orelse list.first.?;
        }
        std.debug.print("{d}\n", .{n.data});
        sum += n.data;
        j = 0;
        // n=zero.next orelse list.first.?;
        while (j < 1000) : (j += 1) {
            n = n.next orelse list.first.?;
        }
        std.debug.print("{d}\n", .{n.data});
        sum += n.data;
        j = 0;
        // n=zero.next orelse list.first.?;
        while (j < 1000) : (j += 1) {
            n = n.next orelse list.first.?;
        }
        std.debug.print("{d}\n", .{n.data});
        sum += n.data;

        std.debug.print("{d}\n", .{sum});
    }
}
