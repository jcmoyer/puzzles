const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Graph = struct {
    nodes: std.ArrayListUnmanaged(Node) = .{},
    name_to_index: std.StringArrayHashMapUnmanaged(usize) = .{},

    fn getOrPut(self: *Graph, allocator: Allocator, name: []const u8) !usize {
        const gop = try self.name_to_index.getOrPut(allocator, name);
        if (gop.found_existing) {
            return gop.value_ptr.*;
        } else {
            const new_index = self.nodes.items.len;
            gop.value_ptr.* = new_index;
            var ptr = try self.nodes.addOne(allocator);
            ptr.* = .{
                .name = name,
                .id = new_index,
                .edges = .{},
            };
            return new_index;
        }
    }

    fn addEdgeNames(self: *Graph, a: Allocator, from: []const u8, to: []const u8) !void {
        var src = try self.getOrPut(a, from);
        var dst = try self.getOrPut(a, to);
        std.debug.assert(null == std.mem.indexOfScalar(usize, self.nodes.items[src].edges.slice(), dst));
        try self.nodes.items[src].edges.append(dst);
    }

    fn getNamedNodePtr(self: Graph, name: []const u8) *Node {
        const index = self.name_to_index.get(name) orelse @panic("node not in graph");
        return &self.nodes.items[index];
    }
};

const Node = struct {
    name: []const u8,
    id: usize,
    rate: u32 = 0,
    edges: std.BoundedArray(usize, 8),
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var g = Graph{};
    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        const src = line[6..8];
        const one_dest_text = "tunnel leads to valve ";
        const one_dest = std.mem.indexOf(u8, line, one_dest_text);
        const multi_dest_text = "tunnels lead to valves ";
        const multi_dest = std.mem.indexOf(u8, line, multi_dest_text);
        if (one_dest) |i| {
            const name = line[i + one_dest_text.len ..];
            try g.addEdgeNames(ps.allocator, src, name);
        } else if (multi_dest) |i| {
            const names = line[i + multi_dest_text.len ..];
            var names_it = std.mem.tokenize(u8, names, ", ");
            while (names_it.next()) |name| {
                try g.addEdgeNames(ps.allocator, src, name);
            }
        }
        const rate_start = std.mem.indexOf(u8, line, "rate=") orelse @panic("no rate specified for node");
        _ = try sr.parse("rate={d}", line[rate_start..], .{&g.getNamedNodePtr(src).rate});
    }
    for (g.nodes.items) |n| {
        std.debug.print("{s} leading to {d} nodes: ", .{ n.name, n.edges.len });
        for (n.edges.slice()) |e| {
            std.debug.print("{s} ", .{g.nodes.items[e].name});
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("{d} total nodes\n", .{g.nodes.items.len});
}
