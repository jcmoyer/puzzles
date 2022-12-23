const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Graph = struct {
    nodes: std.ArrayListUnmanaged(Node) = .{},
    name_to_index: std.StringArrayHashMapUnmanaged(usize) = .{},
    dist_map: sr.Array2D(u16) = .{},

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

    fn getNamedNode(self: Graph, name: []const u8) usize {
        const index = self.name_to_index.get(name) orelse @panic("node not in graph");
        return index;
    }

    fn getNamedNodePtr(self: Graph, name: []const u8) *Node {
        const index = self.name_to_index.get(name) orelse @panic("node not in graph");
        return &self.nodes.items[index];
    }

    /// Finish building the graph and compute distances between all nodes using Floyd-Warshall
    fn finalize(self: *Graph, a: Allocator) !void {
        try self.dist_map.resize(a, self.nodes.items.len, self.nodes.items.len);
        self.dist_map.fill(std.math.maxInt(u16));
        for (self.nodes.items) |n| {
            for (n.edges.slice()) |e| {
                self.dist_map.atPtr(n.id, e).* = 1;
            }
            self.dist_map.atPtr(n.id, n.id).* = 0;
        }
        const node_count = self.nodes.items.len;
        var i: usize = 0;
        var j: usize = 0;
        var k: usize = 0;
        while (k < node_count) : (k += 1) {
            i = 0;
            while (i < node_count) : (i += 1) {
                j = 0;
                while (j < node_count) : (j += 1) {
                    var dist_ptr = self.dist_map.atPtr(i, j);
                    dist_ptr.* = std.math.min(
                        self.dist_map.at(i, k) +| self.dist_map.at(k, j),
                        dist_ptr.*,
                    );
                }
            }
        }
    }

    fn distance(self: Graph, from: usize, to: usize) u16 {
        return self.dist_map.at(from, to);
    }

    fn getNodeById(self: Graph, id: usize) *const Node {
        return &self.nodes.items[id];
    }
};

const Node = struct {
    name: []const u8,
    id: usize,
    rate: u16 = 0,
    edges: std.BoundedArray(usize, 8),
};

const ExplorationState = struct {
    opened: u64,
    pressure: u16,
    time: u8,
    pos: u8,

    fn compare(_: void, a: ExplorationState, b: ExplorationState) std.math.Order {
        return std.math.order(b.pressure, a.pressure);
    }
};

const SearchOptions = struct {
    init_opened: u64 = 0,
    init_pressure: u16 = 0,
    time_limit: u8 = 30,
    twice: bool = false,
};

fn findMaxPressure(g: *Graph, a: Allocator, opts: SearchOptions) !ExplorationState {
    var look = std.PriorityQueue(ExplorationState, void, ExplorationState.compare).init(a, {});

    const start = g.getNamedNode("AA");
    const initial_state = ExplorationState{
        .opened = opts.init_opened,
        .pressure = opts.init_pressure,
        .time = opts.time_limit,
        .pos = @intCast(u8, start),
    };
    try look.add(initial_state);

    var best_state = initial_state;

    while (look.len > 0) {
        const next = look.remove();
        if (next.pressure > best_state.pressure) {
            best_state = next;
        }
        var neighbor: usize = 0;
        while (neighbor < g.nodes.items.len) : (neighbor += 1) {
            if (@intCast(u8, neighbor) == next.pos) {
                continue;
            }

            const neighbor_rate = g.getNodeById(neighbor).rate;
            if (neighbor_rate == 0) {
                continue;
            }

            const neighbor_mask = @as(u64, 1) << @intCast(u6, neighbor);
            if ((next.opened & neighbor_mask) != 0) {
                continue;
            }

            const dist = g.distance(next.pos, neighbor);
            if (dist >= next.time) {
                if (opts.twice) {
                    const state = try findMaxPressure(g, a, .{
                        .time_limit = opts.time_limit,
                        .init_opened = next.opened,
                        .init_pressure = next.pressure,
                        .twice = false,
                    });
                    if (state.pressure > best_state.pressure) {
                        best_state = state;
                        std.debug.print("{d}\n", .{state.pressure});
                    }
                }
                continue;
            }

            const neighbor_state = ExplorationState{
                .opened = next.opened | neighbor_mask,
                .pressure = next.pressure + (g.getNodeById(neighbor).rate * (next.time - dist - 1)),
                .time = next.time - @intCast(u8, dist) - 1,
                .pos = @intCast(u8, neighbor),
            };

            if (neighbor_state.pressure == next.pressure) {
                continue;
            }
            // if (neighbor_state.time == 0) {
            //     continue;
            // }
            // if (seen.contains(neighbor_state)) {
            //     continue;
            // }
            try look.add(neighbor_state);
        }
    }

    return best_state;
}

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
    try g.finalize(ps.allocator);
    for (g.nodes.items) |n| {
        std.debug.print("{s}({d}) leading to {d} nodes: ", .{ n.name, n.id, n.edges.len });
        for (n.edges.slice()) |e| {
            std.debug.print("{s}({d},dist={d}) ", .{ g.nodes.items[e].name, g.nodes.items[e].id, g.distance(n.id, e) });
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("{d} total nodes\n", .{g.nodes.items.len});

    const state_part1 = try findMaxPressure(&g, ps.allocator, .{});
    try ps.solution(state_part1.pressure);

    const state_part2 = try findMaxPressure(&g, ps.allocator, .{ .time_limit = 26, .twice = true });
    try ps.solution(state_part2.pressure);
}
