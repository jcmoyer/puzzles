const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Graph(comptime K: type, comptime EdgeProps: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        nodes: std.AutoArrayHashMap(K, void),
        edges: std.AutoArrayHashMap(K, std.AutoArrayHashMap(K, EdgeProps)),
        back_edges: std.AutoArrayHashMap(K, std.AutoArrayHashMap(K, void)),

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .nodes = std.AutoArrayHashMap(K, void).init(allocator),
                .edges = std.AutoArrayHashMap(K, std.AutoArrayHashMap(K, EdgeProps)).init(allocator),
                .back_edges = std.AutoArrayHashMap(K, std.AutoArrayHashMap(K, void)).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.nodes.deinit();

            for (self.edges.values()) |*val| {
                val.deinit();
            }
            self.edges.deinit();

            for (self.back_edges.values()) |*val| {
                val.deinit();
            }
            self.back_edges.deinit();
        }

        pub fn addVertex(self: *Self, key: K) !void {
            try self.nodes.put(key, {});
        }

        pub fn addEdge(self: *Self, from: K, to: K) !void {
            {
                var ent = try self.edges.getOrPut(from);
                if (!ent.found_existing) {
                    ent.value_ptr.* = std.AutoArrayHashMap(K, EdgeProps).init(self.allocator);
                }
                try ent.value_ptr.put(to, {});
            }
            {
                var ent = try self.back_edges.getOrPut(to);
                if (!ent.found_existing) {
                    ent.value_ptr.* = std.AutoArrayHashMap(K, void).init(self.allocator);
                }
                try ent.value_ptr.put(from, {});
            }
        }

        pub fn addEdgeVertices(self: *Self, from: K, to: K) !void {
            try self.addVertex(from);
            try self.addVertex(to);
            try self.addEdge(from, to);
        }

        pub fn removeEdgesTo(self: *Self, to: K) void {
            for (self.edges.values()) |*edge_map| {
                _ = edge_map.swapRemove(to);
            }
        }

        pub fn removeEdgesFrom(self: *Self, from: K) void {
            if (self.edges.get(from)) |*edge_map| {
                edge_map.clearRetainingCapacity();
            }
        }

        pub const DfsIterator = struct {
            graph: *const Self,
            visited: std.AutoArrayHashMap(K, void),
            frontier: std.ArrayList(K),

            pub fn next(self: *DfsIterator) !?K {
                if (self.frontier.items.len == 0) {
                    return null;
                }
                var node = self.frontier.pop();

                while (self.visited.contains(node)) {
                    if (self.frontier.items.len == 0) {
                        return null;
                    }
                    node = self.frontier.pop();
                }

                try self.visited.put(node, {});

                if (self.graph.edges.get(node)) |edges| {
                    for (edges.keys()) |to| {
                        try self.frontier.append(to);
                    }
                }

                return node;
            }

            pub fn init(allocator: Allocator, parent: *const Self, from: K) !DfsIterator {
                var frontier = std.ArrayList(K).init(allocator);
                errdefer frontier.deinit();
                try frontier.append(from);
                return DfsIterator{
                    .graph = parent,
                    .visited = std.AutoArrayHashMap(K, void).init(allocator),
                    .frontier = frontier,
                };
            }

            pub fn deinit(self: *DfsIterator) void {
                self.visited.deinit();
                self.frontier.deinit();
            }
        };

        /// Handles communication between the visitor and in-progress DFS.
        pub fn VisitContext(comptime UserContext: type) type {
            return struct {
                /// Internal use only.
                graph: *const Self,
                /// Internal use only.
                new_nodes: *std.ArrayList(VisitState(UserContext)),
                /// Internal use only.
                cancel_flag: bool = false,
                /// Node currently being visited.
                node: K,
                /// State at the current node.
                state: UserContext,

                pub fn edges(self: @This(), from: K) []K {
                    if (self.graph.edges.get(from)) |es| {
                        return es.keys();
                    } else {
                        return &[_]K{};
                    }
                }

                pub fn post(self: *@This(), n: K, pathctx: UserContext) !void {
                    try self.new_nodes.append(.{ .cursor = n, .ctx = pathctx });
                }

                pub fn cancel(self: *@This()) void {
                    self.cancel_flag = true;
                }
            };
        }

        fn VisitState(comptime UserContext: type) type {
            return struct {
                cursor: K,
                ctx: UserContext,
            };
        }

        pub fn dfsVisit(
            self: *const Self,
            from: K,
            context: anytype,
            visitor: fn (*VisitContext(@TypeOf(context))) anyerror!void,
        ) !void {
            const VC = VisitContext(@TypeOf(context));
            const VS = VisitState(@TypeOf(context));

            var frontier = std.ArrayList(VS).init(self.allocator);
            defer frontier.deinit();

            var vc = VC{ .graph = self, .new_nodes = &frontier, .node = undefined, .state = undefined };
            defer vc.new_nodes.deinit();

            try frontier.append(.{ .cursor = from, .ctx = context });

            while (frontier.items.len > 0) {
                var node = frontier.pop();
                vc.node = node.cursor;
                vc.state = node.ctx;
                try visitor(&vc);
            }
        }

        pub fn dfs(self: *const Self, from: K) !DfsIterator {
            return try DfsIterator.init(self.allocator, self, from);
        }

        pub const BackDfsIterator = struct {
            graph: *const Self,
            visited: std.AutoArrayHashMap(K, void),
            frontier: std.ArrayList(K),

            pub fn next(self: *BackDfsIterator) !?K {
                if (self.frontier.items.len == 0) {
                    return null;
                }
                var node = self.frontier.pop();

                while (self.visited.contains(node)) {
                    if (self.frontier.items.len == 0) {
                        return null;
                    }
                    node = self.frontier.pop();
                }

                try self.visited.put(node, {});

                if (self.graph.back_edges.get(node)) |edges| {
                    for (edges.keys()) |to| {
                        try self.frontier.append(to);
                    }
                }

                return node;
            }

            pub fn init(allocator: Allocator, parent: *const Self, from: K) !BackDfsIterator {
                var frontier = std.ArrayList(K).init(allocator);
                errdefer frontier.deinit();
                try frontier.append(from);
                return BackDfsIterator{
                    .graph = parent,
                    .visited = std.AutoArrayHashMap(K, void).init(allocator),
                    .frontier = frontier,
                };
            }

            pub fn deinit(self: *BackDfsIterator) void {
                self.visited.deinit();
                self.frontier.deinit();
            }
        };

        pub fn backDfs(self: *const Self, from: K) !BackDfsIterator {
            return try BackDfsIterator.init(self.allocator, self, from);
        }
    };
}

test "graph" {
    const T = Graph(i32, void);
    var g = T.init(std.testing.allocator);
    defer g.deinit();

    try g.addVertex(123);
    try g.addVertex(234);
    try g.addVertex(345);
    try g.addVertex(999);

    try g.addEdge(123, 234);
    try g.addEdge(234, 345);
    try g.addEdge(999, 123);

    var dfs = try g.dfs(123);
    defer dfs.deinit();

    try std.testing.expectEqual(@as(?i32, 123), try dfs.next());
    try std.testing.expectEqual(@as(?i32, 234), try dfs.next());
    try std.testing.expectEqual(@as(?i32, 345), try dfs.next());
    try std.testing.expectEqual(@as(?i32, null), try dfs.next());

    var bdfs = try g.backDfs(345);
    defer bdfs.deinit();

    try std.testing.expectEqual(@as(?i32, 345), try bdfs.next());
    try std.testing.expectEqual(@as(?i32, 234), try bdfs.next());
    try std.testing.expectEqual(@as(?i32, 123), try bdfs.next());
    try std.testing.expectEqual(@as(?i32, 999), try bdfs.next());
    try std.testing.expectEqual(@as(?i32, null), try bdfs.next());
}
