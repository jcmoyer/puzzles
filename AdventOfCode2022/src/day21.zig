const std = @import("std");
const Allocator = std.mem.Allocator;
const sr = @import("sr/sr.zig");

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

const Name = [4]u8;
const NodeHandle = usize;

const Binop = struct {
    left: NodeHandle,
    right: NodeHandle,
};

const Node = union(enum) {
    undef,
    constant: i64,
    add: Binop,
    sub: Binop,
    mul: Binop,
    div: Binop,
    eq: Binop,

    fn eval(self: Node, ctx: ExprTree) i64 {
        return switch (self) {
            .undef => @panic("eval undef node"),
            .constant => |k| k,
            .add => |op| ctx.getNodeByHandle(op.left).eval(ctx) + ctx.getNodeByHandle(op.right).eval(ctx),
            .sub => |op| ctx.getNodeByHandle(op.left).eval(ctx) - ctx.getNodeByHandle(op.right).eval(ctx),
            .mul => |op| ctx.getNodeByHandle(op.left).eval(ctx) * ctx.getNodeByHandle(op.right).eval(ctx),
            .div => |op| @divFloor(ctx.getNodeByHandle(op.left).eval(ctx), ctx.getNodeByHandle(op.right).eval(ctx)),
            .eq => |op| @boolToInt(ctx.getNodeByHandle(op.left).eval(ctx) == ctx.getNodeByHandle(op.right).eval(ctx)),
        };
    }

    fn asBinop(self: Node) Binop {
        return switch (self) {
            .undef => @panic("undef not a binop"),
            .constant => @panic("constant not a binop"),
            inline else => |x| x,
        };
    }

    fn asBinopOpt(self: Node) ?Binop {
        return switch (self) {
            .undef => null,
            .constant => null,
            inline else => |x| x,
        };
    }

    fn left(self: Node) NodeHandle {
        return self.asBinop().left;
    }

    fn right(self: Node) NodeHandle {
        return self.asBinop().right;
    }

    fn asChar(self: Node) u8 {
        return switch (self) {
            .undef => '?',
            .constant => 'k',
            .add => '+',
            .sub => '-',
            .mul => '*',
            .div => '/',
            .eq => '=',
        };
    }
};

const ExprTree = struct {
    name_to_node: std.AutoArrayHashMap(Name, usize),
    node_to_name: std.AutoArrayHashMap(usize, Name),
    nodes: std.ArrayList(Node),

    fn getOrAddNodePtr(self: *ExprTree, n: Name) !*Node {
        var gop = try self.name_to_node.getOrPut(n);
        if (gop.found_existing) {
            return &self.nodes.items[gop.value_ptr.*];
        } else {
            var ptr = try self.nodes.addOne();
            ptr.* = .undef;
            gop.value_ptr.* = self.nodes.items.len - 1;
            try self.node_to_name.putNoClobber(gop.value_ptr.*, n);
            return ptr;
        }
    }

    fn getOrAddNodeHandle(self: *ExprTree, n: Name) !usize {
        var gop = try self.name_to_node.getOrPut(n);
        if (gop.found_existing) {
            return gop.value_ptr.*;
        } else {
            var ptr = try self.nodes.addOne();
            ptr.* = .undef;
            gop.value_ptr.* = self.nodes.items.len - 1;
            try self.node_to_name.putNoClobber(gop.value_ptr.*, n);
            return gop.value_ptr.*;
        }
    }

    fn getHandleByName(self: ExprTree, n: Name) NodeHandle {
        return self.name_to_node.get(n).?;
    }

    fn getNodeByHandle(self: ExprTree, h: NodeHandle) *Node {
        return &self.nodes.items[h];
    }

    fn getName(self: ExprTree, h: NodeHandle) Name {
        return self.node_to_name.get(h).?;
    }

    fn checkNodes(self: ExprTree) void {
        for (self.nodes.items) |n, i| {
            if (n == .undef) {
                std.debug.print("undefined node {d}, name=`{s}`\n", .{ i, self.node_to_name.get(i).? });
            }
        }
    }

    fn hasChild(self: ExprTree, parent: NodeHandle, child: NodeHandle) bool {
        if (parent == child) {
            return true;
        }
        var node = self.getNodeByHandle(parent);
        return switch (node.*) {
            .undef => false,
            .constant => false,
            inline else => {
                if (node.left() == child) {
                    return true;
                }
                if (node.right() == child) {
                    return true;
                }
                return self.hasChild(node.left(), child) or self.hasChild(node.right(), child);
            },
        };
    }

    fn simplify(self: ExprTree, start: NodeHandle) !void {
        var humn = self.getHandleByName("humn"[0..4].*);
        if (self.getNodeByHandle(start) == self.getNodeByHandle(humn)) {
            return;
        }
        if (self.getNodeByHandle(start).* == .undef) {
            @panic("simplify undef");
        }
        if (self.getNodeByHandle(start).* == .constant) {
            return;
        }
        if (!self.hasChild(start, humn)) {
            const k = self.getNodeByHandle(start).eval(self);
            self.getNodeByHandle(start).* = .{ .constant = k };
            return;
        }
        var binop = self.getNodeByHandle(start).asBinop();
        if (self.hasChild(binop.left, humn)) {
            try self.simplify(binop.left);
        } else {
            const k = self.getNodeByHandle(binop.left).eval(self);
            self.getNodeByHandle(binop.left).* = .{ .constant = k };
        }
        if (self.hasChild(binop.right, humn)) {
            try self.simplify(binop.right);
        } else {
            const k = self.getNodeByHandle(binop.right).eval(self);
            self.getNodeByHandle(binop.right).* = .{ .constant = k };
        }
    }
};

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var t = ExprTree{
        .name_to_node = std.AutoArrayHashMap(Name, usize).init(ps.allocator),
        .node_to_name = std.AutoArrayHashMap(usize, Name).init(ps.allocator),
        .nodes = std.ArrayList(Node).init(ps.allocator),
    };

    var lines = sr.sliceLines(ps.input_text);
    while (lines.next()) |line| {
        var parts = std.mem.split(u8, line, ": ");

        const src = parts.next().?;
        const expr_str = parts.next().?;

        if (expr_str.len > 4) {
            // binop
            const expr_type = expr_str[5];
            var expr_operands = std.mem.tokenize(u8, expr_str, "*+/- ");
            const left_name = expr_operands.next().?;

            const right_name = expr_operands.next().?;

            var parent = try t.getOrAddNodePtr(src[0..4].*);
            var left = try t.getOrAddNodeHandle(left_name[0..4].*);
            var right = try t.getOrAddNodeHandle(right_name[0..4].*);

            parent = try t.getOrAddNodePtr(src[0..4].*);
            left = try t.getOrAddNodeHandle(left_name[0..4].*);
            right = try t.getOrAddNodeHandle(right_name[0..4].*);

            switch (expr_type) {
                '+' => parent.* = .{ .add = .{ .left = left, .right = right } },
                '-' => parent.* = .{ .sub = .{ .left = left, .right = right } },
                '*' => parent.* = .{ .mul = .{ .left = left, .right = right } },
                '/' => parent.* = .{ .div = .{ .left = left, .right = right } },
                else => unreachable,
            }
        } else {
            const expr_int = try std.fmt.parseInt(i64, expr_str, 10);
            var parent = try t.getOrAddNodePtr(src[0..4].*);
            parent.* = .{ .constant = expr_int };
        }
    }

    t.checkNodes();

    var root = t.getHandleByName("root"[0..4].*);
    var humn = t.getHandleByName("humn"[0..4].*);
    try t.simplify(root);
    try printTree(t, root);

    try ps.solution(t.getNodeByHandle(root).eval(t));

    // set up tree for part 2
    t.getNodeByHandle(root).* = .{
        .eq = .{
            .left = t.getNodeByHandle(root).left(),
            .right = t.getNodeByHandle(root).right(),
        },
    };
    var humn_ptr = t.getNodeByHandle(humn);
    humn_ptr.* = .{ .constant = 0 };
    var root_ptr = t.getNodeByHandle(root);

    // TODO: proper general purpose solution, solved this using python+z3 instead
    var i: usize = 3848301405000;
    while (true) {
        humn_ptr.constant = @intCast(i64, i);
        if (root_ptr.eval(t) == 1) {
            try ps.solution(i);
            break;
        }
        i += 1;
    }
}

fn printTree(t: ExprTree, start: NodeHandle) !void {
    const name = t.getName(start);
    const node = t.getNodeByHandle(start);
    var w = std.io.getStdErr().writer();

    _ = try w.write(&name);
    _ = try w.write(": ");

    switch (node.*) {
        .undef => _ = try w.write("(?)"),
        .constant => |k| try w.print("{d}\n", .{k}),
        inline else => |op| {
            try w.print("{s} {c} {s}", .{ t.getName(op.left), node.asChar(), t.getName(op.right) });
            try w.writeByte('\n');
            try printTree(t, op.left);
            try printTree(t, op.right);
        },
    }
}
