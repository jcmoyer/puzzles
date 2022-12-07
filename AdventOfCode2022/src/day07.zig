const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    const s = try Solution.solve(ps.allocator, ps.input_text);
    try ps.solution(s.part1);
    try ps.solution(s.part2);
}

const FSDir = struct {
    parent: *FSDir,
    dirname: []const u8,
    children: std.ArrayListUnmanaged(FSEnt) = .{},

    fn addSubDir(self: *FSDir, a: Allocator, subdir_name: []const u8) !void {
        var new_dir = try self.children.addOne(a);
        new_dir.* = .{
            .dir = .{
                .parent = self,
                .dirname = subdir_name,
            },
        };
    }

    fn getSubDir(self: *FSDir, subdir_name: []const u8) ?*FSDir {
        for (self.children.items) |*ent| {
            switch (ent.*) {
                .dir => |*d| if (std.mem.eql(u8, d.dirname, subdir_name)) {
                    return d;
                },
                else => {},
            }
        }
        return null;
    }

    fn addFile(self: *FSDir, a: Allocator, filename: []const u8, sz: u64) !void {
        var new_file = try self.children.addOne(a);
        new_file.* = .{
            .file = .{
                .filename = filename,
                .size = sz,
            },
        };
    }

    fn size(self: FSDir) u64 {
        var sum: u64 = 0;
        for (self.children.items) |ent| {
            switch (ent) {
                .dir => |d| sum += d.size(),
                .file => |f| sum += f.size,
            }
        }
        return sum;
    }

    fn walkDirsRecursive(self: *FSDir, userdata: anytype, f: *const fn (*FSDir, @TypeOf(userdata)) void) void {
        f(self, userdata);
        for (self.children.items) |*ent| {
            if (ent.* == .dir) {
                ent.dir.walkDirsRecursive(userdata, f);
            }
        }
    }
};

const FSFile = struct {
    filename: []const u8,
    size: u64,
};

const FSEnt = union(enum) {
    dir: FSDir,
    file: FSFile,
};

const State = struct {
    cwd: *FSDir,
};

fn runCommands(arena: Allocator, text: []const u8) !FSDir {
    var root = FSDir{
        .parent = undefined,
        .dirname = "/",
    };
    root.parent = &root; // :O

    var state = State{
        .cwd = &root,
    };

    var lines = std.mem.tokenize(u8, text, "\r\n");
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "$ cd")) {
            const path = line[5..];
            if (path[0] == '/') continue;
            if (std.mem.eql(u8, path, "..")) {
                state.cwd = state.cwd.parent;
                continue;
            }
            state.cwd = state.cwd.getSubDir(path).?;
        } else if (std.mem.startsWith(u8, line, "$ ls")) {
            // do nothing
        } else if (std.mem.startsWith(u8, line, "dir")) {
            const subdir = line[4..];
            try state.cwd.addSubDir(arena, subdir);
        } else {
            var split = std.mem.split(u8, line, " ");
            const size = try std.fmt.parseInt(u64, split.next().?, 10);
            const file = split.next().?;
            try state.cwd.addFile(arena, file, size);
        }
    }

    return root;
}

const Solution = struct {
    part1: u64,
    part2: u64,

    fn solve(arena: Allocator, cmds: []const u8) !Solution {
        var root = try runCommands(arena, cmds);

        var sum: u64 = 0;
        root.walkDirsRecursive(&sum, SumUnder100K.f);

        var dstate = MinDeletable{
            .space_used = root.size(),
        };
        root.walkDirsRecursive(&dstate, MinDeletable.f);

        return Solution{
            .part1 = sum,
            .part2 = dstate.min_deletable,
        };
    }
};

const SumUnder100K = struct {
    fn f(d: *FSDir, sum: *u64) void {
        if (d.size() <= 100000) {
            sum.* += d.size();
        }
    }
};

const MinDeletable = struct {
    const total_space = 70000000;
    const space_req = 30000000;

    space_used: u64,
    min_deletable: u64 = std.math.maxInt(u64),

    fn f(d: *FSDir, self: *MinDeletable) void {
        const space_avail = total_space - self.space_used;
        const delete_must_be_at_least = space_req - space_avail;
        if (d.size() >= delete_must_be_at_least) {
            self.min_deletable = std.math.min(self.min_deletable, d.size());
        }
    }
};

const example_input =
    \\$ cd /
    \\$ ls
    \\dir a
    \\14848514 b.txt
    \\8504156 c.dat
    \\dir d
    \\$ cd a
    \\$ ls
    \\dir e
    \\29116 f
    \\2557 g
    \\62596 h.lst
    \\$ cd e
    \\$ ls
    \\584 i
    \\$ cd ..
    \\$ cd ..
    \\$ cd d
    \\$ ls
    \\4060174 j
    \\8033020 d.log
    \\5626152 d.ext
    \\7214296 k
;

test "parts 1 and 2" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    var allocator = arena.allocator();
    defer arena.deinit();

    const s = try Solution.solve(allocator, example_input);

    try std.testing.expectEqual(@as(usize, 95437), s.part1);
    try std.testing.expectEqual(@as(usize, 24933642), s.part2);
}
