const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ArgParseError = error{
    /// Returned when a named argument cannot be handled, or a positional
    /// argument was unexpected.
    UnknownArg,
};

pub const ArgParseState = struct {
    args: []const [:0]const u8,
    arg_index: usize,
    exit_parse: bool = false,

    pub fn getArgChecked(self: ArgParseState, i: usize) error{OutOfRange}![:0]const u8 {
        if (i >= self.args.len) {
            return error.OutOfRange;
        }
        return self.args[i];
    }

    pub fn current(self: ArgParseState) [:0]const u8 {
        return self.args[self.arg_index];
    }

    pub fn consume(self: *ArgParseState, n: usize) error{OutOfRange}!void {
        if (self.arg_index + n >= self.args.len) {
            return error.OutOfRange;
        }
        self.arg_index += n;
    }

    pub fn consumeNext(self: *ArgParseState) error{OutOfRange}![:0]const u8 {
        const result = try self.getArgChecked(self.arg_index + 1);
        try self.consume(1);
        return result;
    }

    pub fn exit(self: *ArgParseState) void {
        self.exit_parse = true;
    }
};

/// Parses arguments into `p`. The type of `p` must implement the following
/// methods:
/// ```
/// pub fn handlePositionalArg(self: *P, state: *ArgParseState, arg: []const u8) !void {}
/// pub fn handleShortArg(self: *P, state: *ArgParseState, shortname: u8) !void {}
/// pub fn handleLongArg(self: *P, state: *ArgParseState, longname: []const u8) !void {}
/// ```
pub fn parse(args: []const [:0]const u8, p: anytype) !void {
    var state = ArgParseState{
        .args = args,
        .arg_index = 1,
    };

    while (!state.exit_parse and state.arg_index < args.len) : (state.arg_index += 1) {
        const a = state.current();
        if (a[0] == '-' and a[1] == '-') {
            try handleLongArg(&state, p);
        } else if (a[0] == '-') {
            try handleShortArg(&state, p);
        } else {
            try handlePositionalArg(&state, p);
        }
    }
}

/// Convenience function that calls `parse` on a default-constructed `T` and
/// returns the result. `T` must meet the requirements of `parse`.
pub fn parseDefault(comptime T: type, args: []const [:0]const u8) !T {
    var params = T{};
    try parse(args, &params);
    return params;
}

fn handlePositionalArg(state: *ArgParseState, p: anytype) !void {
    const arg = state.current();
    try p.handlePositionalArg(state, arg);
}

fn handleShortArg(state: *ArgParseState, p: anytype) !void {
    const shortname = state.current()[1];
    try p.handleShortArg(state, shortname);
}

fn handleLongArg(state: *ArgParseState, p: anytype) !void {
    const longname = state.current()[2..];
    try p.handleLongArg(state, longname);
}

const TestParameters = struct {
    root_dir: ?[]const u8 = null,
    help: bool = false,
    foo: usize = 0,

    fn handlePositionalArg(self: *TestParameters, state: *ArgParseState, arg: []const u8) !void {
        _ = state;

        self.root_dir = arg;
    }

    fn handleShortArg(self: *TestParameters, state: *ArgParseState, shortname: u8) !void {
        _ = state;

        switch (shortname) {
            'h', '?' => {
                self.help = true;
            },
            else => return error.UnknownArg,
        }
    }

    fn handleLongArg(self: *TestParameters, state: *ArgParseState, longname: []const u8) !void {
        if (std.mem.eql(u8, longname, "help")) {
            self.help = true;
        } else if (std.mem.eql(u8, longname, "foo")) {
            self.foo = try std.fmt.parseInt(usize, try state.consumeNext(), 10);
        } else {
            return error.UnknownArg;
        }
    }
};

test "parse" {
    var p = TestParameters{};

    try parse(&[_][:0]const u8{ "test.exe", "--help", "D:/test", "--foo", "2" }, &p);

    try std.testing.expect(p.help);
    try std.testing.expectEqualStrings("D:/test", p.root_dir.?);
    try std.testing.expectEqual(@as(usize, 2), p.foo);
}
