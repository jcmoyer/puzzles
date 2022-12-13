const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

fn matchNext(scanner: *Scanner, ch: u8) bool {
    if (scanner.lookahead(1)) |next| {
        return next == ch;
    } else {
        return false;
    }
}

pub fn untilCharacter(comptime ch: u8) fn (u8) bool {
    const Closure = struct {
        pub fn cond(x: u8) bool {
            return x == ch;
        }
    };
    return Closure.cond;
}

/// Equivalent to regex \w
fn isWordCharacter(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_';
}

pub fn parse(comptime fmt: []const u8, str: []const u8, args: anytype) !bool {
    const ArgsType = @TypeOf(args);
    if (@typeInfo(ArgsType) != .Struct) {
        @compileError("Expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields = std.meta.fields(ArgsType);

    comptime var fmt_scanner = Scanner.init(fmt);
    var str_scanner = Scanner.init(str);
    comptime var field_index = 0;

    inline while (comptime !fmt_scanner.atEnd()) {
        const want = comptime fmt_scanner.readOne() orelse break;
        if (str_scanner.atEnd()) {
            return false;
        }
        //const have = strScanner.current();

        //std.debug.print("'{c}' '{c}'\n", .{want, have});

        if (comptime want == '{') {
            const specifier = comptime fmt_scanner.lookahead(0);
            if (comptime specifier == @as(u8, 'd')) {
                const rbracket = comptime fmt_scanner.lookahead(1);
                if (comptime rbracket == @as(u8, '}')) {
                    comptime fmt_scanner.skip(2);
                } else {
                    @compileError("expected '}'");
                }

                const pointer_child = std.meta.Child(fields[field_index].field_type);

                @field(args, fields[field_index].name).* = try str_scanner.readInt(pointer_child);
                field_index += 1;
            } else if (comptime specifier == @as(u8, 'w')) {
                const rbracket = comptime fmt_scanner.lookahead(1);
                if (comptime rbracket == @as(u8, '}')) {
                    comptime fmt_scanner.skip(2);
                } else {
                    @compileError("expected '}'");
                }

                @field(args, fields[field_index].name).* = str_scanner.readWhile(isWordCharacter);
                field_index += 1;
            } else {
                @compileError("unknown specifier '" ++ &[_]u8{specifier.?} ++ "' after '{', rest of format string: '" ++ fmt_scanner.readToEnd() ++ "'");
            }
        } else {
            if (str_scanner.readOne() != want) {
                return false;
            }
        }
    }

    return fmt_scanner.atEnd() and str_scanner.atEnd();
}

test "parse" {
    {
        const result = try parse("abcd", "abcd", .{});
        try expectEqual(true, result);
    }
    {
        var val: u32 = 0;
        var bot: u32 = 0;
        const result = try parse("value {d} goes to bot {d}", "value 1 goes to bot 2", .{ &val, &bot });
        try expectEqual(true, result);
        try expectEqual(@as(u32, 1), val);
        try expectEqual(@as(u32, 2), bot);
    }
    {
        var val: u32 = 0;
        const result = try parse("value={d}", "value", .{&val});
        try expectEqual(false, result);
    }
    {
        var world: []const u8 = undefined;
        const result = try parse("hello {w}", "hello world", .{&world});
        try expectEqual(true, result);
        try expectEqual(true, std.mem.eql(u8, world, "world"));
    }
}
