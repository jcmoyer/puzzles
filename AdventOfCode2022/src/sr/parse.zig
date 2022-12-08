const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

fn matchNext(scanner: *Scanner, ch: u8) bool {
    if (scanner.peek()) |next| {
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

pub fn parse(comptime fmt: []const u8, str: []const u8, args: anytype) bool {
    // Taken from std.fmt.format
    const ArgsType = @TypeOf(args);
    // XXX: meta.trait.is(.Struct)(ArgsType) doesn't seem to work...
    if (@typeInfo(ArgsType) != .Struct) {
        @compileError("Expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields = std.meta.fields(ArgsType);

    comptime var fmtScanner = Scanner.init(fmt);
    var strScanner = Scanner.init(str);
    comptime var fieldIndex = 0;
    inline while (comptime !fmtScanner.atEnd()) {
        const want = comptime fmtScanner.current();
        if (strScanner.atEnd()) {
            return false;
        }
        //const have = strScanner.current();

        //std.debug.print("'{c}' '{c}'\n", .{want, have});

        if (want == '{') {
            if (comptime matchNext(&fmtScanner, '}')) {
                // check output type
                var ptr = @field(args, fields[fieldIndex].name);
                // TODO: ensure ptr is a ptr lol

                const ChildType = std.meta.Child(@TypeOf(ptr));

                if (comptime std.meta.trait.isIntegral(ChildType)) {
                    ptr.* = strScanner.readInt(ChildType) catch return false;
                    fieldIndex += 1;
                } else {
                    @compileError("unsupported pointer type");
                }
                // consume '{' and '}'
                comptime _ = fmtScanner.readOne();
                comptime _ = fmtScanner.readOne();
            } else if (comptime matchNext(&fmtScanner, 'd')) {
                comptime _ = fmtScanner.readOne();
                if (comptime !matchNext(&fmtScanner, '}')) {
                    @compileError("unmatched '{'");
                }
                comptime _ = fmtScanner.readOne();
                comptime _ = fmtScanner.readOne();

                var ptr = @field(args, fields[fieldIndex].name);
                if (comptime !std.meta.trait.isSingleItemPtr(@TypeOf(ptr))) {
                    @compileError("expected pointer to integer");
                }

                const Child = std.meta.Child(@TypeOf(ptr));
                if (comptime !std.meta.trait.isIntegral(Child)) {
                    @compileError("expected pointer to integer");
                }

                ptr.* = strScanner.readInt(Child) catch return false;
                fieldIndex += 1;
            } else if (comptime matchNext(&fmtScanner, 's')) {
                comptime _ = fmtScanner.readOne();
                if (comptime !matchNext(&fmtScanner, '}')) {
                    @compileError("unmatched '{'");
                }
                comptime _ = fmtScanner.readOne();
                comptime _ = fmtScanner.readOne();

                var ptr = @field(args, fields[fieldIndex].name);
                if (comptime !std.meta.trait.isSingleItemPtr(@TypeOf(ptr))) {
                    @compileError("expected pointer to []const u8");
                }

                const Child = std.meta.Child(@TypeOf(ptr));
                if (comptime Child != []const u8) {
                    @compileError("expected pointer to []const u8");
                }

                // anchor to next character
                if (comptime fmtScanner.atEnd()) {
                    ptr.* = strScanner.readToEnd();
                } else {
                    const anchor = comptime fmtScanner.current();
                    ptr.* = strScanner.readUntil(untilCharacter(anchor));
                }

                fieldIndex += 1;
            } else {
                @compileError("unmatched '{'");
            }
        } else {
            comptime _ = fmtScanner.readOne();
            _ = strScanner.readOne();
        }
    }

    return fmtScanner.atEnd() and strScanner.atEnd();
}

test "parse" {
    {
        const result = parse("abcd", "abcd", .{});
        try expectEqual(true, result);
    }
    {
        var val: u32 = 0;
        var bot: u32 = 0;
        const result = parse("value {} goes to bot {}", "value 1 goes to bot 2", .{ &val, &bot });
        try expectEqual(true, result);
        try expectEqual(@as(u32, 1), val);
        try expectEqual(@as(u32, 2), bot);
    }
    {
        var val: u32 = 0;
        const result = parse("value={}", "value", .{&val});
        try expectEqual(false, result);
    }
    {
        // ok: {s} anchors to end of string
        var world: []const u8 = undefined;
        const result = parse("hello {s}", "hello world", .{&world});
        try expectEqual(true, result);
        try expectEqual(true, std.mem.eql(u8, world, "world"));
    }
}
