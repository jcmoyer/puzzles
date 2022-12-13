const std = @import("std");

pub const Scanner = struct {
    buf: []const u8 = &[_]u8{},
    pos: usize = 0,

    pub fn init(buf: []const u8) Scanner {
        return Scanner{
            .buf = buf,
        };
    }

    /// Consumes characters while `cond` returns `true` for the current character.
    pub fn readWhile(self: *Scanner, cond: *const fn (u8) bool) []const u8 {
        const first = self.pos;
        while (self.pos < self.buf.len and cond(self.buf[self.pos])) : (self.pos += 1) {}
        return self.buf[first..self.pos];
    }

    /// Consumes characters while `cond` returns `false` for the current character.
    pub fn readUntil(self: *Scanner, cond: *const fn (u8) bool) []const u8 {
        const first = self.pos;
        while (self.pos < self.buf.len and !cond(self.buf[self.pos])) : (self.pos += 1) {}
        return self.buf[first..self.pos];
    }

    /// Moves to the next character `cond` returns `true` for. This function
    /// returns `true` if such a character was found, or `false` if the scanner
    /// reached the end of the string.
    pub fn moveToNext(self: *Scanner, cond: *const fn (u8) bool) bool {
        while (self.pos < self.buf.len and !cond(self.buf[self.pos])) : (self.pos += 1) {}
        // either we found a char for which cond passed OR we reached the end
        // so there's no need to re-evaluate cond
        return self.pos < self.buf.len;
    }

    /// Returns `true` if the scanner reached the end of the underlying string.
    pub fn atEnd(self: Scanner) bool {
        return self.pos == self.buf.len;
    }

    /// Advances the cursor `n` characters. `n` may be greater than the number
    /// of remaining characters.
    pub fn skip(self: *Scanner, n: usize) void {
        self.pos += n;
        if (self.pos > self.buf.len) {
            self.pos = self.buf.len;
        }
    }

    /// Starting at the current character, consume as many digits as possible
    /// and interpret them as an integer. A leading negative sign (`-`) is allowed.
    pub fn readInt(self: *Scanner, comptime T: type) !T {
        var start: usize = self.pos;
        if (self.peek() == @as(u8, '-')) {
            self.skip(1);
        }
        _ = self.readWhile(std.ascii.isDigit);
        var str = self.buf[start..self.pos];
        return std.fmt.parseInt(T, str, 10);
    }

    /// Reads until a newline or the end of the input is encountered. If a
    /// newline was found, the scanner advances past it. Returns the characters
    /// read prior to the newline, or the rest of the string if there was no
    /// newline. This function treats `[\r]\n` as a newline.
    pub fn readLine(self: *Scanner) []const u8 {
        const slice = self.readUntil(isNewline);
        if (self.atEnd()) {
            return slice;
        }
        // advance past \n
        self.skip(1);
        // remove optional \r
        if (std.mem.endsWith(u8, slice, "\r")) {
            return slice[0 .. slice.len - 1];
        }
        return slice;
    }

    /// Returns the current character without consuming it, or `null` if at the
    /// end of the string.
    pub fn peek(self: Scanner) ?u8 {
        if (self.atEnd()) {
            return null;
        } else {
            return self.buf[self.pos];
        }
    }

    /// Returns the character `n` characters ahead of the current one.
    /// `n == 0` is equivalent to `peek()`. If `n` would refer to a character
    /// past the end of the buffer, this function returns `null`.
    pub fn lookahead(self: Scanner, n: usize) ?u8 {
        const k = self.pos + n;
        if (k < self.buf.len) {
            return self.buf[k];
        } else {
            return null;
        }
    }

    /// Reads up to `n` characters from the input and returns a slice containing
    /// the characters read. If at the end of the string, returns an empty string.
    pub fn read(self: *Scanner, n: usize) []const u8 {
        const first = self.pos;
        var i: usize = 0;
        while (self.pos < self.buf.len and i < n) {
            self.pos += 1;
            i += 1;
        }
        return self.buf[first..self.pos];
    }

    /// Gets the current character and advances the scanner by one character.
    /// If the scanner is at the end of the string, this function returns `null`
    /// and does not advance the cursor.
    pub fn readOne(self: *Scanner) ?u8 {
        if (self.atEnd()) {
            return null;
        } else {
            const ch = self.buf[self.pos];
            self.pos += 1;
            return ch;
        }
    }

    /// Returns the rest of the input buffer and moves the cursor to the end.
    /// After this function returns, `atEnd()` returns `true`.
    pub fn readToEnd(self: *Scanner) []const u8 {
        const slice = self.buf[self.pos..];
        self.pos = self.buf.len;
        return slice;
    }
};

fn isNewline(ch: u8) bool {
    return ch == '\n';
}

test "Scanner" {
    const text = "hello   world";
    var sc = Scanner.init(text);
    try std.testing.expectEqual(@as(u8, 'h'), sc.readOne().?);
    try std.testing.expectEqual(@as(u8, 'e'), sc.peek().?);
    try std.testing.expectEqual(@as(u8, 'e'), sc.readOne().?);
    try std.testing.expect(sc.lookahead(2) == @as(u8, 'o'));

    const llo = sc.readUntil(std.ascii.isWhitespace);
    try std.testing.expectEqualStrings("llo", llo);

    _ = sc.readWhile(std.ascii.isWhitespace);
    try std.testing.expectEqualStrings("world", sc.readToEnd());
    // nothing left to read
    try std.testing.expectEqualStrings("", sc.read(123));
    try std.testing.expectEqualStrings("", sc.readToEnd());
    try std.testing.expectEqual(true, sc.atEnd());
    try std.testing.expect(sc.lookahead(0) == null);

    const numbers = "123 -456";
    sc = Scanner.init(numbers);
    try std.testing.expectEqual(@as(i32, 123), try sc.readInt(i32));
    sc.skip(1);
    try std.testing.expectEqual(@as(i32, -456), try sc.readInt(i32));
}

test "readLine" {
    const text = "hello\r\nworld\r\rziggy\n\r\n123";
    var sc = Scanner.init(text);
    try std.testing.expectEqualStrings("hello", sc.readLine());
    try std.testing.expectEqualStrings("world\r\rziggy", sc.readLine());
    try std.testing.expectEqualStrings("", sc.readLine());
    try std.testing.expectEqualStrings("123", sc.readLine());
}
