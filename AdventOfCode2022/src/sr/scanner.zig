const std = @import("std");

pub const Scanner = struct {
    buf: []const u8,
    pos: usize = 0,

    pub fn init(buf: []const u8) Scanner {
        return Scanner{
            .buf = buf,
        };
    }

    pub fn readWhile(self: *Scanner, cond: fn (u8) bool) []const u8 {
        const first = self.pos;
        while (self.pos < self.buf.len and cond(self.buf[self.pos])) : (self.pos += 1) {}
        return self.buf[first..self.pos];
    }

    pub fn readUntil(self: *Scanner, cond: fn (u8) bool) []const u8 {
        const first = self.pos;
        while (self.pos < self.buf.len and !cond(self.buf[self.pos])) : (self.pos += 1) {}
        return self.buf[first..self.pos];
    }

    pub fn moveToNext(self: *Scanner, cond: fn (u8) bool) bool {
        while (self.pos < self.buf.len and !cond(self.buf[self.pos])) : (self.pos += 1) {}
        // either we found a char for which cond passed OR we reached the end
        // so there's no need to re-evaluate cond
        return self.pos < self.buf.len;
    }

    pub fn atEnd(self: *const Scanner) bool {
        return self.pos == self.buf.len;
    }

    pub fn readInt(self: *Scanner, comptime T: type) !T {
        const str = self.readWhile(std.ascii.isDigit);
        return std.fmt.parseInt(T, str, 10);
    }

    pub fn readLine(self: *Scanner) []const u8 {
        const slice = self.readUntil(isNewline);
        // consume [\r]\n
        if (!self.atEnd()) {
            if (self.buf[self.pos] == '\r') {
                self.pos += 1;
            }
            if (!self.atEnd() and self.buf[self.pos] == '\n') {
                self.pos += 1;
            }
        }
        return slice;
    }

    pub fn current(self: *Scanner) u8 {
        return self.buf[self.pos];
    }

    pub fn peek(self: *Scanner) ?u8 {
        if (self.atEnd()) {
            return null;
        } else {
            return self.buf[self.pos + 1];
        }
    }

    // Reads up to `n` characters from the input and returns a slice containing the characters read.
    pub fn read(self: *Scanner, n: usize) []const u8 {
        const first = self.pos;
        var i: usize = 0;
        while (self.pos < self.buf.len and i < n) {
            self.pos += 1;
            i += 1;
        }
        return self.buf[first..self.pos];
    }

    pub fn readOne(self: *Scanner) ?u8 {
        if (self.atEnd()) {
            return null;
        } else {
            self.pos += 1;
            if (self.atEnd()) {
                return null;
            } else {
                return self.buf[self.pos];
            }
        }
    }

    pub fn readToEnd(self: *Scanner) []const u8 {
        const slice = self.buf[self.pos..];
        self.pos = self.buf.len;
        return slice;
    }
};

fn isNewline(ch: u8) bool {
    return ch == '\r' or ch == '\n';
}
