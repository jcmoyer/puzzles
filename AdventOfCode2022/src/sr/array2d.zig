const std = @import("std");
const Allocator = std.mem.Allocator;

const Vec2 = @import("vector.zig").Vec2;

pub fn Array2D(comptime T: type) type {
    return struct {
        const Self = @This();

        data: []T = &[_]T{},
        width: usize = 0,
        height: usize = 0,

        pub fn deinit(self: *Self, allocator: Allocator) void {
            allocator.free(self.data);
        }

        pub fn fill(self: *Self, value: T) void {
            std.mem.set(T, self.data, value);
        }

        pub fn clone(self: Self, allocator: Allocator) !Self {
            var new_data = try allocator.alloc(T, self.data.len);
            std.mem.copy(T, new_data, self.data);
            return Self{
                .data = new_data,
                .width = self.width,
                .height = self.height,
            };
        }

        pub fn resize(self: *Self, allocator: Allocator, width: usize, height: usize) !void {
            self.deinit(allocator);
            self.width = width;
            self.height = height;
            self.data = try allocator.alloc(T, width * height);
        }

        pub fn at(self: *Self, x: usize, y: usize) *T {
            return &self.data[y * self.width + x];
        }

        pub fn atConst(self: *const Self, x: usize, y: usize) *const T {
            return &self.data[y * self.width + x];
        }

        pub fn inBounds(self: Self, x: usize, y: usize) bool {
            return x < self.width and y < self.height;
        }

        pub fn atVec(self: *Self, v: Vec2(usize)) *T {
            return self.at(v.x, v.y);
        }

        pub fn atVecConst(self: *const Self, v: Vec2(usize)) *const T {
            return self.atConst(v.x, v.y);
        }

        pub fn inBoundsVec(self: Self, v: Vec2(usize)) bool {
            return self.inBounds(v.x, v.y);
        }

        const RowIterator = struct {
            parent: *Self,
            row: usize,
            col: usize,

            pub fn next(self: *RowIterator) ?*T {
                if (self.col < self.parent.width) {
                    var val = self.parent.at(self.col, self.row);
                    self.col += 1;
                    return val;
                } else {
                    return null;
                }
            }
        };

        pub fn iterateRow(self: *Self, row: usize) RowIterator {
            return RowIterator{ .parent = self, .row = row, .col = 0 };
        }

        const ColumnIterator = struct {
            parent: *Self,
            row: usize,
            col: usize,

            pub fn next(self: *ColumnIterator) ?*T {
                if (self.row < self.parent.height) {
                    var val = self.parent.at(self.col, self.row);
                    self.row += 1;
                    return val;
                } else {
                    return null;
                }
            }
        };

        pub fn iterateColumn(self: *Self, col: usize) ColumnIterator {
            return ColumnIterator{ .parent = self, .row = 0, .col = col };
        }

        pub fn rotateRow(self: *Self, row: usize, n: usize) void {
            var rotate_by = n % self.width;
            if (rotate_by == 0) {
                return;
            }

            var first: usize = 0;
            var last: usize = self.width - 1;

            while (first < last) {
                std.mem.swap(T, self.at(first, row), self.at(last, row));
                first += 1;
                last -= 1;
            }

            first = 0;
            last = rotate_by - 1;

            while (first < last) {
                std.mem.swap(T, self.at(first, row), self.at(last, row));
                first += 1;
                last -= 1;
            }

            first = rotate_by;
            last = self.width - 1;

            while (first < last) {
                std.mem.swap(T, self.at(first, row), self.at(last, row));
                first += 1;
                last -= 1;
            }
        }

        /// Faster version of `rotateColumn`. Uses a stack-allocated buffer to store the `n` elements that will be shifted off the end.
        /// Requires `bufsz >= n`.
        pub fn rotateColumnNoSwap(self: *Self, col: usize, n: usize, comptime bufsz: usize) void {
            const rotate_n = n % self.height;
            if (rotate_n == 0) {
                return;
            }
            var buffer: [bufsz]T = undefined;

            // copy n-elements from end of column into buffer (i = src, j = dst)
            var i: usize = self.height - rotate_n;
            var j: usize = 0;
            while (i < self.height) {
                buffer[j] = self.at(col, i).*;
                i += 1;
                j += 1;
            }

            // shift the front elements towards the end n-spaces by copying backwards
            i = self.height - 1 - rotate_n;
            j = self.height - 1;
            const copy_len: usize = self.height - rotate_n;
            var copy_i: usize = 0;
            while (copy_i < copy_len) : (copy_i += 1) {
                self.at(col, j).* = self.at(col, i).*;
                // these will overflow on the last iteration but we won't need them
                {
                    @setRuntimeSafety(false);
                    i -= 1;
                    j -= 1;
                }
            }

            // insert buffer at front
            i = 0;
            j = 0;
            while (i < rotate_n) {
                self.at(col, j).* = buffer[i];
                i += 1;
                j += 1;
            }
        }

        pub fn rotateColumn(self: *Self, col: usize, n: usize) void {
            var rotate_by = n % self.height;
            if (rotate_by == 0) {
                return;
            }

            var first: usize = 0;
            var last: usize = self.height - 1;

            while (first < last) {
                std.mem.swap(T, self.at(col, first), self.at(col, last));
                first += 1;
                last -= 1;
            }

            first = 0;
            last = rotate_by - 1;

            while (first < last) {
                std.mem.swap(T, self.at(col, first), self.at(col, last));
                first += 1;
                last -= 1;
            }

            first = rotate_by;
            last = self.height - 1;

            while (first < last) {
                std.mem.swap(T, self.at(col, first), self.at(col, last));
                first += 1;
                last -= 1;
            }
        }
    };
}

test "clone" {
    var a = Array2D(u8){};
    var b = Array2D(u8){};

    try a.resize(std.testing.allocator, 3, 2);
    defer a.deinit(std.testing.allocator);
    a.fill('a');
    a.at(2, 1).* = 'b';

    b = try a.clone(std.testing.allocator);
    defer b.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(u8, a.data, b.data);
    try std.testing.expectEqual(a.width, b.width);
    try std.testing.expectEqual(a.height, b.height);
}
