const Vec2i = @import("vector.zig").Vec2i;

pub const Turn = enum { left, right };

pub const Direction = enum {
    north,
    east,
    south,
    west,

    pub fn opposite(self: Direction) Direction {
        return switch (self) {
            .north => .south,
            .east => .west,
            .south => .north,
            .west => .east,
        };
    }

    pub fn isVertical(self: Direction) bool {
        return self == .north or self == .south;
    }

    pub fn isHorizontal(self: Direction) bool {
        return self == .east or self == .west;
    }

    pub fn rotate(self: Direction, turn: Turn) Direction {
        if (turn == .left) {
            return self.rotateCCW();
        } else if (turn == .right) {
            return self.rotateCW();
        } else {
            unreachable;
        }
    }

    pub fn rotateCW(self: Direction) Direction {
        return switch (self) {
            .north => .east,
            .east => .south,
            .south => .west,
            .west => .north,
        };
    }

    pub fn rotateCCW(self: Direction) Direction {
        return switch (self) {
            .north => .west,
            .east => .north,
            .south => .east,
            .west => .south,
        };
    }

    pub fn toVector(self: Direction) Vec2i {
        return switch (self) {
            .north => .{ .x = 0, .y = 1 },
            .east => .{ .x = 1, .y = 0 },
            .south => .{ .x = 0, .y = -1 },
            .west => .{ .x = -1, .y = 0 },
        };
    }

    pub fn toVectorYDown(self: Direction) Vec2i {
        return switch (self) {
            .north => .{ .x = 0, .y = -1 },
            .east => .{ .x = 1, .y = 0 },
            .south => .{ .x = 0, .y = 1 },
            .west => .{ .x = -1, .y = 0 },
        };
    }

    pub fn parseLRUD(ch: u8) !Direction {
        return switch (ch) {
            'L',
            'l',
            => .west,
            'R',
            'r',
            => .east,
            'U', 'u' => .north,
            'D', 'd' => .south,
            else => error.InvalidCharacter,
        };
    }
};
