const std = @import("std");
const expectEqual = std.testing.expectEqual;

const sr = @import("sr.zig");

pub fn Vec2(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const ScalarType = T;
        pub const size = 2;

        x: T = 0,
        y: T = 0,

        pub fn add(a: Self, b: Self) Self {
            return .{ .x = a.x + b.x, .y = a.y + b.y };
        }

        pub fn addWrap(a: Self, b: Self) Self {
            return .{ .x = a.x +% b.x, .y = a.y +% b.y };
        }

        pub fn sub(a: Self, b: Self) Self {
            return .{ .x = a.x - b.x, .y = a.y - b.y };
        }

        pub fn subWrap(a: Self, b: Self) Self {
            return .{ .x = a.x -% b.x, .y = a.y -% b.y };
        }

        pub fn mul(a: Self, s: T) Self {
            return .{ .x = a.x * s, .y = a.y * s };
        }

        /// Rounds towards zero.
        pub fn divTrunc(a: Self, s: T) Self {
            return .{ .x = @divTrunc(a.x, s), .y = @divTrunc(a.y, s) };
        }

        pub fn clamp(a: Self, min: T, max: T) Self {
            return .{
                .x = std.math.clamp(a.x, min, max),
                .y = std.math.clamp(a.y, min, max),
            };
        }

        pub fn toArray(a: Self) [size]ScalarType {
            return .{ a.x, a.y };
        }

        pub fn cast(self: Self, comptime VectorT: type) VectorT {
            return vectorCast(VectorT, self);
        }

        pub fn manhattan(a: Self, b: Self) !ScalarType {
            return vectorManhattan(a, b);
        }

        pub fn euclidean(a: Self, b: Self) ScalarType {
            return vectorEuclidean(a, b);
        }

        pub fn offsetWrap(self: Self, dir: sr.Direction) Self {
            return switch (dir) {
                .west => .{ .x = self.x -% 1, .y = self.y },
                .east => .{ .x = self.x +% 1, .y = self.y },
                .north => .{ .x = self.x, .y = self.y +% 1 },
                .south => .{ .x = self.x, .y = self.y -% 1 },
            };
        }
    };
}

pub const Vec2i = Vec2(i32);
pub const Vec2f = Vec2(f32);
pub const Vec2d = Vec2(f64);
pub const Vec2us = Vec2(usize);
pub const Vec2is = Vec2(isize);

pub fn vectorManhattan(a: anytype, b: anytype) @TypeOf(a).ScalarType {
    const TypeA = @TypeOf(a);
    const TypeB = @TypeOf(b);
    const ScalarA = TypeA.ScalarType;
    const ScalarB = TypeB.ScalarType;

    if (ScalarA != ScalarB) {
        @compileError("incompatible scalar types");
    }

    if (TypeA.size != TypeB.size) {
        @compileError("incompatible vector sizes");
    }

    if (comptime std.meta.trait.isIntegral(ScalarA)) {
        var sum: ScalarA = 0;
        inline for (@typeInfo(TypeA).Struct.fields) |field| {
            sum += std.math.absInt(@field(a, field.name) - @field(b, field.name)) catch unreachable;
        }
        return sum;
    } else if (comptime std.meta.trait.isFloat(ScalarA)) {
        var sum: ScalarA = 0;
        inline for (@typeInfo(TypeA).Struct.fields) |field| {
            sum += std.math.fabs(@field(a, field.name) - @field(b, field.name));
        }
        return sum;
    } else {
        @compileError("not implemented for this type");
    }
}

pub fn vectorEuclidean(a: anytype, b: anytype) @TypeOf(a).ScalarType {
    const TypeA = @TypeOf(a);
    const TypeB = @TypeOf(b);
    const ScalarA = TypeA.ScalarType;
    const ScalarB = TypeB.ScalarType;

    if (ScalarA != ScalarB) {
        @compileError("incompatible scalar types");
    }

    if (TypeA.size != TypeB.size) {
        @compileError("incompatible vector sizes");
    }

    var sum: ScalarA = 0;
    if (comptime std.meta.trait.isFloat(ScalarA)) {
        inline for (@typeInfo(TypeA).Struct.fields) |field| {
            const d = @floatCast(ScalarA, @field(a, field.name) - @field(b, field.name));
            sum += d * d;
        }
    } else {
        @compileError("vectorEuclidean not implemented for " ++ @typeName(TypeA));
    }
    return std.math.sqrt(sum);
}

pub fn vectorCast(comptime DestType: type, v: anytype) DestType {
    const SourceType = @TypeOf(v);

    if (DestType.size != SourceType.size) {
        @compileError("vector sizes must match");
    }

    if (comptime (std.meta.trait.isIntegral(DestType.ScalarType) and std.meta.trait.isIntegral(SourceType.ScalarType))) {
        var result: DestType = undefined;
        inline for (std.meta.fields(SourceType)) |field| {
            @field(result, field.name) = @intCast(DestType.ScalarType, @field(v, field.name));
        }
        return result;
    } else if (comptime (std.meta.trait.isFloat(DestType.ScalarType) and std.meta.trait.isIntegral(SourceType.ScalarType))) {
        var result: DestType = undefined;
        inline for (std.meta.fields(SourceType)) |field| {
            @field(result, field.name) = @intToFloat(DestType.ScalarType, @field(v, field.name));
        }
        return result;
    } else if (comptime (std.meta.trait.isIntegral(DestType.ScalarType) and std.meta.trait.isFloat(SourceType.ScalarType))) {
        var result: DestType = undefined;
        inline for (std.meta.fields(SourceType)) |field| {
            @field(result, field.name) = @floatToInt(DestType.ScalarType, @field(v, field.name));
        }
        return result;
    } else if (comptime (std.meta.trait.isFloat(DestType.ScalarType) and std.meta.trait.isFloat(SourceType.ScalarType))) {
        var result: DestType = undefined;
        inline for (std.meta.fields(SourceType)) |field| {
            @field(result, field.name) = @floatCast(DestType.ScalarType, @field(v, field.name));
        }
        return result;
    }

    @compileError("not implemented");
}

test "vectorCast" {
    var a = Vec2us{ .x = 4, .y = 8 };
    var b = vectorCast(Vec2i, a);
    try expectEqual(@as(i32, 4), b.x);
    try expectEqual(@as(i32, 8), b.y);

    var c = vectorCast(Vec2f, b);
    try std.testing.expectApproxEqAbs(@as(f32, 4), c.x, 0.001);
    try std.testing.expectApproxEqAbs(@as(f32, 8), c.y, 0.001);

    var d = vectorCast(Vec2us, c);
    try expectEqual(@as(usize, 4), d.x);
    try expectEqual(@as(usize, 8), d.y);

    var e = vectorCast(Vec2d, c);
    try std.testing.expectApproxEqAbs(@as(f64, 4), e.x, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 8), e.y, 0.001);
}
