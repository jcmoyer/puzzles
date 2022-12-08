const std = @import("std");
const expectEqual = std.testing.expectEqual;

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

        pub fn toArray(a: Self) [size]ScalarType {
            return .{ a.x, a.y };
        }
    };
}

pub const Vec2i = Vec2(i32);
pub const Vec2us = Vec2(usize);
pub const Vec2f = Vec2(f32);
pub const Vec2d = Vec2(f64);

pub fn manhattan(a: anytype, b: anytype) @TypeOf(a).ScalarType {
    const TypeA = @TypeOf(a);
    const TypeB = @TypeOf(b);
    const ScalarA = TypeA.ScalarType;
    const ScalarB = TypeB.ScalarType;

    if (comptime ScalarA != ScalarB) {
        @compileError("incompatible scalar types");
    }

    if (comptime TypeA.size != TypeB.size) {
        @compileError("incompatible vector sizes");
    }

    if (comptime std.meta.trait.isIntegral(ScalarA) and std.meta.trait.isIntegral(ScalarB)) {
        var sum: ScalarA = 0;
        inline for (@typeInfo(TypeA).Struct.fields) |field| {
            sum += std.math.absInt(@field(a, field.name) - @field(b, field.name)) catch unreachable;
        }
        return sum;
    } else if (comptime std.meta.trait.isFloat(ScalarA) and std.meta.trait.isFloat(ScalarB)) {
        var sum: ScalarA = 0;
        inline for (@typeInfo(TypeA).Struct.fields) |field| {
            sum += std.math.absFloat(@field(a, field.name) - @field(b, field.name));
        }
        return sum;
    } else {
        @compileError("not implemented for this type");
    }
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
