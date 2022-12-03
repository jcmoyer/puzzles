const std = @import("std");
const Allocator = std.mem.Allocator;

const runner = @import("runner.zig");
pub const main = runner.defaultMain;

pub fn solve(ps: *runner.PuzzleSolverState) !void {
    var games = try GameList.loadFromStream(ps.allocator, ps.getPuzzleInputReader());
    try ps.solution(games.totalScore());
    try ps.solution(games.totalScore2());
}

const PlayResult = enum {
    lost,
    draw,
    won,

    fn scoreValue(self: PlayResult) u64 {
        return switch (self) {
            .lost => 0,
            .draw => 3,
            .won => 6,
        };
    }
};

const Play = enum {
    rock,
    paper,
    scissors,

    fn scoreValue(self: Play) u64 {
        return switch (self) {
            .rock => 1,
            .paper => 2,
            .scissors => 3,
        };
    }

    fn beatingPlay(self: Play) Play {
        return switch (self) {
            .rock => .paper,
            .paper => .scissors,
            .scissors => .rock,
        };
    }

    fn losingPlay(self: Play) Play {
        return switch (self) {
            .rock => .scissors,
            .paper => .rock,
            .scissors => .paper,
        };
    }

    fn beats(self: Play, other: Play) bool {
        return self == other.beatingPlay();
    }

    fn getPlayForResult(self: Play, desired: PlayResult) Play {
        return switch (desired) {
            .draw => self,
            .won => self.beatingPlay(),
            .lost => self.losingPlay(),
        };
    }

    /// Returns the result if `a` were played against `b`.
    fn result(a: Play, b: Play) PlayResult {
        if (a == b) {
            return .draw;
        } else if (a.beats(b)) {
            return .won;
        } else {
            return .lost;
        }
    }

    fn parse(ch: u8) !Play {
        return switch (ch) {
            'A', 'X' => .rock,
            'B', 'Y' => .paper,
            'C', 'Z' => .scissors,
            else => error.Unrecognized,
        };
    }
};

const Game = struct {
    elf: Play,
    you: Play,

    fn score(self: Game) u64 {
        const result = self.you.result(self.elf);
        return result.scoreValue() + self.you.scoreValue();
    }

    fn score2(self: Game) u64 {
        const treat_as: PlayResult = switch (self.you) {
            .rock => .lost,
            .paper => .draw,
            .scissors => .won,
        };
        const my_play = self.elf.getPlayForResult(treat_as);
        const result = my_play.result(self.elf);
        return result.scoreValue() + my_play.scoreValue();
    }
};

const GameList = struct {
    games: []Game,

    fn totalScore(self: GameList) u64 {
        var sum: u64 = 0;
        for (self.games) |g| {
            sum += g.score();
        }
        return sum;
    }

    fn totalScore2(self: GameList) u64 {
        var sum: u64 = 0;
        for (self.games) |g| {
            sum += g.score2();
        }
        return sum;
    }

    fn deinit(self: *GameList, allocator: Allocator) void {
        allocator.free(self.games);
    }

    fn loadFromStream(allocator: Allocator, reader: anytype) !GameList {
        var buf: [256]u8 = undefined;

        var games = std.ArrayListUnmanaged(Game){};
        errdefer games.deinit(allocator);

        while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            const stripped_line = std.mem.trimRight(u8, line, "\r\n");
            var iter = std.mem.tokenize(u8, stripped_line, " ");

            var game = try games.addOne(allocator);
            game.elf = try Play.parse(iter.next().?[0]);
            game.you = try Play.parse(iter.next().?[0]);
        }

        return GameList{
            .games = try games.toOwnedSlice(allocator),
        };
    }
};

const example_input =
    \\A Y
    \\B X
    \\C Z
;

test "part 1" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var g = try GameList.loadFromStream(
        allocator,
        reader,
    );
    defer g.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 15), g.totalScore());
}

test "part 2" {
    var allocator = std.testing.allocator;
    var fbs = std.io.fixedBufferStream(example_input);
    var reader = fbs.reader();

    var g = try GameList.loadFromStream(
        allocator,
        reader,
    );
    defer g.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 12), g.totalScore2());
}
