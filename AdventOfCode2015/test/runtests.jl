using Test

import AdventOfCode2015: solve

@testset "day01" begin
    @test solve(1) == (280, 1797)

    # part 1 examples
    @test solve(1, "(())") == (0, nothing)
    @test solve(1, "()()") == (0, nothing)
    @test solve(1, "(((") == (3, nothing)
    @test solve(1, "(()(()(") == (3, nothing)
    @test solve(1, "))(((((") == (3, 1)
    @test solve(1, "())") == (-1, 3)
    @test solve(1, "))(") == (-1, 1)
    @test solve(1, ")))") == (-3, 1)
    @test solve(1, ")())())") == (-3, 1)

    # part 2 examples
    @test solve(1, ")") == (-1, 1)
    @test solve(1, "()())") == (-1, 5)
end

@testset "day02" begin
    @test solve(2) == (1606483, 3842356)

    # examples
    @test solve(2, "2x3x4") == (58, 34)
    @test solve(2, "1x1x10") == (43, 14)
end

@testset "day03" begin
    @test solve(3) == (2081, 2341)

    # examples
    @test solve(3, ">") == (2, 2)
    @test solve(3, "^v") == (2, 3)
    @test solve(3, "^>v<") == (4, 3)
    @test solve(3, "^v^v^v^v^v") == (2, 11)
end

@testset "day04" begin
    @test solve(4) == (282749, 9962624)
end

@testset "day05" begin
    @test solve(5) == (238, 69)

    # examples
    @test solve(5, "ugknbfddgicrmopn")[1] == 1
    @test solve(5, "aaa")[1] == 1
    @test solve(5, "jchzalrnumimnmhp")[1] == 0
    @test solve(5, "haegwjzuvuyypxyu")[1] == 0
    @test solve(5, "dvszwmarrgswjxmb")[1] == 0

    @test solve(5, "qjhvhtzxzqqjkmpb")[2] == 1
    @test solve(5, "xxyxx")[2] == 1
    @test solve(5, "uurcxstgmygtbstg")[2] == 0
    @test solve(5, "ieodomkazucvgmuy")[2] == 0
end

@testset "day06" begin
    @test solve(6) == (569999, 17836115)
end

@testset "day07" begin
    @test solve(7) == (16076, 2797)
end

@testset "day08" begin
    @test solve(8) == (1371, 2117)
end

@testset "day09" begin
    @test solve(9) == (251, 898)
end

@testset "day10" begin
    @test solve(10) == (360154, 5103798)
end

@testset "day11" begin
    @test solve(11) == ("cqjxxyzz", "cqkaabcc")
end

@testset "day12" begin
    @test solve(12) == (119433, 68466)
end

@testset "day13" begin
    @test solve(13) == (733, 725)
end

@testset "day14" begin
    @test solve(14) == (2655, 1059)
end

@testset "day15" begin
    @test solve(15) == (13882464, 11171160)
end

@testset "day16" begin
    @test solve(16) == (103, 405)
end

@testset "day17" begin
    @test solve(17) == (654, 57)
end

@testset "day18" begin
    @test solve(18) == (821, 886)
end

@testset "day19" begin
    @test solve(19) == (518, 200)
end

@testset "day20" begin
    @test_broken solve(20) == (831600, 884520)
end

@testset "day21" begin
    @test_broken solve(21) == (91, 158)
end

@testset "day22" begin
    @test solve(22) == (1824, 1937)
end

@testset "day23" begin
    @test solve(23) == (307, 160)
end

@testset "day24" begin
    @test solve(24) == (10439961859, 72050269)
end

@testset "day25" begin
    @test solve(25) == 8997277
end
