module Day06

function part1(text)
    mat = falses(1000, 1000)

    for line in eachline(IOBuffer(text))
        m = match(r"turn off (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= false
            continue
        end

        m = match(r"turn on (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= true
            continue
        end

        m = match(r"toggle (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= .!(mat[x0:x1, y0:y1])
            continue
        end
    end

    return count(mat)
end

dec(x) = x > 0 ? x - 1 : x
inc(x) = x + 1

function part2(text)
    mat = zeros(Int, (1000, 1000))

    for line in eachline(IOBuffer(text))
        m = match(r"turn off (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= dec.(mat[x0:x1, y0:y1])
            continue
        end

        m = match(r"turn on (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= inc.(mat[x0:x1, y0:y1])
            continue
        end

        m = match(r"toggle (\d+),(\d+) through (\d+),(\d+)", line)
        if m !== nothing
            x0, y0, x1, y1 = map(x -> parse(Int, x) + 1, m.captures)
            mat[x0:x1, y0:y1] .= (inc ∘ inc).(mat[x0:x1, y0:y1])
            continue
        end
    end

    return sum(mat)
end

solve(text) = part1(text), part2(text)

end
