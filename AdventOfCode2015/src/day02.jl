module Day02

include("sf.jl")
using .Supafast

sa(l, w, h) = 2l * w + 2w * h + 2h * l

function slack(l, w, h)
    return min(l * w, w * h, h * l)
end

function ribbon(l, w, h)
    a = min(l, w, h)
    b = (l + w + h) - max(l, w, h) - a
    return 2a + 2b + l * w * h
end

function solve(text)
    p1 = 0
    p2 = 0
    for line in eachline(IOBuffer(text))
        caps = match(r"(\d+)x(\d+)x(\d+)", line).captures
        length, width, height = map(x -> parse(Int, x), caps)
        p1 += sa(length, width, height) + slack(length, width, height)
        p2 += ribbon(length, width, height)
    end
    println(p1)
    println(p2)
end

solve(getinput(2015, 2))

end
