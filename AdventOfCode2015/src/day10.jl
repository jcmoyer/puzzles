module Day10

include("sf.jl")
using .Supafast

function looksay(s)
    cur = first(s)
    n = 1
    out = IOBuffer()
    for c in s[2:end]
        if cur == c
            n += 1
        else
            write(out, string(n), Char(cur))
            cur = c
            n = 1
        end
    end
    write(out, string(n), Char(cur))
    String(take!(out))
end

function solve(text)
    for i = 1:40
        text = looksay(text)
    end
    println(length(text))
    for i = 1:10
        text = looksay(text)
    end
    println(length(text))
end

solve(getinput(2015, 10))

end
