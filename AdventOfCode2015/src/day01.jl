module Day01

include("sf.jl")
using .Supafast

function solve(text)
    floor = 0
    p2 = nothing
    for i in eachindex(text)
        c = text[i]
        if c == '('
            floor += 1
        else
            floor -= 1
            if floor == -1 && p2 === nothing
                p2 = i
            end
        end
    end
    println(floor)
    println(p2)
end

solve(getinput(2015, 1))

end
