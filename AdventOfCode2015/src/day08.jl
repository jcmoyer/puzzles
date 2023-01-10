module Day08

include("sf.jl")
using .Supafast

function solve(text)
    p1 = 0
    p2 = 0
    for line in eachline(IOBuffer(text))
        ast = Meta.parse(line)
        if typeof(ast) != String
            error("not a string; won't eval!")
        end
        p1 += length(line) - length(eval(ast))
        escaped = "\"$(escape_string(line))\""
        p2 += length(escaped) - length(line)
    end
    println(p1)
    println(p2)
end

solve(getinput(2015, 8))

end
