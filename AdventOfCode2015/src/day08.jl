module Day08

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
    return p1, p2
end

end
