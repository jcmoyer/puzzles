module Day15

using ..Supafast

import LinearAlgebra: dot

function score(ingreds::Matrix, distribution)
    prod(max.(0, ingreds[1:4, :] * distribution))
end

function scorecal(ingreds::Matrix, distribution, cals::Integer)
    if dot(ingreds[5, :], distribution) == cals
        return score(ingreds, distribution)
    else
        return 0
    end
end

function solve(text::AbstractString)
    ingreds = Vector{Int}[]

    for line in eachline(IOBuffer(text))
        m = match(r"\w+: capacity (\-?\d+), durability (\-?\d+), flavor (\-?\d+), texture (\-?\d+), calories (\-?\d+)", line)
        v = map(x -> parse(Int, x), m.captures)
        push!(ingreds, v)
    end

    ingredmat = stack(ingreds)

    p1 = maximum(Iterators.product(1:100, 1:100, 1:100, 1:100)) do t
        sum(t) == 100 ? score(ingredmat, Int[t...]) : 0
    end
    p2 = maximum(Iterators.product(1:100, 1:100, 1:100, 1:100)) do t
        sum(t) == 100 ? scorecal(ingredmat, Int[t...], 500) : 0
    end

    return p1, p2
end

end
