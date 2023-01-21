module Day17

using ..Supafast

import Combinatorics: combinations

function solve(text::AbstractString)
    containers = maplines(text, r"(\d+)" => parseint)
    waysbylen = zeros(Int, length(containers))
    ways = 0
    for c in combinations(containers)
        if sum(c) == 150
            waysbylen[length(c)] += 1
            ways += 1
        end
    end
    return ways, first(waysbylen[waysbylen.>0])
end

end
