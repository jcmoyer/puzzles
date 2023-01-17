module Day13

using ..Supafast

# reskinned day 9, only difference is you add the distance to the first node in
# the path when you reach the end v( '_')v
function visitrec(f::Function, start, distmap, first, seen=Set(), d=0)
    push!(seen, start)
    if length(seen) == size(distmap, 1)
        return d + distmap[start, first]
    end
    adjd = Int[]
    for i = axes(distmap, 1)
        if i in seen
            continue
        end
        nd = visitrec(f, i, distmap, first, copy(seen), distmap[start, i])
        push!(adjd, nd)
    end
    adjd .+= d
    if length(adjd) > 0
        return f(adjd)
    else
        return d
    end
end

function solve(text)
    dist = Dict()
    names = Set()
    for line in eachline(IOBuffer(text))
        a, gainlose, units, b = match(r"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.", line)

        dhappy = parse(Int, units)
        if gainlose == "lose"
            dhappy = -dhappy
        end

        dist[(a, b)] = dhappy
        push!(names, a)
        push!(names, b)
    end

    nameid = Dict(zip(names, 1:length(names)))

    distmat = zeros(Int, (1 + length(names), 1 + length(names)))
    for a in names
        for b in names
            if a == b
                continue
            end
            distmat[nameid[a], nameid[b]] = dist[(a, b)] + dist[(b, a)]
        end
    end

    return visitrec(maximum, 1, distmat[1:end-1, 1:end-1], 1), visitrec(maximum, 1, distmat, 1)
end

end
