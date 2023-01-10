module Day09

include("sf.jl")
using .Supafast

function visitrec(f::Function, start, distmap, seen=Set(), d=0)
    push!(seen, start)
    adjd = Int[]
    for i = axes(distmap, 1)
        if i in seen
            continue
        end
        nd = visitrec(f, i, distmap, copy(seen), distmap[start, i])
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
    routes = Dict()
    locations = Set{String}()

    for line in eachline(IOBuffer(text))
        from, to, diststr = match(r"(\w+) to (\w+) = (\d+)", line)
        dist = parse(Int, diststr)
        routes[(from, to)] = dist
        routes[(to, from)] = dist
        push!(locations, from, to)
    end

    nloc = length(locations)
    indices = Dict(zip(locations, 1:nloc))
    distmat = zeros(Int, (nloc, nloc))
    for ((from, to), dist) in routes
        distmat[indices[from], indices[to]] = dist
    end
    println(minimum([visitrec(minimum, i, distmat) for i in 1:nloc]))
    println(maximum([visitrec(maximum, i, distmat) for i in 1:nloc]))
end

solve(getinput(2015, 9))

end
