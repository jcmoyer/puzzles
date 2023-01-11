module Day17

include("sf.jl")
using .Supafast

function countsumto(sumto, containers, used, completesets, runningsum=0)
    if runningsum == sumto
        push!(completesets, used)
        return
    elseif runningsum > sumto
        return
    end
    for i in eachindex(containers)
        if i in used
            continue
        else
            countsumto(
                sumto,
                containers,
                push!(copy(used), i),
                completesets,
                runningsum + containers[i]
            )
        end
    end
end

function countsumto(sumto, containers)
    completesets=Set()
    for i in eachindex(containers)
        used = Set(i)
        sumhere = containers[i]
        countsumto(sumto, containers, used, completesets, sumhere)
    end
    return completesets
end

function solve(text::AbstractString)
    containers = Int[]
    for line in eachline(IOBuffer(text))
        push!(containers,parse(Int, line))
    end

    sets = countsumto(150, containers)
    println(length(sets))

    minlen = minimum(length, sets)
    println(count(s -> length(s) == minlen, sets))
end

solve(getinput(2015, 17))

end
