module Day18

include("sf.jl")
using .Supafast

function step(mat, frozen=Set())
    new = copy(mat)
    for i in CartesianIndices(mat)
        if i in frozen
            continue
        end

        neighborson = 0
        for x in -1:1
            for y in -1:1
                if x == 0 && y == 0
                    continue
                end
                neighboridx = i + CartesianIndex(x,y)
                neighborson += checkbounds(Bool, mat, neighboridx) && mat[neighboridx]
            end
        end
        if mat[i] == true && !(2 <= neighborson <= 3)
            new[i] = false
        elseif mat[i] == false && neighborson == 3
            new[i] = true
        end
    end
    return new
end

function solve(text::AbstractString)
    lights = falses((100,100))
    r = 1
    for line in eachline(IOBuffer(text))
        for c in eachindex(line)
            if line[c] == '#'
                lights[r,c] = true
            end
        end
        r += 1
    end

    p1lights = copy(lights)
    p2lights = copy(lights)
    p2lights[begin,begin] = true
    p2lights[begin,end] = true
    p2lights[end,begin] = true
    p2lights[end,end] = true
    p2frozen = Set([
        CartesianIndex(1,1),
        CartesianIndex(1,size(p2lights, 2)),
        CartesianIndex(size(p2lights, 1),1),
        CartesianIndex(size(p2lights, 1),size(p2lights, 2)),
    ])

    for _ = 1:100
        p1lights=step(p1lights)
        p2lights=step(p2lights, p2frozen)
    end

    println(count(p1lights))
    println(count(p2lights))
end

solve(getinput(2015, 18))

end
