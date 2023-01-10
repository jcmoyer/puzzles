module Day14

include("sf.jl")
using .Supafast

struct Deer
    speed::Int
    flysec::Int
    restsec::Int
end

# misdirected, thought p2 would ask for 2503 billion iterations :)
function disttime(d::Deer, t::Int)
    totalunit = d.flysec + d.restsec
    wholesteps = t ÷ totalunit
    remainingsec = t - (wholesteps * totalunit)
    wholedist = d.speed * d.flysec * wholesteps
    partdist = d.speed * min(d.flysec, remainingsec)
    wholedist + partdist
end

function solve(text)
    deer = Vector{Deer}()

    for line in eachline(IOBuffer(text))
        m = match(r"\w+ can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.", line)
        push!(deer, Deer(map(x -> parse(Int, x), m.captures)...))
    end

    println(maximum(d -> disttime(d, 2503), deer))

    points = zeros(Int, length(deer))
    for t = 1:2503
        _, awardto = findmax(deer) do d
            disttime(d, t)
        end
        points[awardto] += 1
    end
    println(maximum(points))
end

solve(getinput(2015, 14))

end
