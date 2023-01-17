module Day19

using ..Supafast

import DataStructures: PriorityQueue, enqueue!, dequeue!

struct Substitution
    find::Vector{UInt8}
    replace::Vector{UInt8}
end

function replacements(substitutions::Vector{Substitution}, buf)
    distinct = Set()
    for s in substitutions
        positions = findall(s.find, buf)
        for p in positions
            mc = copy(buf)
            splice!(mc, p, s.replace)
            # println(String(copy(mc)))
            push!(distinct, mc)
        end
    end
    return distinct
end

function distlength(a, b)
    return abs(length(a) - length(b))
end

function subcount(substitutions, start, target)
    seen = Dict{Vector{UInt8},UInt16}()
    open = PriorityQueue{Tuple{Vector{UInt8},Int},Int}()

    enqueue!(open, (start, 0), distlength(start, target))

    while length(open) > 0
        next, steps = dequeue!(open)

        if next == target
            # TODO: is there only one reduction? it seems like there should be
            # more but this yields a correct answer
            return steps
        end

        seen[next] = steps

        for n in replacements(substitutions, next)
            # early discard
            if n in keys(seen) && seen[n] <= (steps + 1)
                continue
            end

            t = (n, steps + 1)
            enqueue!(open, t, distlength(n, target))
        end
    end

    return nothing
end

function solve(text::AbstractString)
    forward = Vector{Substitution}()
    backward = Vector{Substitution}()
    molecule = nothing

    for line in eachline(IOBuffer(text))
        m = match(r"(\w+) => (\w+)", line)
        if m !== nothing
            search, replace = m.captures
            searchvec = Vector{UInt8}(search)
            replacevec = Vector{UInt8}(replace)
            push!(forward, Substitution(searchvec, replacevec))
            push!(backward, Substitution(replacevec, searchvec))
        else
            molecule = Vector{UInt8}(line)
        end
    end

    return length(replacements(forward, molecule)), subcount(backward, molecule, UInt8['e'])
end

end
