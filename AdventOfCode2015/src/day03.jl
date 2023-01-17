module Day03

using ..Supafast

function visit(dirs)
    pos = [0, 0]
    visited = Set((pos,))
    for d in dirs
        pos = pos + d
        push!(visited, pos)
    end
    return visited
end

function solve(text)
    dirs = (dir2vec ∘ char2dir).(collect(text))
    return length(visit(dirs)), length(union(visit(dirs[begin:2:end]), visit(dirs[begin+1:2:end])))
end

end
