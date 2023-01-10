module Supafast

export getinput, Direction, dir2vec, char2dir

function getinput(year::Integer, day::Integer)
    localpath = "test/$year-$(lpad(day, 2, '0'))-input.txt"
    if !ispath("../scripts/aoctool.py")
        error("aoctool not found; cwd is: $(pwd())")
    end
    if !ispath(localpath)
        run(`python ../scripts/aoctool.py get-input $year $day $localpath`)
    end
    return readchomp(localpath)
end

@enum Direction right up left down

function char2dir(c::AbstractChar)
    if c == '>'
        return right
    elseif c == '^'
        return up
    elseif c == '<'
        return left
    elseif c == 'v'
        return down
    end
end

function dir2vec(d::Direction)
    if d == right
        return [1,0]
    elseif d == up
        return [0,1]
    elseif d == left
        return [-1,0]
    else
        return [0,-1]
    end
end

end
