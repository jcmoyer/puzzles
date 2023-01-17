module Supafast

export getinput, Direction, dir2vec, char2dir, matchlines, maplines, parseint

function getprojectroot()
    here = realpath(".")
    last = ""
    while here != last
        if isfile(joinpath(here, "Project.toml"))
            return here
        else
            last = here
            here = realpath(joinpath(here, ".."))
        end
    end
    return nothing
end

function getinput(year::Integer, day::Integer)
    inputname = "$year-$(lpad(day, 2, '0'))-input.txt"
    root = getprojectroot()
    localpath = joinpath(root, "test", inputname)
    # avoid running aoctool since it's relatively expensive
    if ispath(localpath)
        return readchomp(localpath)
    end
    aoctoolpath = joinpath(root, "../scripts/aoctool.py")
    if !ispath(aoctoolpath)
        error("aoctool not found; cwd is: $(pwd())")
    end
    run(`python $aoctoolpath get-input $year $day $localpath`)
    return readchomp(localpath)
end

function getoutputfilename(year::Integer, day::Integer)
    outputname = "$year-$(lpad(day, 2, '0'))-output.txt"
    root = getprojectroot()
    return joinpath(root, "test", outputname)
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
        return [1, 0]
    elseif d == up
        return [0, 1]
    elseif d == left
        return [-1, 0]
    else
        return [0, -1]
    end
end

function matchlines(lines, actions...)
    for line in eachline(IOBuffer(lines))
        for (re, f) in actions
            m = match(re, line)
            if m !== nothing
                f(m.captures...)
            end
        end
    end
end

function maplines(lines, actions...)
    results = Vector()
    for line in eachline(IOBuffer(lines))
        for (re, f) in actions
            m = match(re, line)
            if m !== nothing
                push!(results, f(m.captures...))
            end
        end
    end
    results
end

parseint(str) = parse(Int, str)

end
