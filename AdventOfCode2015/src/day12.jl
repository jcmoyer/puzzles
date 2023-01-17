module Day12

using ..Supafast

import JSON

function sumallints(text::AbstractString)
    result = 0
    for r in findall(r"\-?(\d+)", text)
        result += parse(Int, text[r])
    end
    return result
end

function sumints(::Any)
    return 0
end

function sumints(val::Integer)
    return val
end

function sumints(vec::Vector)
    return sum(sumints.(vec))
end

function sumints(obj::Dict)
    result = 0
    for v in values(obj)
        if v isa String
            if v == "red"
                return 0
            end
        else
            result += sumints(v)
        end
    end
    return result
end

function solve(text)
    root = JSON.parse(text)
    return sumallints(text), sumints(root)
end

end
