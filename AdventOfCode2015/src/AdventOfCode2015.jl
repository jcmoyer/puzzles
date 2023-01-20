module AdventOfCode2015

year = match(r"(\d+)", String(nameof((@__MODULE__)))).captures |> only |> (x -> parse(Int, x))

include("sf.jl")

for i = 1:25
    include("day$(lpad("$i", 2, '0')).jl")
end

function solve(day::Int, input::Union{AbstractString,Nothing}=nothing)
    if input === nothing
        input = Supafast.getinput(year, day)
    end
    sym = Symbol("Day", lpad("$day", 2, '0'))
    eval(:($sym)).solve(input)
end

end
