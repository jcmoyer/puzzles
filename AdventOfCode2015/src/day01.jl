module Day01

include("sf.jl")
using .Supafast

function solve(text)
    ints = map(ch -> ch == '(' ? 1 : -1, collect(text))
    println(sum(ints))
    println(findfirst(x -> x == -1, accumulate(+, ints)))
end

solve(getinput(2015, 1))

end
