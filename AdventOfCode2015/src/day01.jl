module Day01

function solve(text)
    ints = map(ch -> ch == '(' ? 1 : -1, collect(text))
    return sum(ints), findfirst(x -> x == -1, accumulate(+, ints))
end

end
