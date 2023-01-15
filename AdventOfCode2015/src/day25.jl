module Day25

include("sf.jl")
using .Supafast

function solve(text::AbstractString)
    row, col = map(parseint, match(r"row (\d+), column (\d+)", text))

    matdim = 2 * max(row, col)
    m = zeros(Int, (matdim, matdim))

    m[1, 1] = 20151125
    prev = (1, 1)

    # only fills the upper left half of the matrix
    for i = 2:matdim
        for j = 1:i
            m[i-j+1, j] = (m[prev...] * 252533) % 33554393
            prev = (i - j + 1, j)
        end
    end

    println(m[row, col])
end

solve(getinput(2015, 25))

end
