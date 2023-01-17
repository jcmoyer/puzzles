module Day11

using ..Supafast

function inc!(str)
    i = length(str)
    carry = true
    while carry
        if str[i] + 1 != UInt8('z' + 1)
            str[i] += 1
            carry = false
        else
            str[i] = UInt8('a')
            i -= 1
            if i == 0
                error("string overflow")
            end
        end
    end
end

function check(str)
    hasincreasing = false
    hasbanned = false
    letterpairs = Dict()
    for i in eachindex(str)
        if i <= length(str) - 2
            if str[i] + 1 == str[i+1] && str[i] + 2 == str[i+2]
                hasincreasing = true
            end
        end
        if str[i] == 'i' || str[i] == 'o' || str[i] == 'l'
            hasbanned = true
        end
        if i <= length(str) - 1
            if str[i] == str[i+1]
                r = i:i+1
                p = str[r]
                if p in keys(letterpairs) && length(intersect(letterpairs[p], r)) > 0
                    # ignore, this overlaps with a previous entry for this pair
                else
                    letterpairs[p] = r
                end
            end
        end
    end
    return hasincreasing && !hasbanned && length(letterpairs) >= 2
end

function solve(text)
    p1, p2 = "n/a", "n/a"
    bytes = map(UInt8, collect(text))
    numfound = 0
    while true
        if check(bytes)
            if numfound == 0
                p1 = String(copy(bytes))
            elseif numfound == 1
                p2 = String(copy(bytes))
                break
            end
            numfound += 1
        end
        inc!(bytes)
    end
    return p1, p2
end

end
