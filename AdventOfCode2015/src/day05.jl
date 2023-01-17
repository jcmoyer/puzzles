module Day05

function nice(str)
    vowels = 0
    anydoubled = false

    for i in eachindex(str)
        c = str[i]

        if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
            vowels += 1
        end

        if i > 1
            pair = str[i-1:i]
            if pair == "ab" || pair == "cd" || pair == "pq" || pair == "xy"
                return false
            elseif pair[1] == pair[2]
                anydoubled = true
            end
        end
    end

    return vowels >= 3 && anydoubled
end

function nice2(str)
    pairs = Dict()
    nicepair = false
    nicetrip = false

    for i in eachindex(str)
        if i > 1
            pair = str[i-1:i]
            if pair in keys(pairs)
                if (i - 1) > (pairs[pair] + 1)
                    nicepair = true
                end
            else
                pairs[pair] = i - 1
            end
        end

        if i > 2
            trip = str[i-2:i]
            if trip[1] == trip[3]
                nicetrip = true
            end
        end
    end

    return nicepair && nicetrip
end

function solve(text)
    mapreduce(nice, +, eachline(IOBuffer(text))), mapreduce(nice2, +, eachline(IOBuffer(text)))
end

end
