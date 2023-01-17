module Day10

function looksay(s)
    cur = first(s)
    n = 1
    out = IOBuffer()
    for c in s[2:end]
        if cur == c
            n += 1
        else
            write(out, string(n), Char(cur))
            cur = c
            n = 1
        end
    end
    write(out, string(n), Char(cur))
    String(take!(out))
end

function solve(text)
    p1, p2 = 0, 0
    for i = 1:40
        text = looksay(text)
    end
    p1 = length(text)
    for i = 1:10
        text = looksay(text)
    end
    p2 = length(text)
    return p1, p2
end

end
