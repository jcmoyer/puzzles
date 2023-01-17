module Day04

# References:
#
# https://en.wikipedia.org/wiki/MD5
# https://www.ietf.org/rfc/rfc1321.txt

const md5_shifts = UInt32[
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
]

const md5_consts = UInt32[
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
]

function md5(text)::UInt128
    buf = Vector{UInt8}(text)

    a0 = 0x67452301
    b0 = 0xefcdab89
    c0 = 0x98badcfe
    d0 = 0x10325476

    push!(buf, 0x80)
    while mod(length(buf), 64) != 56
        push!(buf, 0)
    end

    append!(buf, reinterpret(UInt8, [8 * length(text)]))

    chunkcount = div(length(buf), 64)
    for i = 1:chunkcount
        chunkstart = 64 * (i - 1) + 1
        chunkend = chunkstart + 63
        chunk = buf[chunkstart:chunkend]
        words = reinterpret(UInt32, chunk)

        a = a0
        b = b0
        c = c0
        d = d0
        for j = 0:63
            f = UInt32(0)
            g = UInt32(0)
            if 0 <= j <= 15
                f = (b & c) | ((~b) & d)
                g = j
            elseif 16 <= j <= 31
                f = (d & b) | ((~d) & c)
                g = mod(5j + 1, 16)
            elseif 32 <= j <= 47
                f = b ⊻ c ⊻ d
                g = mod(3j + 5, 16)
            elseif 48 <= j <= 63
                f = c ⊻ (b | (~d))
                g = mod(7j, 16)
            end
            f = f + a + md5_consts[j+1] + words[g+1]
            a = d
            d = c
            c = b
            b = b + bitrotate(f, md5_shifts[j+1])
        end
        a0 += a
        b0 += b
        c0 += c
        d0 += d
    end

    # reinterpret return type can't convert to UInt128 for some reason
    # reinterpret(UInt128, [a0,b0,c0,d0])
    UInt128(a0) | UInt128(b0) << 32 | UInt128(c0) << 64 | UInt128(d0) << 96
end

function solve(text)
    p1, p2 = 0, 0
    i = 0
    while true
        h = md5("$text$i")
        if (h & UInt128(0xf0ffff)) == 0
            p1 = i
            break
        end
        i += 1
    end
    while true
        h = md5("$text$i")
        if (h & UInt128(0xffffff)) == 0
            p2 = i
            break
        end
        i += 1
    end
    return p1, p2
end

end
