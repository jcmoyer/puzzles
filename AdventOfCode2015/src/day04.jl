module Day04

# Reference: https://www.ietf.org/rfc/rfc1321.txt

const md5_s11 = 7
const md5_s12 = 12
const md5_s13 = 17
const md5_s14 = 22

const md5_s21 = 5
const md5_s22 = 9
const md5_s23 = 14
const md5_s24 = 20

const md5_s31 = 4
const md5_s32 = 11
const md5_s33 = 16
const md5_s34 = 23

const md5_s41 = 6
const md5_s42 = 10
const md5_s43 = 15
const md5_s44 = 21

const md5_padding = UInt8[
    0x80, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0
]

mutable struct MD5Context
    a::UInt32
    b::UInt32
    c::UInt32
    d::UInt32
    bitcount::UInt64
    buffer::Vector{UInt8}
    iob::IOBuffer
    function MD5Context()
        storage = Vector{UInt8}(undef, 64)
        iobuf = IOBuffer(storage; write=true, truncate=false)
        new(0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0, storage, iobuf)
    end
end

@inline md5f(x, y, z) = (x & y) | ((~x) & z)
@inline md5g(x, y, z) = (x & z) | (y & (~z))
@inline md5h(x, y, z) = x ⊻ y ⊻ z
@inline md5i(x, y, z) = y ⊻ (x | (~z))

@inline md5mix(mixer::Function, a, b, c, d, x, s, ac) = bitrotate(a + mixer(b, c, d) + x + ac, s) + b

@inline md5ff(a, b, c, d, x, s, ac) = md5mix(md5f, a, b, c, d, x, s, ac)
@inline md5gg(a, b, c, d, x, s, ac) = md5mix(md5g, a, b, c, d, x, s, ac)
@inline md5hh(a, b, c, d, x, s, ac) = md5mix(md5h, a, b, c, d, x, s, ac)
@inline md5ii(a, b, c, d, x, s, ac) = md5mix(md5i, a, b, c, d, x, s, ac)

@inline function transform!(ctx::MD5Context)
    a = ctx.a
    b = ctx.b
    c = ctx.c
    d = ctx.d
    x = reinterpret(UInt32, view(ctx.buffer, :))

    @inbounds begin
        # Round 1
        a = md5ff(a, b, c, d, x[1], md5_s11, 0xd76aa478) # 1
        d = md5ff(d, a, b, c, x[2], md5_s12, 0xe8c7b756) # 2
        c = md5ff(c, d, a, b, x[3], md5_s13, 0x242070db) # 3
        b = md5ff(b, c, d, a, x[4], md5_s14, 0xc1bdceee) # 4
        a = md5ff(a, b, c, d, x[5], md5_s11, 0xf57c0faf) # 5
        d = md5ff(d, a, b, c, x[6], md5_s12, 0x4787c62a) # 6
        c = md5ff(c, d, a, b, x[7], md5_s13, 0xa8304613) # 7
        b = md5ff(b, c, d, a, x[8], md5_s14, 0xfd469501) # 8
        a = md5ff(a, b, c, d, x[9], md5_s11, 0x698098d8) # 9
        d = md5ff(d, a, b, c, x[10], md5_s12, 0x8b44f7af) # 10
        c = md5ff(c, d, a, b, x[11], md5_s13, 0xffff5bb1) # 11
        b = md5ff(b, c, d, a, x[12], md5_s14, 0x895cd7be) # 12
        a = md5ff(a, b, c, d, x[13], md5_s11, 0x6b901122) # 13
        d = md5ff(d, a, b, c, x[14], md5_s12, 0xfd987193) # 14
        c = md5ff(c, d, a, b, x[15], md5_s13, 0xa679438e) # 15
        b = md5ff(b, c, d, a, x[16], md5_s14, 0x49b40821) # 16

        # Round 2
        a = md5gg(a, b, c, d, x[2], md5_s21, 0xf61e2562) # 17
        d = md5gg(d, a, b, c, x[7], md5_s22, 0xc040b340) # 18
        c = md5gg(c, d, a, b, x[12], md5_s23, 0x265e5a51) # 19
        b = md5gg(b, c, d, a, x[1], md5_s24, 0xe9b6c7aa) # 20
        a = md5gg(a, b, c, d, x[6], md5_s21, 0xd62f105d) # 21
        d = md5gg(d, a, b, c, x[11], md5_s22, 0x2441453) # 22
        c = md5gg(c, d, a, b, x[16], md5_s23, 0xd8a1e681) # 23
        b = md5gg(b, c, d, a, x[5], md5_s24, 0xe7d3fbc8) # 24
        a = md5gg(a, b, c, d, x[10], md5_s21, 0x21e1cde6) # 25
        d = md5gg(d, a, b, c, x[15], md5_s22, 0xc33707d6) # 26
        c = md5gg(c, d, a, b, x[4], md5_s23, 0xf4d50d87) # 27
        b = md5gg(b, c, d, a, x[9], md5_s24, 0x455a14ed) # 28
        a = md5gg(a, b, c, d, x[14], md5_s21, 0xa9e3e905) # 29
        d = md5gg(d, a, b, c, x[3], md5_s22, 0xfcefa3f8) # 30
        c = md5gg(c, d, a, b, x[8], md5_s23, 0x676f02d9) # 31
        b = md5gg(b, c, d, a, x[13], md5_s24, 0x8d2a4c8a) # 32

        # Round 3
        a = md5hh(a, b, c, d, x[6], md5_s31, 0xfffa3942) # 33
        d = md5hh(d, a, b, c, x[9], md5_s32, 0x8771f681) # 34
        c = md5hh(c, d, a, b, x[12], md5_s33, 0x6d9d6122) # 35
        b = md5hh(b, c, d, a, x[15], md5_s34, 0xfde5380c) # 36
        a = md5hh(a, b, c, d, x[2], md5_s31, 0xa4beea44) # 37
        d = md5hh(d, a, b, c, x[5], md5_s32, 0x4bdecfa9) # 38
        c = md5hh(c, d, a, b, x[8], md5_s33, 0xf6bb4b60) # 39
        b = md5hh(b, c, d, a, x[11], md5_s34, 0xbebfbc70) # 40
        a = md5hh(a, b, c, d, x[14], md5_s31, 0x289b7ec6) # 41
        d = md5hh(d, a, b, c, x[1], md5_s32, 0xeaa127fa) # 42
        c = md5hh(c, d, a, b, x[4], md5_s33, 0xd4ef3085) # 43
        b = md5hh(b, c, d, a, x[7], md5_s34, 0x4881d05) # 44
        a = md5hh(a, b, c, d, x[10], md5_s31, 0xd9d4d039) # 45
        d = md5hh(d, a, b, c, x[13], md5_s32, 0xe6db99e5) # 46
        c = md5hh(c, d, a, b, x[16], md5_s33, 0x1fa27cf8) # 47
        b = md5hh(b, c, d, a, x[3], md5_s34, 0xc4ac5665) # 48

        # Round 4
        a = md5ii(a, b, c, d, x[1], md5_s41, 0xf4292244) # 49
        d = md5ii(d, a, b, c, x[8], md5_s42, 0x432aff97) # 50
        c = md5ii(c, d, a, b, x[15], md5_s43, 0xab9423a7) # 51
        b = md5ii(b, c, d, a, x[6], md5_s44, 0xfc93a039) # 52
        a = md5ii(a, b, c, d, x[13], md5_s41, 0x655b59c3) # 53
        d = md5ii(d, a, b, c, x[4], md5_s42, 0x8f0ccc92) # 54
        c = md5ii(c, d, a, b, x[11], md5_s43, 0xffeff47d) # 55
        b = md5ii(b, c, d, a, x[2], md5_s44, 0x85845dd1) # 56
        a = md5ii(a, b, c, d, x[9], md5_s41, 0x6fa87e4f) # 57
        d = md5ii(d, a, b, c, x[16], md5_s42, 0xfe2ce6e0) # 58
        c = md5ii(c, d, a, b, x[7], md5_s43, 0xa3014314) # 59
        b = md5ii(b, c, d, a, x[14], md5_s44, 0x4e0811a1) # 60
        a = md5ii(a, b, c, d, x[5], md5_s41, 0xf7537e82) # 61
        d = md5ii(d, a, b, c, x[12], md5_s42, 0xbd3af235) # 62
        c = md5ii(c, d, a, b, x[3], md5_s43, 0x2ad7d2bb) # 63
        b = md5ii(b, c, d, a, x[10], md5_s44, 0xeb86d391) # 64 
    end

    ctx.a += a
    ctx.b += b
    ctx.c += c
    ctx.d += d
end

@inline function update!(ctx::MD5Context, bytes)
    ctx.bitcount += length(bytes) * 8
    spaceleft = 64 - position(ctx.iob)
    if length(bytes) >= spaceleft
        readstart = 1
        write(ctx.iob, @view bytes[readstart:spaceleft])
        transform!(ctx)

        # TODO: correct implementation should transform as many bytes as possible here
        # this will currently raise a bounds error for a too-long `bytes` parameter

        readstart = spaceleft + 1
        write(ctx.iob, @view bytes[readstart:end])
    else
        write(ctx.iob, bytes)
    end
end

@inline function updatebitcount!(ctx::MD5Context, bitcount::UInt64)
    position(ctx.iob) != 56 && error("wrong position for bitcount")
    write(ctx.iob, bitcount)
    transform!(ctx)
end

@inline function final!(ctx::MD5Context)
    bits = ctx.bitcount
    index = 1 + mod(ctx.bitcount ÷ 8, 64)
    padlen = index <= 56 ? 56 - index + 1 : 120 - index + 1
    update!(ctx, @view md5_padding[begin:padlen])
    updatebitcount!(ctx, bits)

    UInt128(ctx.a) | UInt128(ctx.b) << 32 | UInt128(ctx.c) << 64 | UInt128(ctx.d) << 96
end

@inline """
    empty!(ctx::MD5Context)

Resets an MD5Context to its initial state without allocating new storage.
"""
function empty!(ctx::MD5Context)
    ctx.a = 0x67452301
    ctx.b = 0xefcdab89
    ctx.c = 0x98badcfe
    ctx.d = 0x10325476
    ctx.bitcount = 0
    seekstart(ctx.iob)
end

"""
    asciidigits(io::IO, x::Integer)

Writes the ascii representation of `x` to `io`. Slightly faster than `print(io, x)`
and allocates significantly less memory.
"""
function asciidigits(io::IO, x::Integer)
    n = ndigits(x)
    while n > 0
        p = 10^(n - 1)
        place = x ÷ p
        write(io, UInt8('0') + UInt8(place))
        x -= place * p
        n -= 1
    end
end

function search(mask, text, start=0)
    buf = Vector{UInt8}(text)
    iob = IOBuffer(buf; write=true, truncate=false)
    ctx = MD5Context()
    i = start
    while true
        seek(iob, length(text))
        asciidigits(iob, i)
        update!(ctx, buf)
        h = final!(ctx)
        if (h & mask) == 0
            return i
        end
        empty!(ctx)
        i += 1
    end
end

function search_threaded(mask, text, start=0)
    stride = Threads.nthreads()
    result = Threads.Atomic{Int}(typemax(Int))
    Threads.@threads for offset = 1:stride
        buf = Vector{UInt8}(text)
        iob = IOBuffer(buf; write=true, truncate=false)
        ctx = MD5Context()
        i = start + offset
        while true
            seek(iob, length(text))
            asciidigits(iob, i)
            update!(ctx, buf)
            h = final!(ctx)
            if (h & mask) == 0
                Threads.atomic_min!(result, i)
                break
            end
            currentresult = result[]
            if currentresult != typemax(Int) && i > currentresult
                break
            end
            empty!(ctx)
            i += stride
        end
    end
    result[]
end

function solve(text)
    p1 = search_threaded(0xf0ffff, text)
    p2 = search_threaded(0xffffff, text, 1 + p1)
    return p1, p2
end

end
