module Day16

using ..Supafast

const FactDict = Dict{AbstractString,Union{Missing,Int}}

function issame(known::FactDict, unknown::FactDict)
    return 3 == count(skipmissing(values(known) .== values(unknown)))
end

function issame2(known::FactDict, unknown::FactDict)
    facts = [
        known["children"] === unknown["children"]
        known["cats"] < unknown["cats"]
        known["samoyeds"] === unknown["samoyeds"]
        known["pomeranians"] > unknown["pomeranians"]
        known["akitas"] === unknown["akitas"]
        known["vizslas"] === unknown["vizslas"]
        known["goldfish"] > unknown["goldfish"]
        known["trees"] < unknown["trees"]
        known["cars"] === unknown["cars"]
        known["perfumes"] === unknown["perfumes"]
    ]
    return 3 == count(skipmissing(facts))
end

function solve(text::AbstractString)
    known = FactDict(
        "children" => 3,
        "cats" => 7,
        "samoyeds" => 2,
        "pomeranians" => 3,
        "akitas" => 0,
        "vizslas" => 0,
        "goldfish" => 5,
        "trees" => 3,
        "cars" => 2,
        "perfumes" => 1
    )

    patt = r"(\w+): (\d+)"

    sues = Vector{Dict}()
    for line in eachline(IOBuffer(text))
        sue = FactDict(
            "children" => missing,
            "cats" => missing,
            "samoyeds" => missing,
            "pomeranians" => missing,
            "akitas" => missing,
            "vizslas" => missing,
            "goldfish" => missing,
            "trees" => missing,
            "cars" => missing,
            "perfumes" => missing,
        )

        for r in findall(patt, line)
            name, count = match(patt, line[r])
            sue[name] = parse(Int, count)
        end

        push!(sues, sue)
    end

    p1 = findfirst(sues) do s
        issame(known, s)
    end
    p2 = findfirst(sues) do s
        issame2(known, s)
    end
    return p1, p2
end

end
