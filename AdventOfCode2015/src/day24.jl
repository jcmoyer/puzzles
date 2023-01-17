module Day24

using ..Supafast

# Hand solved solution:
#
# input = {1,3,5,11,13,17,19,23,29,31,37,41,43,47,53,59,67,71,73,79,83,89,97,101,103,107,109,113}
#
# input[2:end] .- input[1:end-1]
#           +2+2 +6 +2 +4 +2 +4 +6 +2 +6 +4 +2 +4 +6 +6 +8 +4 +2 +6 +4 +6 +8  +4  +2  +4  +2  +4
#
# all numbers are prime
# g1, g2, g3 ⊆ input
# ∑g1 = ∑g2 = ∑g3 = ∑input ÷ 3 = 508
#
# g1 = {109,107,103,101,83,5}
# g2 = {113,97,89,79,73,53,3,1}
# g3 = {71,67,59,47,43,41,37,31,29,23,19,17,13,11}
#
# ∏g1 = 50352028435
# ∏g2 = 894514243137
# ∏g3 = 821794338336351260023 (BigInt required)
#
# the minimal solution has an upper bound at ∏g1
#
# other possible? solutions:
# 31112183811 ({109,107,103,97,89,3})
# 10542541903 ({113,107,101,97,89,1})
# 10439961859 ({113,107,103,101,83,1})  (actual solution)
#
# Q: are all subsets summing to 508 valid?
#
# == part 2 ==
# same idea, but 4 groups
#
# g1, g2, g3, g4 ⊆ input
# ∑g1 = ∑g2 = ∑g3 = ∑g4 = ∑input ÷ 4 = 381
#
# {113,107,101,59,1} -> 72050269  (actual solution)
# {109,103,101,67,1} -> 75973109
#
# since these solution sets have lengths 6 and 5, brute forcing combinations
# should be fast enough (376740, 98280 combinations respectively)

import Combinatorics: combinations

function qe(weights; groupcount)
    sumto = sum(weights) ÷ groupcount
    for i in 1:length(weights)
        solutionhere = nothing
        for c in combinations(weights, i)
            if sum(c) == sumto
                p = prod(c)
                if solutionhere === nothing
                    solutionhere = p
                else
                    solutionhere = min(solutionhere, p)
                end
            end
        end
        if solutionhere !== nothing
            return solutionhere
        end
    end
    return nothing
end

function solve(text::AbstractString)
    weights = maplines(text, r"(\d+)" => parseint)
    sort!(weights)
    return qe(weights; groupcount=3), qe(weights; groupcount=4)
end

end
