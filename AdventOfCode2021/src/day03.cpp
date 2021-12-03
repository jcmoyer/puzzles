#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct column_bitcount {
    int ones = 0, zeroes = 0;
};

enum class filter_instruction {
    erase_one,
    erase_zero,
    erase_none,
};

std::vector<column_bitcount> count_bits(const std::vector<std::string>& rows) {
    std::vector<column_bitcount> bitcounts;
    for (size_t x = 0; x < rows[0].size(); ++x) {
        column_bitcount& count = bitcounts.emplace_back();
        for (size_t y = 0; y < rows.size(); ++y) {
            if (rows[y][x] == '1') {
                ++count.ones;
            } else {
                ++count.zeroes;
            }
        }
    }
    return bitcounts;
}

// Predicate receives the bitcount for column `bitpos` and returns a filter_instruction
template <typename Predicate>
void filter_candidates(std::vector<std::string>& candidates, size_t bitpos, Predicate pred) {
    // [!] bit counting is iterative, you cannot use the first bit count!
    // bits must be recounted after each filter step
    auto counts = count_bits(candidates);
    auto instr = pred(counts[bitpos]);
    for (size_t i = candidates.size() - 1; i != -1; --i) {
        // [!] filtering has to stop at one element remaining so we can't use e.g. std::erase_if
        if (candidates.size() == 1) {
            break;
        }
        switch (instr) {
        case filter_instruction::erase_zero:
            if (candidates[i][bitpos] == '0') {
                candidates.erase(candidates.begin() + i);
            }
            break;
        case filter_instruction::erase_one:
            if (candidates[i][bitpos] == '1') {
                candidates.erase(candidates.begin() + i);
            }
            break;
        default:
            break;
        }
    }
}

void part1(const std::vector<std::string>& lines) {
    auto bcs = count_bits(lines);
    size_t bitval = 0;
    size_t bitmask = 0;
    for (size_t i = bcs.size() - 1, j = 0; i != -1; --i, ++j) {
        bitval |= static_cast<size_t>(bcs[i].ones > bcs[i].zeroes) << j;
        bitmask |= 1ull << j;
    }
    sr::solution(bitval * ((~bitval) & bitmask));
}

void part2(const std::vector<std::string>& lines) {
    std::vector<std::string> oxycandidates = lines;
    std::vector<std::string> co2candidates = lines;
    for (size_t x = 0; x < lines[0].size(); ++x) {
        filter_candidates(oxycandidates, x, [](column_bitcount bc) {
            return bc.ones >= bc.zeroes ? filter_instruction::erase_zero : filter_instruction::erase_one;
        });
        filter_candidates(co2candidates, x, [](column_bitcount bc) {
            return bc.ones < bc.zeroes ? filter_instruction::erase_zero : filter_instruction::erase_one;
        });
    }
    sr::solution(std::stoi(oxycandidates[0], 0, 2) * std::stoi(co2candidates[0], 0, 2));
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);
    auto lines = sr::read_lines<std::string>(input);

    part1(lines);
    part2(lines);

    return 0;
}
