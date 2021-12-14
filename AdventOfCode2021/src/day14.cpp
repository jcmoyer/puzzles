#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

size_t reduce(std::string_view polymer, const std::unordered_map<std::string, std::string>& insertions, size_t steps) {
    char first = polymer.front(), last = polymer.back();
    std::unordered_map<std::string, uint64_t> occur_pairs, occur_trips;
    // split polymer into pairs
    for (int j = 0; j < polymer.size() - 1; ++j) {
        char left = polymer[j];
        char right = polymer[j + 1];
        std::string xy;
        xy += left;
        xy += right;
        occur_pairs[xy]++;
    }
    for (size_t i = 0; i < steps; ++i) {
        // each pair gets turned into a triplet
        occur_trips.clear();
        for (auto&& [k, v] : occur_pairs) {
            if (auto it = insertions.find(k); it != insertions.end()) {
                std::string triplet;
                triplet += k[0];
                triplet += it->second;
                triplet += k[1];
                occur_trips.insert(std::make_pair(triplet, v));
            }
        }
        // reduce triplets into pairs
        occur_pairs.clear();
        for (auto&& [k, v] : occur_trips) {
            occur_pairs[k.substr(0, 2)] += v;
            occur_pairs[k.substr(1, 2)] += v;
        }
    }
    std::unordered_map<char, uint64_t> freq;
    for (auto&& [k, v] : occur_pairs) {
        for (char c : k) {
            freq[c] += v;
        }
    }
    // adjust for character sharing; all pairs share both characters with the exception
    // of the leftmost and rightmost character in the original polymer
    for (auto&& [k, v] : freq) {
        if (k == first || k == last) {
            // TODO: this might be incorrect if first == last
            v = v / 2 + 1;
        } else {
            v /= 2;
        }
    }
    auto [min, max] = std::ranges::minmax_element(freq, [](auto&& a, auto&& b) {
        return a.second < b.second;
    });
    return max->second - min->second;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string polymer;
    std::getline(input, polymer);

    std::unordered_map<std::string, std::string> insertions;

    for (const auto& line : sr::lines(input)) {
        if (line.size() == 0)
            continue;
        std::string from, to;
        sr::parse(R"((\w+) -> (\w+))", line, from, to);
        insertions[from] = to;
    }

    sr::solution(reduce(polymer, insertions, 10));
    sr::solution(reduce(polymer, insertions, 40));

    return 0;
}
