#include <fmt/format.h>
#include <fstream>
#include <functional>
#include <numeric>

#include <sr/sr.hpp>

template <typename I, typename F>
std::pair<I, I> find_pair(I first, I last, F pred) {
    if (first == last) {
        return {last, last};
    }
    for (I i = first; i != last; ++i)
        for (I j = i + 1; j != last; ++j)
            if (pred(*i, *j))
                return {i, j};
    return {last, last};
}

template <typename I, typename F>
std::pair<I, I> find_range(I first, I last, F pred) {
    if (first == last) {
        return {last, last};
    }
    for (I i = first; i != last; ++i)
        for (I j = i + 1; j != last; ++j)
            if (pred(i, j))
                return {i, j};
    return {last, last};
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector ent = sr::read_lines<int64_t>(input);

    const size_t PREAMBLE_LEN = 25;

    size_t cursor = PREAMBLE_LEN;
    size_t offset = 0;
    int64_t invalid = 0;

    std::vector<int64_t> addends;
    addends.reserve(PREAMBLE_LEN);

    for (size_t cursor = PREAMBLE_LEN; cursor < ent.size(); ++cursor, ++offset) {
        addends.assign(ent.begin() + offset, ent.begin() + offset + PREAMBLE_LEN);

        auto [i, j] = find_pair(addends.begin(), addends.end(), [&](int64_t x, int64_t y) {
            return x + y == ent[cursor];
        });

        if (i == addends.end() && j == addends.end()) {
            invalid = ent[cursor];
            sr::solution(ent[cursor]);
            break;
        }
    }

    auto [i, j] = find_range(ent.begin(), ent.end(), [&](auto first, auto last) {
        return std::accumulate(first, last, 0LL) == invalid;
    });

    if (i != ent.end() && j != ent.end()) {
        auto [min, max] = std::minmax_element(i, j);
        sr::solution(*min + *max);
    }

    return 0;
}
