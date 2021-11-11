#include <algorithm>
#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

bool is_increasing(std::string_view n) {
    return std::is_sorted(n.begin(), n.end());
}

bool has_adjacent(std::string_view n) {
    return std::adjacent_find(n.begin(), n.end()) != n.end();
}

// since we know the input is sorted, we only have to count the number of each character
bool has_only_two_adjacent(std::string_view n) {
    int table[10] = {0};
    for (char ch : n)
        ++table[ch - '0'];
    return std::find(std::begin(table), std::end(table), 2) != std::end(table);
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    std::string line;
    std::getline(input, line);

    int lo;
    int hi;
    sr::parse("(\\d+)\\-(\\d+)", line, lo, hi);

    int count_1 = 0;
    int count_2 = 0;
    for (int i = lo; i <= hi; ++i) {
        auto s = std::to_string(i);
        if (!is_increasing(s)) {
            continue;
        }
        if (has_adjacent(s))
            ++count_1;
        if (has_only_two_adjacent(s))
            ++count_2;
    }
    fmt::print("{}\n{}\n", count_1, count_2);

    return 0;
}
