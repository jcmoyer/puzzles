#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

void flash(sr::array2d<int>& tm, sr::vec2i p, std::unordered_set<sr::vec2i>& flashed) {
    flashed.insert(p);
    sr::for_neighbors8(tm, p, [&](int& e, auto p) {
        ++e;
    });
}

int count_flashable(sr::array2d<int>& tm, std::unordered_set<sr::vec2i>& flashed) {
    int count = 0;
    sr::for_each_2d(tm, [&](int& e, auto p) {
        if (!flashed.contains(p) && e > 9)
            ++count;
    });
    return count;
}

int tick(sr::array2d<int>& m) {
    std::unordered_set<sr::vec2i> flashed;

    int count = 0;

    for (int& e : m)
        ++e;

    while (count_flashable(m, flashed)) {
        sr::for_each_2d(m, [&](int& e, auto p) {
            if (e > 9 && !flashed.count(p)) {
                flash(m, p, flashed);
                ++count;
            }
        });
    }

    for (auto p : flashed) {
        m.at(p) = 0;
    }

    return count;
}

sr::array2d<int> to_int_map(const sr::array2d<char>& m) {
    sr::array2d<int> out(m.width(), m.height());
    sr::for_each_2d(m, [&](char ch, auto point) {
        out.at(point) = ch - '0';
    });
    return out;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);
    auto char_tilemap = sr::read_tilemap(input);

    sr::array2d<int> octos;

    // part 1
    octos = to_int_map(char_tilemap);
    int flashes = 0;
    for (int i = 0; i < 100; ++i) {
        flashes += tick(octos);
    }
    sr::solution(flashes);

    // part 2
    octos = to_int_map(char_tilemap);
    for (int i = 0;; ++i) {
        if (tick(octos) == 100) {
            sr::solution(1 + i);
            break;
        }
    }

    return 0;
}
