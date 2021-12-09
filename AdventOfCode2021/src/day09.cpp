#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

using vec2i = sr::vec2i;

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    auto tm = sr::read_tilemap(input);
    int sum = 0;

    sr::graph<vec2i> g;

    for (int y = 0; y < tm.height(); ++y) {
        for (int x = 0; x < tm.width(); ++x) {
            char v = tm.at(x, y);
            int lower = 0;
            int target = 4;

            if (v != '9')
                g.insert(vec2i{x, y}, {});

            if (x == 0 || x == tm.width() - 1) {
                --target;
            }
            if (y == 0 || y == tm.height() - 1) {
                --target;
            }

            if (x - 1 >= 0 && v < tm.at(x - 1, y)) {
                ++lower;
                if (v != '9' && tm.at(x - 1, y) != '9')
                    g.add_edge(vec2i{x - 1, y}, vec2i{x, y}, {});
            }
            if (x + 1 < tm.width() && v < tm.at(x + 1, y)) {
                ++lower;
                if (v != '9' && tm.at(x + 1, y) != '9')
                    g.add_edge(vec2i{x + 1, y}, vec2i{x, y}, {});
            }

            if (y - 1 >= 0 && v < tm.at(x, y - 1)) {
                ++lower;
                if (v != '9' && tm.at(x, y - 1) != '9')
                    g.add_edge(vec2i{x, y - 1}, vec2i{x, y}, {});
            }
            if (y + 1 < tm.height() && v < tm.at(x, y + 1)) {
                ++lower;
                if (v != '9' && tm.at(x, y + 1) != '9')
                    g.add_edge(vec2i{x, y + 1}, vec2i{x, y}, {});
            }

            if (lower == target) {
                sum += 1 + (v - '0');
            }
        }
    }

    std::vector<size_t> basins;

    for (int y = 0; y < tm.height(); ++y) {
        for (int x = 0; x < tm.width(); ++x) {
            if (g.contains(vec2i{x, y})) {
                size_t sz = 0;
                g.back_dfs(vec2i{x, y}, [&](auto&&) {
                    ++sz;
                });
                basins.push_back(sz);
            }
        }
    }

    std::ranges::sort(basins, std::greater{});

    sr::solution(sum);
    sr::solution(basins[0] * basins[1] * basins[2]);

    return 0;
}
