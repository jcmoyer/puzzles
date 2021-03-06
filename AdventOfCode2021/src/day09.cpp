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
    std::vector<vec2i> lowest_points;

    sr::for_each_2d(tm, [&](char h, vec2i p) {
        if (h == '9')
            return;

        int lower = 0;
        int target = 4;

        g.insert(p, {});

        target -= tm.x_edge(p.x());
        target -= tm.y_edge(p.y());

        sr::for_neighbors4(tm, p, [&](auto t, vec2i point) {
            if (h < t) {
                ++lower;
                if (t != '9')
                    g.add_edge(point, p, {});
            }
        });

        if (lower == target) {
            lowest_points.push_back(p);
        }
    });

    std::vector<size_t> basins;

    for (auto p : lowest_points) {
        sum += 1 + (tm[p] - '0');
        size_t sz = 0;
        g.back_dfs(p, [&](auto&&) {
            ++sz;
        });
        basins.push_back(sz);
    }

    std::ranges::sort(basins, std::greater{});

    sr::solution(sum);
    sr::solution(basins[0] * basins[1] * basins[2]);

    return 0;
}
