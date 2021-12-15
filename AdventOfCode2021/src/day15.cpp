#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

using risk_map_type = sr::array2d<uint8_t>;

struct controller {
    int heuristic(const risk_map_type& a, sr::vec2i from, sr::vec2i to) {
        return sr::manhattan(to, sr::vec2i{(int)a.width() - 1, (int)a.height() - 1});
    }

    int weight(const risk_map_type& a, sr::vec2i from, sr::vec2i to) {
        return a[to];
    }

    template <typename Buffer>
    void neighbors(const risk_map_type& a, sr::vec2i pos, Buffer& buf) {
        sr::for_neighbors4(a, pos, [&](auto, auto npos) {
            buf.push_back(npos);
        });
    }
};

size_t solve(const risk_map_type& riskmap) {
    sr::vec2i start{};
    sr::vec2i end{(int)riskmap.width() - 1, (int)riskmap.height() - 1};
    std::vector<sr::vec2i> path = sr::astar(riskmap, start, end, controller{});
    size_t result = 0;
    for (size_t i = 1; i < path.size(); ++i) {
        result += riskmap[path[i]];
    }
    return result;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    auto map = sr::read_tilemap(input);

    risk_map_type risk_map(map.width(), map.height());
    for (size_t i = 0; i < map.size(); ++i) {
        char* ch = map.data() + i;
        *(risk_map.data() + i) = *ch - '0';
    }
    sr::solution(solve(risk_map));

    risk_map_type big_risk_map(map.width() * 5, map.height() * 5);
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < 5; ++x) {
            int subarea_min_x = x * map.width();
            int subarea_max_x = subarea_min_x + map.width();
            int subarea_min_y = y * map.height();
            int subarea_max_y = subarea_min_y + map.height();
            int risk_bonus = x + y;
            for (int sy = subarea_min_y; sy < subarea_max_y; ++sy) {
                for (int sx = subarea_min_x; sx < subarea_max_x; ++sx) {
                    int src = risk_map.at(sx - subarea_min_x, sy - subarea_min_y);
                    int val = src + risk_bonus;
                    if (val > 9) {
                        val %= 10;
                        val += 1;
                    }
                    big_risk_map.at(sx, sy) = val;
                }
            }
        }
    }
    sr::solution(solve(big_risk_map));

    return 0;
}
