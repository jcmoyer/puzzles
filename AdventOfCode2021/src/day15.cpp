#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

using risk_map_type = sr::array2d<uint8_t>;

size_t heuristic(const risk_map_type& riskmap, sr::vec2i from, sr::vec2i to) {
    return std::abs(riskmap.at(from) - riskmap.at(to));
}

// more or less copied from https://en.wikipedia.org/wiki/A*_search_algorithm
size_t astar(const risk_map_type& riskmap) {
    sr::vec2i start{};
    sr::vec2i end{(int)riskmap.width() - 1, (int)riskmap.height() - 1};

    std::vector<sr::vec2i> frontier;
    frontier.reserve(riskmap.size());

    frontier.push_back(start);

    std::unordered_map<sr::vec2i, sr::vec2i> came_from;
    came_from.reserve(riskmap.size());

    std::unordered_map<sr::vec2i, size_t> g_score_map;
    g_score_map.reserve(riskmap.size());
    g_score_map[start] = 0;

    std::unordered_map<sr::vec2i, size_t> f_score_map;
    f_score_map.reserve(riskmap.size());
    f_score_map[start] = 0;

    auto heap_pred = [&](auto&& v0, auto&& v1) {
        size_t v0_score = -1;
        if (auto it = f_score_map.find(v0); it != f_score_map.end()) {
            v0_score = it->second;
        }
        size_t v1_score = -1;
        if (auto it = f_score_map.find(v1); it != f_score_map.end()) {
            v1_score = it->second;
        }
        // operator> causes the heap to be a min-heap
        return v0_score > v1_score;
    };

    while (frontier.size()) {
        auto current = frontier.front();

        if (current == end)
            break;

        std::ranges::pop_heap(frontier, heap_pred);
        frontier.pop_back();

        sr::for_neighbors4(riskmap, current, [&](int val, sr::vec2i npos) {
            auto tentative_score = g_score_map[current] + val;
            size_t nscore = -1;
            if (g_score_map.contains(npos)) {
                nscore = g_score_map[npos];
            }
            if (tentative_score < nscore) {
                came_from[npos] = current;
                g_score_map[npos] = tentative_score;
                f_score_map[npos] = tentative_score + heuristic(riskmap, current, npos);
                if (std::ranges::find(frontier, npos) == frontier.end()) {
                    frontier.push_back(npos);
                    std::ranges::push_heap(frontier, heap_pred);
                }
            }
        });
    }
    auto path_cur = end;
    size_t total_risk = 0;
    while (came_from.contains(path_cur)) {
        total_risk += riskmap.at(path_cur);
        path_cur = came_from[path_cur];
    }
    return total_risk;
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
    sr::solution(astar(risk_map));

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
    sr::solution(astar(big_risk_map));
    return 0;
}
