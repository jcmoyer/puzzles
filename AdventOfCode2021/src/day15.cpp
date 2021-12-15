#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

using risk_map_type = sr::array2d<uint8_t>;

size_t heuristic(const risk_map_type& riskmap, sr::vec2i from, sr::vec2i to) {
    return std::abs(riskmap.at(from) - riskmap.at(to));
}

struct path_score {
    size_t g_score, f_score;

    static constexpr path_score infinity() {
        return {std::numeric_limits<size_t>::max(), std::numeric_limits<size_t>::max()};
    }
};

size_t sum_risk(const risk_map_type& riskmap, const sr::array2d<sr::vec2i>& pathmap) {
    sr::vec2i start{};
    sr::vec2i end{(int)riskmap.width() - 1, (int)riskmap.height() - 1};

    auto path_cur = end;
    size_t total_risk = 0;
    while (path_cur != start) {
        total_risk += riskmap.at(path_cur);
        path_cur = pathmap[path_cur];
    }
    return total_risk;
}

// implementation based on pseudocode from https://en.wikipedia.org/wiki/A*_search_algorithm
size_t astar(const risk_map_type& riskmap) {
    sr::vec2i start{};
    sr::vec2i end{(int)riskmap.width() - 1, (int)riskmap.height() - 1};

    std::vector<sr::vec2i> frontier;
    frontier.reserve(riskmap.size());
    // used for O(1) lookup
    std::unordered_set<sr::vec2i> frontier_set;
    frontier_set.reserve(riskmap.size());

    frontier.push_back(start);
    frontier_set.insert(start);

    sr::array2d<sr::vec2i> came_from(riskmap.width(), riskmap.height());

    sr::array2d<path_score> score_map(riskmap.width(), riskmap.height(), path_score::infinity());
    score_map[start] = path_score{0, 0};

    auto heap_pred = [&](auto&& v0, auto&& v1) {
        // operator> causes the heap to be a min-heap
        return score_map[v0].f_score > score_map[v1].f_score;
    };

    while (frontier.size()) {
        auto current = frontier.front();

        if (current == end)
            return sum_risk(riskmap, came_from);

        std::ranges::pop_heap(frontier, heap_pred);
        frontier.pop_back();
        frontier_set.erase(current);

        sr::for_neighbors4(riskmap, current, [&](uint8_t val, sr::vec2i npos) {
            auto tentative_score = score_map[current].g_score + val;
            size_t nscore = score_map[npos].g_score;
            if (tentative_score < nscore) {
                came_from[npos] = current;
                score_map[npos].g_score = tentative_score;
                score_map[npos].f_score = tentative_score + heuristic(riskmap, current, npos);
                if (!frontier_set.contains(npos)) {
                    frontier.push_back(npos);
                    std::ranges::push_heap(frontier, heap_pred);
                    frontier_set.insert(npos);
                }
            }
        });
    }

    throw std::runtime_error("no path to end");
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
