#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <sr/sr.hpp>

const char OPEN = '.';
const char LUMBER = '#';
const char TREE = '|';

using automata = sr::array2d<char>;
using point = sr::vec2i;

namespace std {
template <>
struct hash<automata> {
    std::size_t operator()(const automata& a) const {
        // very naive and slow hash function but it gets the job done
        std::size_t h = 0;
        for (char c : a) {
            h += c;
        }
        return h;
    }
};
}

int64_t value(automata& ca) {
    int64_t wood = 0;
    int64_t lumber = 0;
    for (char c : ca) {
        if (c == TREE)
            ++wood;
        else if (c == LUMBER)
            ++lumber;
    }
    return wood * lumber;
}

void tick(automata& ca) {
    automata new_ca(ca);
    for (int y = 0; y < ca.height(); ++y) {
        for (int x = 0; x < ca.width(); ++x) {
            char c = ca[point{x, y}];
            if (c == OPEN) {
                int n_trees = 0;
                for (int yy = -1; yy <= +1; ++yy) {
                    for (int xx = -1; xx <= +1; ++xx) {
                        if (xx == 0 && yy == 0)
                            continue;
                        if (ca[point{x + xx, y + yy}] == TREE)
                            ++n_trees;
                    }
                }
                if (n_trees >= 3) {
                    new_ca[point{x, y}] = TREE;
                }
            } else if (c == TREE) {
                int n_lumber = 0;
                for (int yy = -1; yy <= +1; ++yy) {
                    for (int xx = -1; xx <= +1; ++xx) {
                        if (xx == 0 && yy == 0)
                            continue;
                        if (ca[point{x + xx, y + yy}] == LUMBER)
                            ++n_lumber;
                    }
                }
                if (n_lumber >= 3) {
                    new_ca[point{x, y}] = LUMBER;
                }
            } else if (c == LUMBER) {
                int n_trees = 0, n_lumber = 0;
                for (int yy = -1; yy <= +1; ++yy) {
                    for (int xx = -1; xx <= +1; ++xx) {
                        if (xx == 0 && yy == 0)
                            continue;
                        char c2 = ca[point{x + xx, y + yy}];
                        if (c2 == TREE)
                            ++n_trees;
                        else if (c2 == LUMBER)
                            ++n_lumber;
                    }
                }
                if (n_trees > 0 && n_lumber > 0) {
                    new_ca[point{x, y}] = LUMBER;
                } else {
                    new_ca[point{x, y}] = OPEN;
                }
            }
        }
    }
    ca = std::move(new_ca);
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;

    int height = 0;
    int width = 0;
    while (std::getline(input, line)) {
        width = line.size();
        ++height;
    }
    input.clear();
    input.seekg(0);

    // inflate width/height to avoid bounds checks
    automata ca(width + 2, height + 2);

    int y = 0;
    while (std::getline(input, line)) {
        for (int x = 0; x < line.size(); ++x) {
            ca[point{x + 1, y + 1}] = line[x];
        }
        ++y;
    }

    // store a copy of init state for part2
    automata ca_init(ca);

    // part1
    for (int i = 0; i < 10; ++i) {
        tick(ca);
    }

    std::cout << value(ca) << std::endl;

    // part2
    // restore initial state
    ca = std::move(ca_init);

    struct ca_iteration {
        int64_t gen;
        int64_t val;
    };
    std::unordered_map<automata, ca_iteration> cache;
    for (int64_t i = 0; i < 1000000000; ++i) {
        tick(ca);
        if (auto prev_state = cache.find(ca); prev_state != cache.end()) {
            int64_t prev_gen = prev_state->second.gen;
            int64_t cur_gen = i + 1;
            int64_t gens_left = 1000000000 - prev_gen;
            int64_t gen_diff = cur_gen - prev_gen;

            // we want to find the corresponding extrapolated state between prev_gen and cur_gen
            int64_t search_gen_id = prev_gen + (gens_left % gen_diff);

            auto search_state = std::find_if(cache.begin(), cache.end(), [&](auto&& kvp) {
                return kvp.second.gen == search_gen_id;
            });

            std::cout << search_state->second.val << std::endl;
            break;
        } else {
            cache.emplace(std::make_pair(ca, ca_iteration{i + 1, value(ca)}));
        }
    }

    return 0;
}
