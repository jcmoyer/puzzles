#include <atomic>
#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <iostream>
#include <numeric>
#include <thread>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

#include "aoc/intcode.hpp"

bool is_label_char(char ch) {
    return ch >= 'A' && ch <= 'Z';
}

struct portal {
    std::string name;
    sr::vec2i p0, p1;

    void insert(const sr::vec2i& p) {
        if (p0 == sr::vec2i{}) {
            p0 = p;
        } else if (p1 == sr::vec2i{}) {
            p1 = p;
        } else {
            throw std::runtime_error("tried to associate more than 2 points with portal");
        }
    }

    bool is_oneway() const {
        return p0 != sr::vec2i{} && p1 == sr::vec2i{};
    }

    sr::vec2i opposite(const sr::vec2i& from) const {
        if (p0 == from)
            return p1;
        else if (p1 == from)
            return p0;
        else
            throw std::runtime_error("point not in portal");
    }

    const sr::vec2i& point(int i) const {
        if (i == 0)
            return p0;
        else if (i == 1)
            return p1;
        else
            throw std::runtime_error("point not in portal");
    }
};

struct world {
    std::unordered_map<sr::vec2i, char> tilemap;
    std::unordered_map<std::string, portal> portals;
    std::unordered_map<sr::vec2i, std::string> reverse_portal_map;

    size_t width, height;

    void trim() {
        std::vector<sr::vec2i> removable;
        for (auto& [pos, tile] : tilemap) {
            if (tile != '.' && tile != '#') {
                removable.push_back(pos);
            }
        }
        for (const auto& pos : removable) {
            tilemap.erase(pos);
        }
    }

    bool is_outer(const sr::vec2i& p) const {
        return p.x() == 2 || p.x() == width - 3 || p.y() == 2 || p.y() == height - 3;
    }

    bool is_inner(const sr::vec2i& p) const {
        return !is_outer(p);
    }

    const sr::vec2i& select_outer(const portal& p) const {
        if (is_outer(p.p0))
            return p.p0;
        if (is_outer(p.p1))
            return p.p1;
        throw std::runtime_error("neither points are outer");
    }

    const sr::vec2i& select_inner(const portal& p) const {
        if (is_inner(p.p0))
            return p.p0;
        if (is_inner(p.p1))
            return p.p1;
        throw std::runtime_error("neither points are inner");
    }
};

struct path_visit {
    std::unordered_set<sr::vec2i> points;
    std::unordered_set<std::string> portals;
};

int64_t distance(const world& w, const portal& from, const portal& to) {
    struct search_state {
        sr::vec2i pos{};
        int64_t dist = 0;
        std::string path;
    };

    std::vector<search_state> states;

    std::deque<search_state> frontier;
    // std::unordered_set<sr::vec2i> visited;

    std::unordered_map<std::string, std::unordered_set<sr::vec2i>> visited;

    frontier.push_back(search_state{from.p0, 0, from.name});

    while (frontier.size()) {
        search_state current = frontier.front();
        frontier.pop_front();

        if (visited[current.path].count(current.pos)) {
            continue;
        } else {
            visited[current.path].insert(current.pos);
        }

        std::string last_portal_name = current.path.substr(current.path.size() - 2);

        // avoid re-entering the same portal
        size_t cycle = current.path.find(last_portal_name);
        if (cycle != current.path.size() - 2 && cycle % 2 == 0) {
            continue;
        }

        // clang-format off
        auto up    = current.pos + sr::vec2i{ 0, -1};
        auto down  = current.pos + sr::vec2i{ 0, +1};
        auto left  = current.pos + sr::vec2i{-1,  0};
        auto right = current.pos + sr::vec2i{ 1,  0};
        // clang-format on

        if (auto it = w.tilemap.find(up); it != w.tilemap.end() && it->second == '.') {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (port.name == to.name) {
                    states.push_back(current);
                } else {
                    const auto new_p = port.opposite(portal_it->first);
                    frontier.push_back({new_p, 2 + current.dist, current.path + portal_it->second});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path});
            }
        }
        if (auto it = w.tilemap.find(down); it != w.tilemap.end() && it->second == '.') {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (port.name == to.name) {
                    states.push_back(current);
                } else {
                    const auto new_p = port.opposite(portal_it->first);
                    frontier.push_back({new_p, 2 + current.dist, current.path + portal_it->second});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path});
            }
        }
        if (auto it = w.tilemap.find(left); it != w.tilemap.end() && it->second == '.') {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (port.name == to.name) {
                    states.push_back(current);
                } else {
                    const auto new_p = port.opposite(portal_it->first);
                    frontier.push_back({new_p, 2 + current.dist, current.path + portal_it->second});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path});
            }
        }
        if (auto it = w.tilemap.find(right); it != w.tilemap.end() && it->second == '.') {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (port.name == to.name) {
                    states.push_back(current);
                } else {
                    const auto new_p = port.opposite(portal_it->first);
                    frontier.push_back({new_p, 2 + current.dist, current.path + portal_it->second});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path});
            }
        }
    }

    return 1 + std::min_element(states.begin(), states.end(), [](const auto& s0, const auto& s1) {
        return s0.dist < s1.dist;
    })->dist;
}

int64_t distance_part2(const world& w, const portal& from, const portal& to) {
    struct search_state {
        sr::vec2i pos{};
        int64_t dist = 0;
        std::string path;
        int level = 0;
    };

    std::vector<search_state> states;

    std::deque<search_state> frontier;

    std::unordered_map<int, std::unordered_map<std::string, std::unordered_set<sr::vec2i>>> visited;

    frontier.push_back(search_state{from.p0, 0, from.name});

    while (frontier.size()) {
        search_state current = frontier.front();
        frontier.pop_front();

        if (visited[current.level][current.path].count(current.pos)) {
            continue;
        } else {
            visited[current.level][current.path].insert(current.pos);
        }

        if (states.size())
            break;

        // completely arbitrary limit to prevent the program from running until the heat death of the universe
        if (current.level > 30)
            continue;

        std::string last_portal_name = current.path.substr(current.path.size() - 2);

        // avoid re-entering the same portal
        size_t cycle = current.path.find(last_portal_name);
        if (cycle != current.path.size() - 2 && cycle % 2 == 0) {
            continue;
        }

        // clang-format off
        auto up    = current.pos + sr::vec2i{ 0, -1};
        auto down  = current.pos + sr::vec2i{ 0, +1};
        auto left  = current.pos + sr::vec2i{-1,  0};
        auto right = current.pos + sr::vec2i{ 1,  0};
        // clang-format on

        if (auto it = w.tilemap.find(up);
            it != w.tilemap.end() && it->second == '.' && !visited[current.level][current.path].count(it->first)) {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (current.level == 0 && w.is_outer(it->first) && port.name == to.name) {
                    states.push_back(current);
                } else if (w.is_inner(it->first)) {
                    const auto warp_pt = w.select_outer(port);
                    int new_level = current.level + 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                } else if (current.level > 0 && w.is_outer(it->first) && port.name != from.name &&
                           port.name != to.name) {
                    const auto warp_pt = w.select_inner(port);
                    int new_level = current.level - 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path, current.level});
            }
        }
        if (auto it = w.tilemap.find(down);
            it != w.tilemap.end() && it->second == '.' && !visited[current.level][current.path].count(it->first)) {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (current.level == 0 && w.is_outer(it->first) && port.name == to.name) {
                    states.push_back(current);
                } else if (w.is_inner(it->first)) {
                    const auto warp_pt = w.select_outer(port);
                    int new_level = current.level + 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                } else if (current.level > 0 && w.is_outer(it->first) && port.name != from.name &&
                           port.name != to.name) {
                    const auto warp_pt = w.select_inner(port);
                    int new_level = current.level - 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path, current.level});
            }
        }
        if (auto it = w.tilemap.find(left);
            it != w.tilemap.end() && it->second == '.' && !visited[current.level][current.path].count(it->first)) {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (current.level == 0 && w.is_outer(it->first) && port.name == to.name) {
                    states.push_back(current);
                } else if (w.is_inner(it->first)) {
                    const auto warp_pt = w.select_outer(port);
                    int new_level = current.level + 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                } else if (current.level > 0 && w.is_outer(it->first) && port.name != from.name &&
                           port.name != to.name) {
                    const auto warp_pt = w.select_inner(port);
                    int new_level = current.level - 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path, current.level});
            }
        }
        if (auto it = w.tilemap.find(right);
            it != w.tilemap.end() && it->second == '.' && !visited[current.level][current.path].count(it->first)) {
            if (auto portal_it = w.reverse_portal_map.find(it->first); portal_it != w.reverse_portal_map.end()) {
                const auto port = w.portals.at(portal_it->second);
                if (current.level == 0 && w.is_outer(it->first) && port.name == to.name) {
                    states.push_back(current);
                } else if (w.is_inner(it->first)) {
                    const auto warp_pt = w.select_outer(port);
                    int new_level = current.level + 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                } else if (current.level > 0 && w.is_outer(it->first) && port.name != from.name &&
                           port.name != to.name) {
                    const auto warp_pt = w.select_inner(port);
                    int new_level = current.level - 1;
                    frontier.push_back({warp_pt, 2 + current.dist, current.path + portal_it->second, new_level});
                }
            } else {
                frontier.push_back({it->first, 1 + current.dist, current.path, current.level});
            }
        }
    }

    return 1 + std::min_element(states.begin(), states.end(), [](const auto& s0, const auto& s1) {
        return s0.dist < s1.dist;
    })->dist;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    std::string line;

    int y = 0;

    world w;
    w.width = 0;
    w.height = 0;

    while (std::getline(input, line)) {
        w.width = std::max(w.width, line.size());
        ++w.height;
        for (int x = 0; x < line.size(); ++x) {
            sr::vec2i p{x, y};
            w.tilemap[p] = line[x];
        }
        ++y;
    }

    for (auto& [pos, tile] : w.tilemap) {
        if (tile == '.') {
            // clang-format off
            auto up    = pos + sr::vec2i{ 0, -1};
            auto down  = pos + sr::vec2i{ 0, +1};
            auto left  = pos + sr::vec2i{-1,  0};
            auto right = pos + sr::vec2i{ 1,  0};
            // clang-format on

            if (auto it = w.tilemap.find(up); it != w.tilemap.end() && is_label_char(it->second)) {
                auto upup = w.tilemap.at(up + sr::vec2i{0, -1});
                std::string label{upup, it->second};
                w.portals[label].name = label;
                w.portals[label].insert(pos);
                w.reverse_portal_map[pos] = label;
            }

            if (auto it = w.tilemap.find(down); it != w.tilemap.end() && is_label_char(it->second)) {
                auto downdown = w.tilemap.at(down + sr::vec2i{0, +1});
                std::string label{it->second, downdown};
                w.portals[label].name = label;
                w.portals[label].insert(pos);
                w.reverse_portal_map[pos] = label;
            }

            if (auto it = w.tilemap.find(left); it != w.tilemap.end() && is_label_char(it->second)) {
                auto leftleft = w.tilemap.at(left + sr::vec2i{-1, 0});
                std::string label{leftleft, it->second};
                w.portals[label].name = label;
                w.portals[label].insert(pos);
                w.reverse_portal_map[pos] = label;
            }

            if (auto it = w.tilemap.find(right); it != w.tilemap.end() && is_label_char(it->second)) {
                auto rightright = w.tilemap.at(right + sr::vec2i{1, 0});
                std::string label{it->second, rightright};
                w.portals[label].name = label;
                w.portals[label].insert(pos);
                w.reverse_portal_map[pos] = label;
            }
        }
    }

    w.trim();

    fmt::print("{}\n", distance(w, w.portals.at("AA"), w.portals.at("ZZ")));
    fmt::print("{}\n", distance_part2(w, w.portals.at("AA"), w.portals.at("ZZ")));

    return 0;
}
