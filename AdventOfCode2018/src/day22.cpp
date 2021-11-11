#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <deque>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/array2d.hpp"

// A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183.
int64_t erosion_level(int64_t geo_index, int64_t cave_depth) {
    return (geo_index + cave_depth) % 20183;
}

struct point64 {
    int64_t x;
    int64_t y;

    point to_point() {
        return {(int)x, (int)y};
    }
};
bool operator==(const point64& a, const point64& b) {
    return a.x == b.x && a.y == b.y;
}
bool operator!=(const point64& a, const point64& b) {
    return !(a == b);
}
namespace std {
template <>
struct hash<point64> {
    std::size_t operator()(const point64& p) const {
        return (p.x << 32 | p.y);
    }
};
}

int64_t geologic_index(int64_t x, int64_t y, int64_t target_x, int64_t target_y, int64_t cave_depth) {
    static std::unordered_map<point64, int64_t> cache;
    if (auto it = cache.find(point64{x, y}); it != cache.end()) {
        return it->second;
    }

    int64_t value;

    if (x == 0 && y == 0) {
        value = 0;
    } else if (x == target_x && y == target_y) {
        value = 0;
    } else if (x == 0) {
        value = y * 48271;
    } else if (y == 0) {
        value = x * 16807;
    } else {
        value = erosion_level(geologic_index(x - 1, y, target_x, target_y, cave_depth), cave_depth) *
                erosion_level(geologic_index(x, y - 1, target_x, target_y, cave_depth), cave_depth);
    }

    cache.insert_or_assign(point64{x, y}, value);
    return value;
}

enum terrain_type { rocky, wet, narrow };

terrain_type terrain_for_erosion(int64_t erosion) {
    if (erosion % 3 == 0)
        return rocky;
    if (erosion % 3 == 1)
        return wet;
    if (erosion % 3 == 2)
        return narrow;
}

int64_t terrain_risk(terrain_type t) {
    if (t == rocky)
        return 0;
    if (t == wet)
        return 1;
    if (t == narrow)
        return 2;
}

enum equipment { undefined = 0, neither = 1, climbing_gear = 2, torch = 3 };

// rocky: climbing gear OR torch
// wet: climbing gear OR neither
// narrow: torch OR neither
int64_t switch_cost(terrain_type current, equipment eq, terrain_type dest, equipment& new_eq) {
    if (current == dest) {
        new_eq = eq;
        return 0;
    }

    if (current == rocky && dest == wet) {
        if (eq == climbing_gear) {
            new_eq = eq;
            return 0;
        }
        if (eq == torch) {
            new_eq = neither;
            return 7;
        }
    } else if (current == rocky && dest == narrow) {
        if (eq == climbing_gear) {
            new_eq = torch;
            return 7;
        }
        if (eq == torch) {
            new_eq = eq;
            return 0;
        }
    } else if (current == wet && dest == rocky) {
        if (eq == climbing_gear) {
            new_eq = eq;
            return 0;
        }
        if (eq == neither) {
            new_eq = climbing_gear;
            return 7;
        }
    } else if (current == wet && dest == narrow) {
        if (eq == climbing_gear) {
            new_eq = neither;
            return 7;
        }
        if (eq == neither) {
            new_eq = eq;
            return 0;
        }
    } else if (current == narrow && dest == rocky) {
        if (eq == torch) {
            new_eq = eq;
            return 0;
        }
        if (eq == neither) {
            new_eq = torch;
            return 7;
        }
    } else if (current == narrow && dest == wet) {
        if (eq == torch) {
            new_eq = neither;
            return 7;
        }
        if (eq == neither) {
            new_eq = eq;
            return 0;
        }
    }
    throw;
}

template <typename K, typename V>
const V& try_get(const std::unordered_map<K, V>& map, const K& key, const V& d) {
    if (auto it = map.find(key); it != map.end()) {
        return it->second;
    } else {
        return d;
    }
}

std::vector<point64> neighbors(const point64& p) {
    return {{p.x, p.y - 1}, {p.x - 1, p.y}, {p.x + 1, p.y}, {p.x, p.y + 1}};
}

std::vector<point64> astar(const array2d<terrain_type>& t, point64 start, point64 target) {
    std::unordered_set<point64> visited;
    std::unordered_map<point64, point64> prev;
    std::unordered_map<point64, int64_t> start_score;
    std::unordered_map<point64, int64_t> node_score;
    std::vector<point64> unvisited;
    std::unordered_map<point64, equipment> equips;

    constexpr int64_t INF = std::numeric_limits<int64_t>::max();

    auto heuristic = [&](const point64& p) {
        return 7 * (abs(p.x - target.x) + abs(p.y - target.y));
    };

    auto in_bounds = [&](const point64& p) {
        return p.x >= 0 && p.x <= target.x && p.y >= 0 && p.y <= target.y;
    };

    start_score[start] = 0;
    equips[start] = torch;
    node_score[start] = heuristic(start);
    unvisited.push_back(start);

    while (unvisited.size()) {
        std::sort(unvisited.begin(), unvisited.end(), [&](const point64& a, const point64& b) {
            return try_get(node_score, a, INF) < try_get(node_score, b, INF);
        });
        point64 current = unvisited.front();
        unvisited.erase(unvisited.begin());
        visited.insert(current);

        if (current == target) {
            std::vector<point64> points;
            for (;;) {
                points.push_back(current);
                if (auto it = prev.find(current); it != prev.end()) {
                    current = prev[current];
                } else {
                    break;
                }
            }
            for (const auto& p : points) {
                std::cout << p.x << "," << p.y << std::endl;
            }
            std::cin.get();

            return points;
        }

        auto n = neighbors(current);
        for (auto& p : n) {
            if (!in_bounds(p))
                continue;
            if (visited.count(p))
                continue;
            int64_t score = try_get(start_score, p, INF);
            if (score != INF) {
                equipment e_new;
                score += 1 + switch_cost(t[current.to_point()], equips[current], t[p.to_point()], e_new);
                equips[p] = e_new;
            }
            bool is_in_queue = std::find_if(unvisited.begin(), unvisited.end(), [&p](const point64& u) {
                return p == u;
            }) != unvisited.end();
            if (!is_in_queue) {
                unvisited.push_back(p);
            } else if (score >= try_get(start_score, p, INF)) {
                continue;
            }
            prev[p] = current;
            start_score[p] = score;
            node_score[p] = score + heuristic(p);
        }
    }
    throw;
}

enum side { left, top, right, bottom };

side enter_side(point64 from, point64 to) {
    if (to.x == from.x + 1) {
        return left;
    }
    if (to.x == from.x - 1) {
        return right;
    }
    if (to.y == from.y + 1) {
        return top;
    }
    if (to.y == from.y - 1) {
        return bottom;
    }
}

side exit_side(point64 from, point64 to) {
    if (to.x == from.x + 1) {
        return right;
    }
    if (to.x == from.x - 1) {
        return left;
    }
    if (to.y == from.y + 1) {
        return bottom;
    }
    if (to.y == from.y - 1) {
        return top;
    }
}

void shortest_path2(const array2d<terrain_type>& t, point64 from, point64 to) {
    std::unordered_map<point64, std::array<int, 4>> dist;
    std::unordered_map<point64, point64> prev;
    std::vector<point64> q;
    std::unordered_map<point64, std::array<equipment, 4>> equips;

    auto in_bounds = [&](const point64& p) {
        return p.x >= 0 && p.x < t.width() && p.y >= 0 && p.y < t.height();
    };

    q.push_back(from);
    // dist[from] = 0;
    equips[point64{from.x, from.y}][right] = torch;
    equips[point64{from.x, from.y}][bottom] = torch;

    while (q.begin() != q.end()) {
        std::sort(q.begin(), q.end(), [&](const point64& a, const point64& b) {
            return dist[a] < dist[b];
        });
        point64 p = q.front();
        q.erase(q.begin());

        if (p == to) {
            break;
        }

        for (auto d : neighbors(p)) {
            if (!in_bounds(d))
                continue;
            // if (dist.count(d)) continue;

            bool any_undefined = false;
            for (auto& e : equips[point64{d.x, d.y}]) {
                if (e == undefined) {
                    any_undefined = true;
                    break;
                }
            }
            if (!any_undefined)
                continue;

            // evaluated for this?

            prev[d] = p;
            equipment e_new;

            auto src_type = t[p.to_point()];
            auto dst_type = t[d.to_point()];
            auto equip = equips[p][exit_side(p, d)];
            dist[d][exit_side(p, d)] = 1 + switch_cost(src_type, equip, dst_type, e_new);

            for (auto& e : equips[d]) {
                e = e_new;
            }

            // special case for final node
            if (d == to) {
                if (equip != torch) {
                    dist[d][exit_side(p, d)] = 1 + 7;
                }
            }

            q.push_back(d);
        }
    }

    std::cout << "Done " << std::endl;

    int total_dist = 0;
    for (point64 p = to; p != from; p = prev[p]) {
        total_dist += dist[p][exit_side(prev[p], p)];
        std::cout << p.x << "," << p.y << std::endl;
        // std::cin.get();
    }

    // 1219 too high
    std::cout << total_dist << std::endl;
    std::cin.get();

    /*

    for (int i = 0; i < r.paths.size(); ++i) {
      auto& path = r.paths[i];
      path.resize(1 + dist[path.front()]);
      std::swap(path.front(), path.back());
      int j = path.size() - 1;
      point64 init_point = prev[path[path.size() - 1]];
      for (point64 p = init_point; p != from; p = prev[p]) {
        path[--j] = p;
      }
      path[0] = from;
    }
    */
}

int main(int argc, char* argv[]) {
    int64_t depth = 4080;
    int64_t target_x = 14;
    int64_t target_y = 785;

    int64_t padding = 150;

    array2d<terrain_type> terrain(target_x + 1 + padding, target_y + 1 + padding);

    int64_t total_risk = 0;
    for (int64_t y = 0; y <= target_y + padding; ++y) {
        for (int64_t x = 0; x <= target_x + padding; ++x) {
            int64_t geo_index = geologic_index(x, y, target_x, target_y, depth);
            int64_t erosion = erosion_level(geo_index, depth);
            terrain_type ttype = terrain_for_erosion(erosion);
            terrain[point{(int)x, (int)y}] = ttype;
            total_risk += terrain_risk(ttype);
        }
    }

    std::cout << total_risk << std::endl;

    // astar(terrain, point64{0, 0}, point64{target_x, target_y});
    shortest_path2(terrain, point64{0, 0}, point64{target_x, target_y});

    return 0;
}
