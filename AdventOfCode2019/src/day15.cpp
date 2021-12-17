#include <aoc/intcode.hpp>
#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

enum tile_type { empty, wall, oxygen };

enum direction { none = 0, north = 1, south = 2, west = 3, east = 4 };

using movement_sequence = std::vector<direction>;

struct tile {
    int64_t x;
    int64_t y;
    tile_type type;
};

using tilemap = std::unordered_map<sr::vec2i, tile>;

direction reverse(direction d) {
    switch (d) {
    case north:
        return south;
    case south:
        return north;
    case west:
        return east;
    case east:
        return west;
    }
    throw std::runtime_error("bad direction");
}

direction to_direction(const sr::vec2i& p) {
    if (p == sr::vec2i{0, 1}) {
        return north;
    } else if (p == sr::vec2i{0, -1}) {
        return south;
    } else if (p == sr::vec2i{-1, 0}) {
        return west;
    } else if (p == sr::vec2i{1, 0}) {
        return east;
    } else {
        throw std::runtime_error("cannot convert point to direction");
    }
}

sr::vec2i to_vector(direction d) {
    switch (d) {
    case east:
        return {1, 0};
    case north:
        return {0, 1};
    case west:
        return {-1, 0};
    case south:
        return {0, -1};
    }
    throw std::runtime_error("invalid direction");
}

std::vector<sr::vec2i> get_explorables_into(
    const tilemap& tm, const sr::vec2i& p, std::vector<sr::vec2i>& explorables) {
    // ensure p is in the map
    tm.at(p);

    // clang-format off
    auto up    = p + sr::vec2i{ 0,  1};
    auto down  = p + sr::vec2i{ 0, -1};
    auto left  = p + sr::vec2i{-1,  0};
    auto right = p + sr::vec2i{ 1,  0};
    // clang-format on

    if (!tm.count(up))
        explorables.emplace_back(up);
    if (!tm.count(down))
        explorables.emplace_back(down);
    if (!tm.count(left))
        explorables.emplace_back(left);
    if (!tm.count(right))
        explorables.emplace_back(right);
    return explorables;
}

std::vector<sr::vec2i> get_explorables(const tilemap& tm, const sr::vec2i& p) {
    std::vector<sr::vec2i> explorables;
    explorables.reserve(4);
    get_explorables_into(tm, p, explorables);
    return explorables;
}

std::unordered_set<sr::vec2i> get_all_explorables(const tilemap& tm) {
    std::unordered_set<sr::vec2i> explorables;

    for (const auto& [k, v] : tm) {
        if (v.type == empty) {
            auto here = get_explorables(tm, k);
            explorables.insert(here.begin(), here.end());
        }
    }

    return explorables;
}

bool is_moveable(tile_type t) {
    return t == empty || t == oxygen;
}

std::vector<sr::vec2i> get_moveable_adjacent(const tilemap& tm, const sr::vec2i& p) {
    // ensure p is in the map
    tm.at(p);

    // clang-format off
    auto up    = p + sr::vec2i{ 0,  1};
    auto down  = p + sr::vec2i{ 0, -1};
    auto left  = p + sr::vec2i{-1,  0};
    auto right = p + sr::vec2i{ 1,  0};
    // clang-format on

    std::vector<sr::vec2i> moveables;
    moveables.reserve(4);
    if (auto it = tm.find(up); it != tm.end() && is_moveable(it->second.type))
        moveables.emplace_back(up);

    if (auto it = tm.find(down); it != tm.end() && is_moveable(it->second.type))
        moveables.emplace_back(down);

    if (auto it = tm.find(left); it != tm.end() && is_moveable(it->second.type))
        moveables.emplace_back(left);

    if (auto it = tm.find(right); it != tm.end() && is_moveable(it->second.type))
        moveables.emplace_back(right);
    return moveables;
}

// describes how to get from one place to another
movement_sequence build_movement_sequence(const tilemap& tm, const sr::vec2i& from, const sr::vec2i& to) {
    std::vector<int64_t> moves;

    struct search_state {
        sr::vec2i pos;
        movement_sequence movement;
    };

    std::unordered_set<sr::vec2i> visited;

    std::deque<search_state> frontier;
    frontier.push_back({from, {}});
    while (frontier.size()) {
        auto next = frontier.front();
        frontier.pop_front();

        if (next.pos == to) {
            return next.movement;
        }

        if (visited.count(next.pos))
            continue;

        visited.emplace(next.pos);

        auto moveables = get_moveable_adjacent(tm, next.pos);

        // union with explorable "to"
        auto explorables = get_explorables(tm, next.pos);
        if (auto it = std::find(explorables.begin(), explorables.end(), to); it != explorables.end()) {
            moveables.push_back(*it);
        }

        for (const auto& move : moveables) {
            auto relative = move - next.pos;
            auto seq = next.movement;
            seq.push_back(to_direction(relative));
            frontier.push_back({move, seq});
        }
    }

    throw std::runtime_error("could not get from source to destination");
}

int64_t spread_oxygen(tilemap& tm) {
    tilemap new_tm = tm;
    int64_t changed = 0;
    for (auto& [k, v] : tm) {
        if (v.type == oxygen) {
            // clang-format off
            auto up    = k + sr::vec2i{ 0,  1};
            auto down  = k + sr::vec2i{ 0, -1};
            auto left  = k + sr::vec2i{-1,  0};
            auto right = k + sr::vec2i{ 1,  0};
            // clang-format on

            if (auto it = tm.find(up); it != tm.end() && it->second.type == empty) {
                new_tm[up].type = oxygen;
                ++changed;
            }
            if (auto it = tm.find(down); it != tm.end() && it->second.type == empty) {
                new_tm[down].type = oxygen;
                ++changed;
            }
            if (auto it = tm.find(left); it != tm.end() && it->second.type == empty) {
                new_tm[left].type = oxygen;
                ++changed;
            }
            if (auto it = tm.find(right); it != tm.end() && it->second.type == empty) {
                new_tm[right].type = oxygen;
                ++changed;
            }
        }
    }
    tm = new_tm;
    return changed;
}

class robot_control {
public:
    robot_control(intcode_program prog);

    void run();

    void mark_tile(const sr::vec2i& where, tile_type ty);

    void explore(const movement_sequence& s);

    tilemap tm;

    int64_t distance_to_oxygen() const;

private:
    void on_vm_output(int64_t message);

    sr::vec2i current_pos{};
    sr::vec2i oxygen_pos{};
    intcode_vm vm;
    direction last_direction = none;
};

robot_control::robot_control(intcode_program prog) {
    vm.set_program(std::move(prog));
}

void robot_control::run() {
    mark_tile(current_pos, empty);
    // std::deque<sr::vec2i> frontier;
    // frontier.push_back(current_pos);

    auto explorables = get_all_explorables(tm);

    while (explorables.size()) {
        for (auto& e : explorables) {
            explore(build_movement_sequence(tm, current_pos, e));
        }

        explorables = get_all_explorables(tm);
    }
}

void robot_control::explore(const movement_sequence& s) {
    size_t move_id = 0;
    vm.set_output_handler([&](int64_t msg) {
        switch (msg) {
        case 0:
            if (move_id < s.size() - 1)
                throw std::runtime_error("unexpected wall");
            else
                mark_tile(current_pos + to_vector(s[move_id]), wall);
            break;
        case 1:

            mark_tile(current_pos + to_vector(s[move_id]), empty);
            current_pos = current_pos + to_vector(s[move_id]);
            break;
        case 2:
            if (move_id < s.size() - 1)
                throw std::runtime_error("unexpected oxygen");
            else {
                mark_tile(current_pos + to_vector(s[move_id]), oxygen);
                oxygen_pos = current_pos + to_vector(s[move_id]);
                current_pos = current_pos + to_vector(s[move_id]);
            }
            break;
        }
    });

    // run the inputs in lockstep
    while (move_id < s.size()) {
        vm.push_input(static_cast<int64_t>(s[move_id]));
        vm.run();
        ++move_id;
    }
}

void robot_control::mark_tile(const sr::vec2i& where, tile_type ty) {
    tile t{where.x(), where.y(), ty};
    tm[where] = t;
}

void robot_control::on_vm_output(int64_t message) {
    switch (message) {
    case 0:
        break;
    case 1:
        break;
    case 2:
        break;
    }
}

int64_t robot_control::distance_to_oxygen() const {
    return build_movement_sequence(tm, sr::vec2i{}, oxygen_pos).size();
}

// for debugging purposes
void render_map(const tilemap& m) {
    auto [mi, ma] = std::minmax_element(m.begin(), m.end(), [](const auto& x, const auto& y) {
        return x.first < y.first;
    });

    sr::vec2i minp = mi->first;
    sr::vec2i maxp = ma->first;
    for (int y = maxp.y(); y >= minp.y(); --y) {
        for (int x = minp.x(); x <= maxp.x(); ++x) {
            if (auto it = m.find(sr::vec2i{x, y}); it != m.end()) {
                if (it->second.type == oxygen) {
                    fmt::print("@");
                } else if (it->first == sr::vec2i{}) {
                    fmt::print(">");
                } else {
                    fmt::print(it->second.type == wall ? "#" : ".");
                }

            } else {
                fmt::print(".");
            }
        }
        fmt::print("\n");
    }
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    robot_control r(prog);
    r.run();

    // part 1
    fmt::print("{}\n", r.distance_to_oxygen());

    // part 2
    int64_t minutes = 0;
    while (spread_oxygen(r.tm) > 0) {
        ++minutes;
    }
    fmt::print("{}\n", minutes);

    return 0;
}
