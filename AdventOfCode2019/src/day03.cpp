#include <algorithm>
#include <fmt/format.h>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include <sr/sr.hpp>

sr::direction parse_dir(char ch) {
    switch (ch) {
    case 'R':
        return sr::east;
    case 'U':
        return sr::north;
    case 'L':
        return sr::west;
    case 'D':
        return sr::south;
    }
    throw std::runtime_error("invalid direction character");
}

struct wire_run {
    sr::direction dir;
    int dist;
};

struct wiremap_cell {
    int steps1 = 0;
    int steps2 = 0;

    int total_steps() const {
        return steps1 + steps2;
    }
};

void parse_series(std::string& wiredata, std::vector<wire_run>& series) {
    std::stringstream ss(wiredata);
    std::string motion;
    while (std::getline(ss, motion, ',')) {
        char direction;
        int distance;
        sr::parse("([RULD])(\\d+)", motion, direction, distance);
        series.push_back(wire_run{parse_dir(direction), distance});
    }
}

template <typename F>
void walk_series(const std::vector<wire_run>& series, F func) {
    sr::vec2i cursor{};
    int steps = 0;
    for (const auto& run : series) {
        sr::vec2i delta = sr::to_unit_vector<int>(run.dir);
        for (int i = 0; i < run.dist; ++i) {
            ++steps;
            cursor += delta;
            func(cursor, steps);
        }
    }
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);

    std::string wiredata1, wiredata2;
    std::getline(input, wiredata1);
    std::getline(input, wiredata2);

    std::vector<wire_run> wire1run, wire2run;

    parse_series(wiredata1, wire1run);
    parse_series(wiredata2, wire2run);

    std::unordered_map<sr::vec2i, wiremap_cell> wire_map;
    wire_map.reserve(150000);

    std::vector<sr::vec2i> intersections;
    intersections.reserve(100);

    // first we mark all points that wire 1 runs through
    walk_series(wire1run, [&](const sr::vec2i& point, int steps) {
        wire_map.try_emplace(point, wiremap_cell{steps});
    });

    // then we walk through wire 2 and examine each point, checking to see if wire 1 ran through it
    // note that this does not paint wire 2 onto the wiremap
    walk_series(wire2run, [&](const sr::vec2i& point, int steps) {
        auto w1point = wire_map.find(point);
        if (w1point == wire_map.end()) {
            return;
        }
        // wire 1 is here; record an intersection and the number of steps
        intersections.push_back(point);
        w1point->second.steps2 = steps;
    });

    // part 1: locate the intersection closest to origin
    constexpr sr::vec2i origin{};
    auto it1 = std::min_element(intersections.begin(), intersections.end(), [&](const auto& x, const auto& y) {
        return sr::manhattan(origin, x) < sr::manhattan(origin, y);
    });
    fmt::print("{}\n", sr::manhattan(origin, *it1));

    // part 2: locate the intersection closest to origin by number of steps
    auto it2 = std::min_element(intersections.begin(), intersections.end(), [&](const auto& x, const auto& y) {
        return wire_map[x].total_steps() < wire_map[y].total_steps();
    });
    fmt::print("{}\n", wire_map[*it2].total_steps());

    return 0;
}
