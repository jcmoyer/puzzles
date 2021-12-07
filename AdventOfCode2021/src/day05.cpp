#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

template <typename T>
T gcd(T x, T y) {
    int i;
    while (y != 0) {
        i = x % y;
        x = y;
        y = i;
    }
    return x;
}

struct line {
    sr::vec2i p0, p1;

    bool horizontal() const {
        return p0.y() == p1.y();
    }

    bool vertical() const {
        return p0.x() == p1.x();
    }

    sr::vec2i slope() const {
        auto dy = p1.y() - p0.y();
        auto dx = p1.x() - p0.x();
        auto div = gcd(std::abs(dx), std::abs(dy));
        return sr::vec2i{dx / div, dy / div};
    }
};

struct point_count {
    int hv, diag;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::vector<line> lines;
    for (auto& line : sr::lines(input)) {
        auto& l = lines.emplace_back();
        sr::parse(R"((\d+),(\d+) -> (\d+),(\d+))", line, l.p0.x(), l.p0.y(), l.p1.x(), l.p1.y());
    }

    std::unordered_map<sr::vec2i, point_count> pointmap;
    for (auto line : lines) {
        if (line.horizontal() || line.vertical()) {
            for (sr::vec2i p = line.p0; p != line.p1; p += line.slope()) {
                ++pointmap[p].hv;
            }
            ++pointmap[line.p1].hv;
        } else {
            for (sr::vec2i p = line.p0; p != line.p1; p += line.slope()) {
                ++pointmap[p].diag;
            }
            ++pointmap[line.p1].diag;
        }
    }

    sr::solution(std::count_if(pointmap.begin(), pointmap.end(), [](auto kvp) {
        return kvp.second.hv >= 2;
    }));
    sr::solution(std::count_if(pointmap.begin(), pointmap.end(), [](auto kvp) {
        return kvp.second.hv + kvp.second.diag >= 2;
    }));

    return 0;
}
