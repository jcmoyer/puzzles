#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

enum class axis {
    x,
    y,
};

struct fold {
    axis a;
    int pos;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::unordered_set<sr::vec2i> dots;
    std::vector<fold> folds;

    for (const auto& line : sr::lines(input)) {
        if (line.size() == 0)
            continue;

        int x, y;
        try {
            sr::parse(R"((\d+),(\d+))", line, x, y);
            dots.insert(sr::vec2i{x, y});
        } catch (const sr::bad_match&) {
            try {
                sr::parse(R"(fold along x=(\d+))", line, x);
                folds.push_back(fold{axis::x, x});
            } catch (const sr::bad_match&) {
                sr::parse(R"(fold along y=(\d+))", line, y);
                folds.push_back(fold{axis::y, y});
            }
        }
    }

    for (size_t i = 0; i < folds.size(); ++i) {
        std::unordered_set<sr::vec2i> new_dots;
        const fold& f = folds[i];
        for (auto& p : dots) {
            if (f.a == axis::y && p.y() > f.pos) {
                new_dots.insert(sr::vec2i{p.x(), f.pos - (p.y() - f.pos)});
            } else if (f.a == axis::x && p.x() > f.pos) {
                new_dots.insert(sr::vec2i{f.pos - (p.x() - f.pos), p.y()});
            } else {
                new_dots.insert(p);
            }
        }
        dots = new_dots;
        if (i == 0) {
            sr::solution(dots.size());
        }
    }

    auto [minx, maxx] = std::ranges::minmax_element(dots, [](auto&& p0, auto&& p1) {
        return p0.x() < p1.x();
    });
    auto [miny, maxy] = std::ranges::minmax_element(dots, [](auto&& p0, auto&& p1) {
        return p0.y() < p1.y();
    });

    for (int y = miny->y(); y <= maxy->y(); ++y) {
        for (int x = minx->x(); x <= maxx->x(); ++x) {
            if (dots.contains(sr::vec2i{x, y})) {
                std::cout << "#";
            } else {
                std::cout << ".";
            }
        }
        std::cout << "\n";
    }

    return 0;
}
