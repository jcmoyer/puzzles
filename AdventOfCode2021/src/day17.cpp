#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct probe {
    sr::vec2i pos{}, vel{};

    void step() {
        pos += vel;
        // drag
        if (vel.x() < 0) {
            ++vel.x();
        } else if (vel.x() > 0) {
            --vel.x();
        }
        // gravity
        vel.y() -= 1;
    }
};

struct rect {
    // assumes +X-right +Y-up; top should be more-positive
    int left, right, bottom, top;

    bool contains(const sr::vec2i& v) const {
        return v.x() >= left && v.y() <= top && v.x() <= right && v.y() >= bottom;
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string line;
    std::getline(input, line);

    rect test_rect{};
    sr::parse(R"(target area: x=(\-?\d+)\.\.(\-?\d+), y=(\-?\d+)\.\.(\-?\d+))",
        line,
        test_rect.left,
        test_rect.right,
        test_rect.bottom,
        test_rect.top);

    int all_max_y = 0;
    int vels = 0;
    for (int search_y = -500; search_y < 500; ++search_y) {
        for (int search_x = -1; search_x < test_rect.right + 10; ++search_x) {
            probe p;
            p.vel = {(int)search_x, (int)search_y};
            int max_y = 0;        
            int steps = 0;
            while (true) {
                ++steps;
                p.step();
                max_y = std::max(max_y, p.pos.y());
                if (test_rect.contains(p.pos)) {
                    all_max_y = std::max(max_y, all_max_y);
                    ++vels;
                    break;
                }
                if ((p.vel.x() == 0 && p.pos.y() < test_rect.bottom) || (p.pos.x() > test_rect.right))
                    break;
            }
        }
    }

    sr::solution(all_max_y);
    sr::solution(vels);

    return 0;
}
