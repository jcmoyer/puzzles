#include <sr/sr.hpp>

size_t step_ca(sr::array2d<char>& ca, sr::array2d<char>& out) {
    size_t moved = 0;
    for (int y = 0; y < ca.height(); ++y) {
        for (int x = 0; x < ca.width(); ++x) {
            if (ca.at(x, y) == '>') {
                const auto* next = ca.at_opt(x + 1, y);
                int target_x = x + 1;
                if (!next) {
                    next = ca.at_opt(0, y);
                    target_x = 0;
                }
                if (*next == '.') {
                    out.at(x, y) = '.';
                    out.at(target_x, y) = '>';
                    ++moved;
                }
            }
        }
    }
    ca = out;
    for (int y = 0; y < ca.height(); ++y) {
        for (int x = 0; x < ca.width(); ++x) {
            if (ca.at(x, y) == 'v') {
                const auto* next = ca.at_opt(x, y + 1);
                int target_y = y + 1;
                if (!next) {
                    next = ca.at_opt(x, 0);
                    target_y = 0;
                }
                if (*next == '.') {
                    out.at(x, y) = '.';
                    out.at(x, target_y) = 'v';
                    ++moved;
                }
            }
        }
    }
    return moved;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    auto tm = sr::read_tilemap(args.get_input_stream());
    auto out = tm;
    size_t steps = 0;
    while (true) {
        size_t moved = step_ca(tm, out);
        ++steps;
        if (moved == 0) {
            break;
        }
        tm = out;
    }
    sr::solution(steps);
    return 0;
}
