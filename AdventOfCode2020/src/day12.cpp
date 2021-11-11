#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct action {
    char n;
    int param;
};

namespace sr {
template <>
struct parse_type<action> {
    static action parse(const char* first, const char* last) {
        if (first == last || first + 1 == last)
            throw bad_format{};
        action act;
        act.n = parse_type<char>::parse(first, first + 1);
        act.param = parse_type<int>::parse(first + 1, last);
        return act;
    }
};
}

struct ship {
    sr::direction dir = sr::east;
    sr::vec2i pos{};
};

struct waypoint {
    sr::vec2i pos{10, 1};
};

int part1(const std::vector<action>& actions) {
    ship s;
    for (const auto& a : actions) {
        switch (a.n) {
        // clang-format off
        case 'N': s.pos.y() += a.param; break;
        case 'S': s.pos.y() -= a.param; break;
        case 'W': s.pos.x() -= a.param; break;
        case 'E': s.pos.x() += a.param; break;
        case 'L': s.dir = sr::rotate_ccw(s.dir, a.param / 90); break;
        case 'R': s.dir = sr::rotate_cw(s.dir, a.param / 90); break;
        case 'F': sr::displace(s.pos, s.dir, a.param); break;
        default: throw std::runtime_error("unexpected action");
            // clang-format on
        }
    }
    return sr::manhattan(s.pos, {});
}

int part2(const std::vector<action>& actions) {
    ship sh;
    waypoint wp;
    for (const auto& a : actions) {
        switch (a.n) {
        // clang-format off
        case 'N': wp.pos.y() += a.param; break;
        case 'S': wp.pos.y() -= a.param; break;
        case 'W': wp.pos.x() -= a.param; break;
        case 'E': wp.pos.x() += a.param; break;
        case 'L': sr::integral_rotate(wp.pos, a.param); break;
        case 'R': sr::integral_rotate(wp.pos, -a.param); break;
        case 'F': sh.pos += wp.pos * a.param; break;
        default: throw std::runtime_error("unexpected action");
        // clang-format off
        }
    }
    return sr::manhattan(sh.pos, {});
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);
    std::vector<action> ent = sr::read_lines<action>(input);

    sr::solution(part1(ent));
    sr::solution(part2(ent));

    return 0;
}
