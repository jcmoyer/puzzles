#include <aoc/intcode.hpp>
#include <fmt/format.h>
#include <fstream>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

enum color { black = 0, white = 1 };
enum rotation { ccw = 0, cw = 1 };

using paintmap = std::unordered_map<sr::vec2i, color>;

template <typename Integer>
constexpr color color_cast(Integer val) {
    switch (val) {
    case black:
        return black;
    case white:
        return white;
    }
    throw std::runtime_error("bad color value");
}

template <typename Integer>
constexpr rotation rotation_cast(Integer val) {
    switch (val) {
    case ccw:
        return ccw;
    case cw:
        return cw;
    }
    throw std::runtime_error("bad direction value");
}

class robot {
public:
    robot(intcode_program prog, paintmap& map_);

    void run(color initial_color);

private:
    void on_vm_output(int64_t message);
    void do_recv_color(color c);
    void do_recv_rotation(rotation rot);

    enum recvstate { recv_color, recv_rotation };

    paintmap& map;
    intcode_vm vm;
    sr::vec2i pos{};
    sr::direction facing = sr::north;
    recvstate state = recv_color;
};

robot::robot(intcode_program prog, paintmap& map_) : map{map_} {
    vm.set_program(std::move(prog));
    vm.set_output_handler([this](int64_t val) {
        on_vm_output(val);
    });
}

void robot::run(color initial_color) {
    vm.push_input(initial_color);

    while (!vm.is_halted()) {
        vm.run();

        if (auto it = map.find(pos); it != map.end()) {
            vm.push_input(it->second);
        } else {
            vm.push_input(black);
        }
    }
}

void robot::on_vm_output(int64_t message) {
    switch (state) {
    case recv_color:
        do_recv_color(color_cast(message));
        break;
    case recv_rotation:
        do_recv_rotation(rotation_cast(message));
        break;
    }
}

void robot::do_recv_color(color c) {
    map[pos] = c;
    state = recv_rotation;
}

void robot::do_recv_rotation(rotation rot) {
    facing = rot == ccw ? sr::rotate_ccw(facing) : sr::rotate_cw(facing);
    sr::displace(pos, facing, 1);
    state = recv_color;
}

void render_map(const paintmap& m) {
    auto [miny, maxy] = std::minmax_element(m.begin(), m.end(), [](const auto& x, const auto& y) {
        return x.first.y() < y.first.y();
    });
    auto [minx, maxx] = std::minmax_element(m.begin(), m.end(), [](const auto& x, const auto& y) {
        return x.first.x() < y.first.x();
    });

    sr::vec2i minp{minx->first.x(), miny->first.y()};
    sr::vec2i maxp{maxx->first.x(), maxy->first.y()};
    for (int y = maxp.y(); y >= minp.y(); --y) {
        for (int x = minp.x(); x <= maxp.x(); ++x) {
            if (auto it = m.find(sr::vec2i{x, y}); it != m.end()) {
                fmt::print(it->second == white ? "#" : ".");
            } else {
                fmt::print(".");
            }
        }
        fmt::print("\n");
    }
}

paintmap run_robot(intcode_program prog, color initial_color) {
    paintmap m;
    robot r(std::move(prog), m);
    r.run(initial_color);
    return m;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    // part 1
    fmt::print("{}\n", run_robot(prog, black).size());
    // part 2
    render_map(run_robot(prog, white));

    return 0;
}
