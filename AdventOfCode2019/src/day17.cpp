#include <aoc/intcode.hpp>
#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

void write_string(intcode_vm& vm, const std::string& s) {
    for (char ch : s) {
        vm.push_input(ch);
    }
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    std::unordered_map<sr::point, char> map;

    intcode_vm vm;
    vm.set_program(prog);

    int x = 0;
    int y = 0;
    vm.set_output_handler([&](int64_t val) {
        fmt::print("{}", (char)val);

        char ch = (char)val;

        if (val == 10) {
            x = 0;
            ++y;
        } else {
            map[sr::point{x, y}] = ch;
            ++x;
        }
    });
    vm.run();

    int sum = 0;
    for (auto& [k, v] : map) {
        // clang-format off
        auto up    = k + sr::point{ 0,  1};
        auto down  = k + sr::point{ 0, -1};
        auto left  = k + sr::point{-1,  0};
        auto right = k + sr::point{ 1,  0};
        // clang-format on
        auto up_i = map.find(up);
        auto down_i = map.find(down);
        auto left_i = map.find(left);
        auto right_i = map.find(right);
        if (up_i != map.end() && down_i != map.end() && left_i != map.end() && right_i != map.end()) {
            if (up_i->second == '#' && down_i->second == '#' && left_i->second == '#' && right_i->second == '#') {
                sum += k.x * k.y;
            }
        }
    }

    vm.set_program(prog);
    vm.write_memory(0, 2);
    write_string(vm, "A,B,A,C,B,A,C,B,A,C\n");
    write_string(vm, "L,6,L,4,R,12\n");
    write_string(vm, "L,6,R,12,R,12,L,8\n");
    write_string(vm, "L,6,L,10,L,10,L,6\n");
    write_string(vm, "n\n");
    vm.set_output_handler([&](int64_t val) {
        fmt::print("{}\n", val);
    });

    vm.run();

    return 0;
}
