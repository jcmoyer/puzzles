#include <aoc/intcode.hpp>
#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <sstream>
#include <stdexcept>

#include <sr/sr.hpp>

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    std::vector<int64_t> prog;
    std::string number;
    while (std::getline(input, number, ',')) {
        prog.push_back(std::stoll(number));
    }

    intcode_vm v;
    v.set_output_handler([](int64_t val) {
        fmt::print("{}\n", val);
    });

    v.set_program(prog);
    v.push_input(1);
    v.run();

    v.set_program(prog);
    v.push_input(2);
    v.run();

    return 0;
}
