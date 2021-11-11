#include <aoc/intcode.hpp>
#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <sstream>
#include <stdexcept>

#include <sr/sr.hpp>

int64_t run_program_nv(const intcode_program& prog, int noun, int verb) {
    intcode_vm v;
    v.set_program(prog);
    v.write_memory(1, noun);
    v.write_memory(2, verb);
    v.run();
    return v.read_memory(0);
}

int64_t solve_program_nv(const intcode_program& prog, int result) {
    for (int noun = 0; noun <= 99; ++noun) {
        for (int verb = 0; verb <= 99; ++verb) {
            if (run_program_nv(prog, noun, verb) == result) {
                return 100 * noun + verb;
            }
        }
    }
    throw std::runtime_error("could not solve noun-verb for result");
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    // part 1
    fmt::print("{}\n", run_program_nv(prog, 12, 2));

    // part 2
    fmt::print("{}\n", solve_program_nv(prog, 19690720));

    return 0;
}
