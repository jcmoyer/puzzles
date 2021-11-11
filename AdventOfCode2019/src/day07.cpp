#include <aoc/intcode.hpp>
#include <array>
#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

using phase_array = std::array<int, 5>;

template <typename Iterator>
bool all_halted(Iterator first, Iterator last) {
    return std::all_of(first, last, [](const intcode_vm& vm) {
        return vm.is_halted();
    });
}

int64_t solve_max_signal(const intcode_program& prog, phase_array phase, bool feedback_loop) {
    std::vector<int64_t> results;

    do {
        std::array<intcode_vm, 5> amps;
        for (int i = 0; i < amps.size(); ++i) {
            amps[i].set_program(prog);
            amps[i].push_input(phase[i]);

            // link this amp output to the next amp input
            if (i + 1 < amps.size()) {
                amps[i].set_output_handler([&, i](int64_t val) {
                    amps[i + 1].push_input(val);
                });
            } else {
                amps[i].set_output_handler([&, i](int64_t val) {
                    if (feedback_loop) {
                        amps[0].push_input(val);
                    }
                    results.push_back(val);
                });
            }
        }

        // first amp input is always zero
        amps[0].push_input(0);

        while (!all_halted(std::begin(amps), std::end(amps))) {
            for (intcode_vm& amp : amps) {
                amp.run();
            }
        }
    } while (std::next_permutation(phase.begin(), phase.end()));

    return *std::max_element(results.begin(), results.end());
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    fmt::print("{}\n", solve_max_signal(prog, {0, 1, 2, 3, 4}, false));
    fmt::print("{}\n", solve_max_signal(prog, {5, 6, 7, 8, 9}, true));

    return 0;
}
