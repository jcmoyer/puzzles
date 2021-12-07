#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct worldstate {
    uint64_t counts[9]{};

    uint64_t total() const {
        return std::accumulate(std::begin(counts), std::end(counts), 0ull);
    }

    uint64_t run(size_t iters) {
        for (size_t i = 0; i < iters; ++i) {
            uint64_t new_fish = counts[0];
            std::rotate(std::begin(counts), std::begin(counts) + 1, std::end(counts));
            counts[6] += new_fish;
        }
        return total();
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::string line;
    std::getline(input, line);

    worldstate init_state{};

    sr::split(line, ',', [&](auto first, auto last) {
        int id = std::stoi(std::string(first, last));
        init_state.counts[id] += 1;
    });

    sr::solution(worldstate(init_state).run(80));
    sr::solution(worldstate(init_state).run(256));

    return 0;
}
