#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::vector<int> entries;

    for (const auto& line : sr::lines(input)) {
    }

    for (auto& e : entries) {
    }

    sr::solution(0);

    return 0;
}
