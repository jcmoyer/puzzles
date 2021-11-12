#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<int> ent;
    for (const auto& line : sr::lines(input)) {
        ent.push_back(std::stoi(line));
    }

    return 0;
}
