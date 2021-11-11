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

    for (size_t i = 0; i < ent.size(); ++i) {
        for (size_t j = i + 1; j < ent.size(); ++j) {
            if (ent[i] + ent[j] == 2020) {
                fmt::print("{}\n", ent[i] * ent[j]);
            }
        }
    }

    for (size_t i = 0; i < ent.size(); ++i) {
        for (size_t j = i + 1; j < ent.size(); ++j) {
            for (size_t k = j + 1; k < ent.size(); ++k) {
                if (ent[i] + ent[j] + ent[k] == 2020) {
                    fmt::print("{}\n", ent[i] * ent[j] * ent[k]);
                }
            }
        }
    }

    return 0;
}
