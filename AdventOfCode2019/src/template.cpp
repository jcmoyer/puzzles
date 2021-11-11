#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    for (const auto& line : sr::lines(input)) {
        fmt::print("{}\n", line);
    }
    // clang-format off
  auto inp = "";
    // clang-format on

    return 0;
}
