#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

constexpr int fuel_1(int mass) {
    return (mass / 3) - 2;
}

constexpr int fuel_2(int mass) {
    int m = fuel_1(mass);
    int sum = 0;
    while (m > 0) {
        sum += m;
        m = fuel_1(m);
    }
    return sum;
}

template <typename Stream, typename FuelFunction>
int solve(Stream input, FuelFunction f) {
    int sum = 0;
    for (const auto& line : sr::lines(input)) {
        sum += f(std::stoi(line));
    }
    return sum;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    fmt::print("{}\n", solve(std::ifstream(filename), fuel_1));
    fmt::print("{}\n", solve(std::ifstream(filename), fuel_2));
    return 0;
}
