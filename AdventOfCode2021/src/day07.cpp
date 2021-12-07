#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int dist_p1(int x, int y) {
    return std::abs(x - y);
}

// https://en.wikipedia.org/wiki/Triangular_number
int dist_p2(int x, int y) {
    int d = std::abs(x - y);
    return d * (d + 1) / 2;
}

template <typename FuelFunc>
int solve(const std::vector<int>& crabs, FuelFunc f) {
    auto [min, max] = std::minmax_element(crabs.begin(), crabs.end());

    std::vector<int> shift_fuel;
    shift_fuel.reserve(max - min);

    for (int i = *min; i <= *max; ++i) {
        int fuel_sum = std::reduce(crabs.begin(), crabs.end(), 0, [=](int s, int x) {
            return s + f(x, i);
        });
        shift_fuel.push_back(fuel_sum);
    }

    return *std::min_element(shift_fuel.begin(), shift_fuel.end());
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::string line;
    std::getline(input, line);

    std::vector<int> crabs;
    sr::split(line, ',', [&](auto first, auto last) {
        int d = std::stoi(std::string(first, last));
        crabs.push_back(d);
    });

    sr::solution(solve(crabs, dist_p1));
    sr::solution(solve(crabs, dist_p2));

    return 0;
}
