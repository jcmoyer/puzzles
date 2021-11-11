#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <thread>

#include <sr/sr.hpp>

struct bus {
    int64_t id, offset;
    bus(int64_t id_, int64_t offset_) : id{id_}, offset{offset_} {}
};

struct puzzle_input {
    int64_t earliest;
    std::vector<bus> buses;
};

// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
int64_t inverse(int64_t a, int64_t n) {
    int64_t t = 0, new_t = 1;
    int64_t r = n, new_r = a;

    while (new_r != 0) {
        int64_t q = r / new_r;

        int64_t old_t = t;
        t = new_t;
        new_t = old_t - q * new_t;

        int64_t old_r = r;
        r = new_r;
        new_r = old_r - q * new_r;
    }

    if (r > 1)
        throw std::runtime_error("a not invertible");

    if (t < 0)
        t = t + n;

    return t;
}

int64_t part1(int64_t earliest, const std::vector<bus>& buses) {
    struct bus_comp_time {
        int64_t bus_id;
        int64_t near_time;
    };

    std::vector<bus_comp_time> nearest_times;
    for (const auto& b : buses) {
        int est_wait = (earliest / b.id) * b.id;
        if (est_wait < earliest) {
            est_wait += b.id;
        }
        nearest_times.push_back({b.id, est_wait});
    }

    auto& b_ent = *std::min_element(nearest_times.begin(), nearest_times.end(), [](auto&& a, auto&& b) {
        return a.near_time < b.near_time;
    });

    return b_ent.bus_id * (b_ent.near_time - earliest);
}

int64_t part2(const std::vector<bus>& buses) {
    int64_t n = std::accumulate(buses.begin(), buses.end(), 1LL, [](int64_t p, auto&& bus) {
        return p * bus.id;
    });

    int64_t x = std::accumulate(buses.begin(), buses.end(), 0LL, [&](int64_t s, auto&& bus) {
        return s + bus.offset * (n / bus.id) * inverse(n / bus.id, bus.id);
    });

    return n - x % n;
}

puzzle_input read_puzzle_input(std::istream& input) {
    puzzle_input result;
    std::string buf;

    std::getline(input, buf);
    result.earliest = std::stoll(buf);

    int64_t offset = 0;
    while (std::getline(input, buf, ',')) {
        if (buf != "x") {
            result.buses.emplace_back(std::stoi(buf), offset);
        }
        ++offset;
    }

    return result;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream stream(args.input_filename);
    puzzle_input input = read_puzzle_input(stream);

    sr::solution(part1(input.earliest, input.buses));
    sr::solution(part2(input.buses));

    return 0;
}
