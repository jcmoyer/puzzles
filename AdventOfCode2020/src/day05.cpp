#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct range {
    int low, high;

    int dist() const {
        return high - low;
    }

    range take_low() const {
        return {low, low + dist() / 2};
    }

    range take_high() const {
        return {low + 1 + dist() / 2, high};
    }

    bool singular() const {
        return low == high;
    }
};

struct address {
    int row, col;

    int seat_id() const {
        return row * 8 + col;
    };
};

address parse_bsp(std::string_view str) {
    range rows = {0, 127};
    range cols = {0, 7};
    for (char ch : str) {
        switch (ch) {
        case 'F':
            rows = rows.take_low();
            break;
        case 'B':
            rows = rows.take_high();
            break;
        case 'L':
            cols = cols.take_low();
            break;
        case 'R':
            cols = cols.take_high();
            break;
        }
    }
    if (!rows.singular() || !cols.singular()) {
        throw std::runtime_error("ambiguous result");
    }
    return {rows.low, cols.low};
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<address> ent;
    for (const auto& line : sr::lines(input)) {
        ent.push_back(parse_bsp(line));
    }

    auto it = std::max_element(ent.begin(), ent.end(), [](auto&& addr1, auto&& addr2) {
        return addr1.seat_id() < addr2.seat_id();
    });
    sr::solution(it->seat_id());

    std::vector<int> seats;
    for (auto&& a : ent) {
        seats.push_back(a.seat_id());
    }
    std::sort(seats.begin(), seats.end());

    int last = seats[0];
    for (size_t i = 1; i < seats.size(); ++i) {
        if (seats[i] != last + 1 && seats[i - 1] == seats[i] - 2) {
            sr::solution(seats[i] - 1);
            break;
        }
        last = seats[i];
    }

    return 0;
}
