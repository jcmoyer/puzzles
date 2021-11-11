#include <cassert>
#include <fmt/format.h>
#include <forward_list>
#include <fstream>
#include <list>
#include <memory>
#include <numeric>
#include <robin_hood.hpp>

#include <sr/sr.hpp>

template <typename I, typename R>
void wrapping_increment(I& it, R&& range) {
    ++it;
    if (it == std::ranges::end(range))
        it = std::ranges::begin(range);
}

[[nodiscard]] constexpr int parse_digit(char ch) {
    if (ch < '0' || ch > '9')
        throw std::range_error("ch out of range; must be in ['0'..'9']");
    return ch - '0';
}

[[nodiscard]] constexpr char to_digit(int x) {
    if (x < 0 || x > 9)
        throw std::range_error("x out of range; must be in [0..9]");
    return '0' + x;
}

struct game_parameters {
    std::string_view initial_cups;
    int max_cups = -1;
    int iterations;
};

struct game_state {
    std::forward_list<int> cups;
    int lowest, highest;

    void set_cups(std::string_view s, size_t total_cups) {
        cups.resize(total_cups);
        std::forward_list<int>::iterator it = cups.begin();
        int max_int = 0;

        int written = 0;
        for (char ch : s) {
            int x = parse_digit(ch);
            max_int = std::max(x, max_int);
            *it++ = x;
            ++written;
        }
        highest = max_int;

        for (size_t i = max_int + 1; i <= total_cups; ++i) {
            *it++ = i;
            highest = i;
            ++written;
        }
        lowest = 1;

        assert(total_cups == written);
    }

    std::string part1() const {
        auto it = std::find(cups.begin(), cups.end(), 1);
        auto last = it;
        std::string result;
        wrapping_increment(it, cups);
        while (it != last) {
            result.push_back(to_digit(*it));
            wrapping_increment(it, cups);
        }
        return result;
    }

    int64_t part2() const {
        auto it = std::ranges::find(cups, 1);
        wrapping_increment(it, cups);
        int64_t x = *it;
        wrapping_increment(it, cups);
        int64_t y = *it;
        return x * y;
    }
};

game_state play_game(const game_parameters& params) {
    game_state state;
    auto& cups = state.cups;

    state.set_cups(params.initial_cups, params.max_cups > 0 ? params.max_cups : params.initial_cups.size());

    std::vector<std::forward_list<int>::iterator> all_cup_labels(1 + state.highest);

    for (std::forward_list<int>::iterator it = cups.begin(); it != cups.end(); ++it) {
        all_cup_labels[*it] = it;
    }

    // current cup represented by an iterator, which is guaranteed to be valid
    std::forward_list<int>::iterator current = cups.begin();

    // temporary storage for picked-up cups
    std::vector<int> pickup;

    for (int i = 0; i < params.iterations; ++i) {
        pickup.clear();

        for (int j = 0; j < 3; ++j) {
            auto next = std::next(current);

            if (next == cups.end()) {
                pickup.push_back(*cups.begin());
                cups.erase_after(cups.before_begin());
            } else {
                pickup.push_back(*next);
                cups.erase_after(current);
            }
        }

        // find cup to insert after
        int destination_label = *current - 1;
        while (destination_label < state.lowest || destination_label == pickup[0] || destination_label == pickup[1] ||
               destination_label == pickup[2]) {
            if (destination_label < state.lowest) {
                destination_label = state.highest;
            } else {
                --destination_label;
            }
        }

        // insert those cups after dest
        auto insert_it = all_cup_labels[destination_label];

        // put the picked-up cups back into the list and re-map them
        std::forward_list<int>::iterator inserted_it;
        inserted_it = cups.insert_after(insert_it, pickup[2]);
        all_cup_labels[*inserted_it] = inserted_it;
        inserted_it = cups.insert_after(insert_it, pickup[1]);
        all_cup_labels[*inserted_it] = inserted_it;
        inserted_it = cups.insert_after(insert_it, pickup[0]);
        all_cup_labels[*inserted_it] = inserted_it;

        // the following cup from 'current' is always the next current cup
        wrapping_increment(current, cups);
    }

    return state;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string cups_str;
    std::getline(input, cups_str);

    sr::solution(play_game({.initial_cups = cups_str, .iterations = 100}).part1());
    sr::solution(play_game({.initial_cups = cups_str, .max_cups = 1'000'000, .iterations = 10'000'000}).part2());

    return 0;
}
