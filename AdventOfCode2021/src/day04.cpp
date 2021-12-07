#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct board_cell {
    int val = 0;
    bool marked = false;
};

struct win_state {
    bool won = false;
    int nums[5]{};
};

using board = sr::array2d<board_cell>;

win_state win(const board& b) {
    win_state ws{};

    for (size_t y = 0; y < 5; ++y) {
        bool all = true;
        for (size_t x = 0; x < 5; ++x) {
            ws.nums[x] = b.at(x, y).val;
            if (!b.at(x, y).marked)
                all = false;
        }
        if (all) {
            ws.won = true;
            return ws;
        }
    }

    for (size_t x = 0; x < 5; ++x) {
        bool all = true;
        for (size_t y = 0; y < 5; ++y) {
            ws.nums[y] = b.at(x, y).val;
            if (!b.at(x, y).marked)
                all = false;
        }
        if (all) {
            ws.won = true;
            return ws;
        }
    }

    return ws;
}

void mark_num(board& b, int n) {
    for (size_t y = 0; y < 5; ++y) {
        for (size_t x = 0; x < 5; ++x) {
            if (b.at(x, y).val == n) {
                b.at(x, y).marked = true;
                return;
            }
        }
    }
}

int sum_unmarked(const board& b) {
    int sum = 0;
    for (size_t y = 0; y < 5; ++y) {
        for (size_t x = 0; x < 5; ++x) {
            if (!b.at(x, y).marked) {
                sum += b.at(x, y).val;
            }
        }
    }
    return sum;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::string numbers_str;
    std::getline(input, numbers_str);

    std::vector<sr::array2d<board_cell>> boards;

    while (input) {
        auto& b = boards.emplace_back();
        b.resize(5, 5);
        for (int y = 0; y < 5; ++y) {
            for (int x = 0; x < 5; ++x) {
                input >> b.at(x, y).val;
            }
        }
    }

    std::vector<int> numbers;

    sr::split(numbers_str, ',', [&](auto first, auto last) {
        int n = std::stoi(std::string(first, last));
        numbers.push_back(n);
    });

    size_t score_first_win = -1;
    size_t id_last_win = -1;
    size_t score_last_win = 0;
    std::unordered_set<size_t> id_won;

    for (int n : numbers) {
        for (size_t i = 0; i < boards.size(); ++i) {
            mark_num(boards[i], n);
            auto ws = win(boards[i]);
            if (ws.won) {
                if (score_first_win == -1) {
                    score_first_win = sum_unmarked(boards[i]) * n;
                }
                if (!id_won.count(i)) {
                    id_won.emplace(i);
                    id_last_win = i;
                    score_last_win = sum_unmarked(boards[i]) * n;
                }
            }
        }
    }

    sr::solution(score_first_win);
    sr::solution(score_last_win);

    return 0;
}
