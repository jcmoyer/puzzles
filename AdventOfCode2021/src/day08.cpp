#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

namespace ranges = std::ranges;

bool is_unique_size(size_t n) {
    return n == 2 || n == 3 || n == 4 || n == 7;
}

struct entry {
    std::vector<std::string> signals;
    std::vector<std::string> outputs;

    int count_unique_outputs() const {
        return ranges::count_if(outputs, [](auto&& s) {
            return is_unique_size(s.size());
        });
    }

    int solve_output() {
        // sort so we can easily search subsequences and compare out-of-order characters
        for (auto& sig : signals) {
            ranges::sort(sig);
        }
        for (auto& output : outputs) {
            ranges::sort(output);
        }

        std::unordered_map<int, std::string> segment_assoc;
        // 1
        auto one = ranges::find_if(signals, [](auto&& sig) {
            return sig.size() == 2;
        });
        segment_assoc[1] = *one;
        // 4
        auto four = ranges::find_if(signals, [](auto&& sig) {
            return sig.size() == 4;
        });
        segment_assoc[4] = *four;
        // 7
        auto seven = ranges::find_if(signals, [](auto&& sig) {
            return sig.size() == 3;
        });
        segment_assoc[7] = *seven;
        // 8
        auto eight = ranges::find_if(signals, [](auto&& sig) {
            return sig.size() == 7;
        });
        segment_assoc[8] = *eight;

        std::deque<std::string> unconsidered;
        ranges::copy_if(signals, std::back_inserter(unconsidered), [](auto&& sig) {
            return sig.size() != 2 && sig.size() != 4 && sig.size() != 3 && sig.size() != 7;
        });

        while (unconsidered.size()) {
            std::string sig = *unconsidered.begin();
            unconsidered.erase(unconsidered.begin());

            if (sig.size() == 6) {
                // 0, 6, 9

                // 0 = 1, not 4, 7, not 8
                // 6 = not 1, not 4, not 7, not 8
                // 9 = 1, 4, 7, not 8

                if (ranges::includes(sig, segment_assoc[1]) && !ranges::includes(sig, segment_assoc[4]) &&
                    ranges::includes(sig, segment_assoc[7]) && !ranges::includes(sig, segment_assoc[8])) {
                    segment_assoc[0] = sig;
                } else if (!ranges::includes(sig, segment_assoc[1]) && !ranges::includes(sig, segment_assoc[4]) &&
                           !ranges::includes(sig, segment_assoc[7]) && !ranges::includes(sig, segment_assoc[8])) {
                    segment_assoc[6] = sig;
                } else if (ranges::includes(sig, segment_assoc[1]) && ranges::includes(sig, segment_assoc[4]) &&
                           ranges::includes(sig, segment_assoc[7]) && !ranges::includes(sig, segment_assoc[8])) {
                    segment_assoc[9] = sig;
                }
            } else if (sig.size() == 5) {
                // 2, 3, 5

                // 2 = not 1, not 4, not 7, not 8
                // 3 = 1, not 4, 7, not 8
                // 5 = not 1, not 4, not 7, not 8
                // 2 and 5 differ by 4, 5 contains 3 of 4's outputs, 2 contains 2 of 4's outputs

                if (ranges::includes(sig, segment_assoc[1]) && !ranges::includes(sig, segment_assoc[4]) &&
                    ranges::includes(sig, segment_assoc[7]) && !ranges::includes(sig, segment_assoc[8])) {
                    segment_assoc[3] = sig;
                } else if (!ranges::includes(sig, segment_assoc[1]) && !ranges::includes(sig, segment_assoc[4]) &&
                           !ranges::includes(sig, segment_assoc[7]) && !ranges::includes(sig, segment_assoc[8])) {
                    int four_common = ranges::count_if(sig, [&](auto ch) {
                        return ranges::find(segment_assoc[4], ch) != segment_assoc[4].end();
                    });

                    if (four_common == 3) {
                        segment_assoc[5] = sig;
                    } else if (four_common == 2) {
                        segment_assoc[2] = sig;
                    } else {
                        throw std::runtime_error("unsolvable");
                    }
                }
            }
        }

        std::string num;
        for (auto& output : outputs) {
            for (auto& [digit, signal] : segment_assoc) {
                if (signal == output) {
                    num += '0' + digit;
                }
            }
        }

        return std::stoi(num);
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::vector<entry> entries;

    for (auto line : sr::lines(input)) {
        std::string_view left, right;
        sr::parse(R"(^([\w ]+) \| ([\w ]+)$)", line, left, right);

        entry& e = entries.emplace_back();

        sr::split(left, ' ', [&](auto first, auto last) {
            e.signals.emplace_back(first, last);
        });

        sr::split(right, ' ', [&](auto first, auto last) {
            e.outputs.emplace_back(first, last);
        });
    }

    int sum1 = 0;
    int sum2 = 0;
    for (auto e : entries) {
        sum1 += e.count_unique_outputs();
        sum2 += e.solve_output();
    }

    sr::solution(sum1);
    sr::solution(sum2);

    return 0;
}
