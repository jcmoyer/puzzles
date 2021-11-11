#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <ranges>
#include <unordered_map>

#include <sr/sr.hpp>

struct password_ent {
    std::string password;
    int min_ch;
    int max_ch;
    char ch;

    bool valid() const {
        std::unordered_map<char, int> occurrences;
        for (char c : password) {
            ++occurrences[c];
        }
        int cnt = occurrences[ch];
        return cnt >= min_ch && cnt <= max_ch;
    }

    bool valid2() const {
        return (password.at(min_ch - 1) == ch) ^ (password.at(max_ch - 1) == ch);
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<password_ent> ent;
    for (const auto& line : sr::lines(input)) {
        auto& e = ent.emplace_back();
        sr::parse(R"((\d+)\-(\d+) (\w): (\w+))", line, e.min_ch, e.max_ch, e.ch, e.password);
    }

    sr::solution(std::ranges::count_if(ent, [](const auto& e) {
        return e.valid();
    }));

    sr::solution(std::ranges::count_if(ent, [](const auto& e) {
        return e.valid2();
    }));

    return 0;
}
