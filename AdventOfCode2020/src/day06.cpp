#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <unordered_set>

#include <sr/sr.hpp>

struct person {
    std::unordered_set<char> answers;

    template <typename I>
    person(I first, I last) : answers(first, last) {}
};

struct group {
    std::vector<person> people;
    std::unordered_set<char> answers;

    void add_answers(std::string_view s) {
        people.emplace_back(s.begin(), s.end());
        answers.insert(s.begin(), s.end());
    }

    int part1() const {
        return (int)answers.size();
    }

    int part2() const {
        return (int)std::count_if(answers.begin(), answers.end(), [&](char c) {
            return std::all_of(people.begin(), people.end(), [&](auto&& ia) {
                return ia.answers.count(c);
            });
        });
    }
};

struct group_list {
    std::vector<group> groups;

    void add_group(group g) {
        groups.push_back(std::move(g));
    }

    int part1() const {
        return std::accumulate(groups.begin(), groups.end(), 0, [](auto&& state, auto&& x) {
            return state + x.part1();
        });
    }

    int part2() const {
        return std::accumulate(groups.begin(), groups.end(), 0, [](auto&& state, auto&& x) {
            return state + x.part2();
        });
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    group_list groups;
    group current_group;

    for (const auto& line : sr::lines(input)) {
        if (line.size() == 0) {
            groups.add_group(std::move(current_group));
            current_group = group{};
        } else {
            current_group.add_answers(line);
        }
    }
    groups.add_group(std::move(current_group));

    sr::solution(groups.part1());
    sr::solution(groups.part2());

    return 0;
}
