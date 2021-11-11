#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct occurrence_info {
    // newer turn resides at 0
    size_t last_turns_spoken[2];
    size_t count = 0;

    void push_turn(size_t n) {
        last_turns_spoken[1] = last_turns_spoken[0];
        last_turns_spoken[0] = n;
        if (count != 2)
            ++count;
    }

    size_t turn_difference() const {
        if (count < 2)
            return 0;
        else
            return last_turns_spoken[0] - last_turns_spoken[1];
    }
};

size_t play_game(std::vector<size_t> numbers_spoken, size_t times) {
    std::unordered_map<size_t, occurrence_info> occurrences;

    // hack for part 2 to reduce allocations
    if (times > 1000000)
        occurrences.reserve(4000000);

    for (size_t i = 0; i < numbers_spoken.size(); ++i) {
        occurrences[numbers_spoken[i]].push_turn(i + 1);
    }

    for (size_t i = numbers_spoken.size(); i < times; ++i) {
        size_t last_number_spoken = numbers_spoken.back();
        size_t new_number = occurrences[last_number_spoken].turn_difference();
        occurrences[new_number].push_turn(i + 1);
        numbers_spoken.push_back(new_number);
    }

    return numbers_spoken.back();
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<size_t> numbers_spoken;
    std::string buf;
    while (std::getline(input, buf, ',')) {
        numbers_spoken.push_back(std::stoi(buf));
    }

    sr::solution(play_game(numbers_spoken, 2020));
    sr::solution(play_game(numbers_spoken, 30000000));

    return 0;
}
