#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

struct box_id {
    std::map<char, int> letters;
    std::string id;
    bool is_two = false;
    bool is_three = false;

    box_id(std::string id_) : id{std::move(id_)} {
        for (char ch : id) {
            ++letters[ch];
        }
        for (const auto& [letter, n] : letters) {
            if (n == 2) {
                is_two = true;
            } else if (n == 3) {
                is_three = true;
            }
        }
    }
};

int hamming(std::string_view x, std::string_view y) {
    if (x.size() != y.size())
        throw std::runtime_error("string sizes not equal");
    int sum = 0;
    for (size_t i = 0; i < x.size(); ++i) {
        if (x[i] != y[i])
            ++sum;
    }
    return sum;
}

// Returns a string consisting of only the characters that are the same in
// corresponding indices between the strings x and y. The order of the
// characters is preserved from the original strings.
//
// This is essentially a set intersection where values in the set are compared
// on both their value and location within a collection. The set is sorted on
// location.
std::string intersection(std::string_view x, std::string_view y) {
    if (x.size() != y.size())
        throw std::runtime_error("string sizes not equal");
    std::string result;
    result.reserve(x.size());
    for (size_t i = 0; i < x.size(); ++i) {
        if (x[i] == y[i])
            result += x[i];
    }
    return result;
}

int checksum(int twos, int threes) {
    return twos * threes;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);

    // Load all box IDs into memory
    std::string line;
    std::vector<box_id> box_ids;
    while (std::getline(input, line)) {
        box_ids.emplace_back(line);
    }

    // Part 1
    int twos = 0;
    int threes = 0;
    for (const auto& bid : box_ids) {
        if (bid.is_two)
            ++twos;
        if (bid.is_three)
            ++threes;
    }
    std::cout << checksum(twos, threes) << std::endl;

    // Part 2
    for (size_t i = 0; i < box_ids.size(); ++i) {
        for (size_t j = i + 1; j < box_ids.size(); ++j) {
            if (hamming(box_ids[i].id, box_ids[j].id) == 1) {
                std::cout << intersection(box_ids[i].id, box_ids[j].id) << std::endl;
                goto end_part2;
            }
        }
    }
end_part2:

    return 0;
}
