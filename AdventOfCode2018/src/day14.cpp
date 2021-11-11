#include <algorithm>
#include <charconv>
#include <cstdint>
#include <execution>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

std::vector<int> recipes{3, 7};
std::size_t elf1 = 0;
std::size_t elf2 = 1;

void step_elf(std::size_t& e) {
    int step = 1 + recipes[e];
    e = (e + step) % recipes.size();
}

void combine_recipes() {
    int n = recipes[elf1] + recipes[elf2];
    int tens = n / 10;
    int ones = n % 10;

    if (n >= 10) {
        recipes.push_back(tens);
        recipes.push_back(ones);
    } else {
        recipes.push_back(ones);
    }

    step_elf(elf1);
    step_elf(elf2);
}

std::vector<int> digits(int i) {
    std::vector<int> r;
    while (i) {
        int next_digit = i % 10;
        i /= 10;
        r.push_back(next_digit);
    }
    std::reverse(r.begin(), r.end());
    return r;
}

int main(int argc, char* argv[]) {
    std::size_t recipe_count;

    std::ifstream input(argv[1]);
    std::string line;
    if (std::getline(input, line)) {
        std::from_chars(line.data(), line.data() + line.size(), recipe_count);
    } else {
        return 1;
    }

    recipes.reserve(recipe_count * 128);

    // part1
    while (recipes.size() < recipe_count + 10) {
        combine_recipes();
    }
    auto first = recipes.begin() + recipe_count;
    auto last = first + 10;
    for (auto it = first; it != last; ++it) {
        std::cout << *it;
    }
    std::cout << std::endl;

    // part2

    /*
    const int recipe_batch_size = 500000;
    std::size_t search_start = 0;
    std::vector<int> search_range = digits(recipe_count);
    for (;;) {
      for (int i = 0; i < recipe_batch_size; ++i) {
        combine_recipes();
      }
      auto search_start_it = recipes.begin() + search_start;
      search_start = std::distance(recipes.begin(), recipes.end() - search_range.size());
      auto sresult = std::search(std::execution::par, search_start_it, recipes.end(), search_range.begin(),
    search_range.end()); if (sresult != recipes.end()) { std::cout << std::distance(recipes.begin(), sresult); break;
      }
    }
    std::cout << std::endl;
    */

    std::vector<int> search_range = digits(recipe_count);
    for (;;) {
        auto sresult = std::search(
            recipes.rbegin(), recipes.rbegin() + search_range.size() + 1, search_range.begin(), search_range.end());
        if (sresult != recipes.rbegin() + search_range.size() + 1) {
            std::cout << std::distance(sresult + search_range.size(), recipes.rend()) << std::endl;
            break;
        }
        combine_recipes();
    }

    return 0;
}
