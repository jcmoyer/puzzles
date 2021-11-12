#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>

constexpr bool reacts(char x, char y) {
    return x == (y - 0x20) || y == (x - 0x20);
}

std::string react(std::string polymer) {
    std::string::iterator it = polymer.begin();
    for (;;) {
        it = std::adjacent_find(it, polymer.end(), reacts);
        if (it == polymer.end())
            break;
        it = polymer.erase(it, it + 2);
        // rewind the iterator since it's possible the character before this match can now react
        if (it > polymer.begin())
            --it;
    }
    return polymer;
}

std::string react2(std::string polymer) {
    std::string rstack;
    rstack.reserve(polymer.size());
    for (int i = 0; i < polymer.size(); ++i) {
        if (rstack.size() > 0 && reacts(rstack.back(), polymer[i])) {
            rstack.pop_back();
        } else {
            rstack.push_back(polymer[i]);
        }
    }
    return rstack;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string base_polymer;
    std::getline(input, base_polymer);

    std::string reacted_polymer = react(base_polymer);
    std::cout << reacted_polymer.size() << std::endl;

    size_t min_size = std::numeric_limits<size_t>::max();
    for (char c = 'a'; c <= 'z'; ++c) {
        std::string new_polymer = base_polymer;
        new_polymer.erase(std::remove_if(new_polymer.begin(),
                              new_polymer.end(),
                              [c](char x) {
                                  return x == c || x == c - 0x20;
                              }),
            new_polymer.end());
        new_polymer = react(std::move(new_polymer));
        if (new_polymer.size() < min_size) {
            min_size = new_polymer.size();
        }
    }
    std::cout << min_size << std::endl;

    return 0;
}
