#include <fstream>
#include <iostream>
#include <numeric>
#include <unordered_set>
#include <vector>

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    if (!input) {
        std::cerr << "could not open file: " << argv[1];
        return 1;
    }

    std::vector<int> frequencies;

    int d;
    while (input >> d) {
        frequencies.push_back(d);
    }

    // Part 1
    std::cout << std::reduce(frequencies.begin(), frequencies.end()) << std::endl;

    // Part 2
    int freq = 0;
    auto freq_it = frequencies.begin();
    std::unordered_set<int> hist;
    std::unordered_set<int>::const_iterator hist_it;
    hist.emplace(0);
    while (true) {
        freq += *freq_it;
        // wrap around to beginning frequency if we're at the end
        ++freq_it;
        if (freq_it == frequencies.end()) {
            freq_it = frequencies.begin();
        }
        bool inserted;
        std::tie(hist_it, inserted) = hist.emplace(freq);
        if (!inserted)
            break;
    }

    std::cout << *hist_it << std::endl;

    return 0;
}
