#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

template <typename OutputIt>
void deltas(auto&& rng, OutputIt out) {
    auto first = std::begin(rng);
    auto last = std::end(rng);
    auto next = first;
    ++next;
    while (next != last) {
        *out++ = *next - *first;
        ++first;
        ++next;
    }
}

template <typename F>
void window(auto&& rng, size_t window_size, F callback) {
    auto first = std::begin(rng);
    auto last = std::end(rng);
    auto window_end = first;
    // adjust window_end being careful to not go past the end of the range
    while (window_size && window_end != last) {
        ++window_end;
        --window_size;
    }
    do {
        callback(first, window_end);
        ++first;
        ++window_end;
    } while (window_end != last);
    callback(first, window_end);
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<int> ent;
    for (const auto& line : sr::lines(input)) {
        ent.push_back(std::stoi(line));
    }

    std::vector<int> delta_ints;
    size_t counts = 0;

    // part 1
    deltas(ent, std::back_inserter(delta_ints));
    counts = std::ranges::count_if(delta_ints, [](int x) {
        return x > 0;
    });
    sr::solution(counts);

    // part 2
    std::vector<int> windows;
    window(ent, 3, [&](auto first, auto last) {
        windows.emplace_back(std::accumulate(first, last, 0));
    });
    delta_ints.clear();
    deltas(windows, std::back_inserter(delta_ints));
    counts = std::ranges::count_if(delta_ints, [](int x) {
        return x > 0;
    });
    sr::solution(counts);

    return 0;
}
