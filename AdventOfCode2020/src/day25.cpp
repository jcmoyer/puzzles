#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int64_t transform(int64_t subject_number, int64_t loop_size) {
    int64_t value = 1;
    for (int64_t i = 0; i < loop_size; ++i) {
        value = value * subject_number;
        value %= 20201227;
    }
    return value;
}

int64_t transform_find(int64_t subject_number, int64_t search_key) {
    int64_t value = 1;
    int64_t loop_iterations = 0;
    while (true) {
        ++loop_iterations;
        value = value * subject_number;
        value %= 20201227;
        if (value == search_key)
            return loop_iterations;
    }
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string key0str, key1str;
    std::getline(input, key0str);
    std::getline(input, key1str);

    int64_t key0, key1;
    key0 = std::stoll(key0str);
    key1 = std::stoll(key1str);

    int64_t loop0, loop1;

    loop0 = transform_find(7, key0);
    sr::solution(transform(key1, loop0));

    return 0;
}
