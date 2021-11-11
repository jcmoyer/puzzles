#include <fstream>

#include <sr/sr.hpp>

int count_trees(const sr::array2d<char>& tilemap, sr::vec2i slope) {
    sr::vec2i pos{0, 0};
    int sum = 0;
    while (pos.y() < tilemap.height()) {
        if (tilemap.at(pos.x() % tilemap.width(), pos.y()) == '#') {
            ++sum;
        }
        pos += slope;
    }
    return sum;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);
    sr::array2d<char> tilemap = sr::read_tilemap(input);

    sr::solution(count_trees(tilemap, {3, 1}));

    sr::solution(count_trees(tilemap, {1, 1}) * count_trees(tilemap, {3, 1}) * count_trees(tilemap, {5, 1}) *
                 count_trees(tilemap, {7, 1}) * count_trees(tilemap, {1, 2}));

    return 0;
}
