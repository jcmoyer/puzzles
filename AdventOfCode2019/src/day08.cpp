#include <fmt/format.h>
#include <fstream>
#include <string>
#include <string_view>
#include <vector>

const size_t WIDTH = 25;
const size_t HEIGHT = 6;
const size_t LAYER_SIZE = WIDTH * HEIGHT;

enum color : char { black = '0', white = '1', transparent = '2' };

using pixel = char;
using layer = std::string_view;
using image = std::vector<layer>;

image load_image(std::string_view s) {
    if (s.size() % LAYER_SIZE != 0) {
        throw std::runtime_error("image source contains an incomplete layer");
    }

    image im;
    im.reserve(s.size() / LAYER_SIZE);

    for (size_t offset = 0; offset < s.size(); offset += LAYER_SIZE) {
        im.emplace_back(s.data() + offset, LAYER_SIZE);
    }

    return im;
}

pixel raycast(const image& im, int x, int y) {
    for (int z = 0; z < im.size(); ++z) {
        pixel p = im[z][y * WIDTH + x];
        if (p != transparent) {
            return p;
        }
    }
    return transparent;
}

void render_image(const image& im) {
    for (int y = 0; y < HEIGHT; ++y) {
        for (int x = 0; x < WIDTH; ++x) {
            pixel p = raycast(im, x, y);
            fmt::print("{}", p == black ? '_' : '#');
        }
        fmt::print("\n");
    }
}

auto find_layer_with_least_color(const image& im, pixel color) {
    return std::min_element(im.begin(), im.end(), [color](const layer& x, const layer& y) {
        return std::count(x.begin(), x.end(), color) < std::count(y.begin(), y.end(), color);
    });
}

int count_color(const layer& layer, pixel color) {
    return static_cast<int>(std::count(layer.begin(), layer.end(), color));
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    std::string line;
    std::getline(input, line);

    auto image = load_image(line);

    // part 1
    const layer& ly = *find_layer_with_least_color(image, black);
    fmt::print("{}\n", count_color(ly, white) * count_color(ly, transparent));

    // part 2
    render_image(image);

    return 0;
}
