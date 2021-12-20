#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct image_bounds {
    sr::vec2i min, max;
};

class image {
public:
    image() {}

    image(const sr::array2d<char>& data) {
        for (int y = 0; y < data.height(); ++y) {
            for (int x = 0; x < data.width(); ++x) {
                set({x, y}, data.at(x, y) == '#');
            }
        }
    }

    void set(sr::vec2i p, bool state) {
        pixels[p] = state;
    }

    bool is_light(sr::vec2i p) const {
        if (auto it = pixels.find(p); it != pixels.end())
            return it->second;
        return space_bit;
    }

    uint32_t index_at(sr::vec2i p) const {
        uint32_t index = 0;
        for (int yo = -1; yo <= 1; ++yo) {
            for (int xo = -1; xo <= 1; ++xo) {
                index <<= 1;
                index |= is_light(p + sr::vec2i{xo, yo});
            }
        }
        return index;
    }

    image_bounds sim_bounds() const {
        auto [minx, maxx] = std::ranges::minmax_element(pixels, [](auto&& p0, auto&& p1) {
            return p0.first.x() < p1.first.x();
        });
        auto [miny, maxy] = std::ranges::minmax_element(pixels, [](auto&& p0, auto&& p1) {
            return p0.first.y() < p1.first.y();
        });
        return {
            {minx->first.x() - 1, miny->first.y() - 1},
            {maxx->first.x() + 1, maxy->first.y() + 1},
        };
    }

    int count_lit() const {
        return (int)std::ranges::count_if(pixels, [](auto&& p) {
            return p.second;
        });
    }

    void enhance(const sr::dynamic_bitset& filter, image& output) const {
        output.space_bit = !space_bit;
        image_bounds b = sim_bounds();
        for (int y = b.min.y(); y <= b.max.y(); ++y) {
            for (int x = b.min.x(); x <= b.max.x(); ++x) {
                uint32_t index = index_at({x, y});
                output.set({x, y}, filter.get(index));
            }
        }
    }

private:
    std::unordered_map<sr::vec2i, bool> pixels;
    // whether infinite space is light or dark; assumes filter[0] is #
    bool space_bit = false;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    auto& input = args.get_input_stream();

    std::string filter_str;
    std::getline(input, filter_str);

    std::string empty;
    std::getline(input, empty);

    auto image_data = sr::read_tilemap(input);
    image init_image(image_data);

    sr::dynamic_bitset filter;
    for (char ch : filter_str) {
        filter.push_back(ch == '#');
    }

    // part 1
    image working_image = init_image;
    for (int i = 0; i < 2; ++i) {
        image new_image;
        working_image.enhance(filter, new_image);
        working_image = new_image;
    }
    sr::solution(working_image.count_lit());

    // part 2
    working_image = init_image;
    for (int i = 0; i < 50; ++i) {
        image new_image;
        working_image.enhance(filter, new_image);
        working_image = new_image;
    }
    sr::solution(working_image.count_lit());

    return 0;
}
