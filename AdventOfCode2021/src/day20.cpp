#include <sr/sr.hpp>

struct image_bounds {
    sr::vec2i min, max;

    bool contains(const sr::vec2i& p) const {
        return p.x() >= min.x() && p.y() >= min.y() && p.x() <= max.x() && p.y() <= max.y();
    }
};

class image {
public:
    image() {}

    image(const sr::array2d<char>& data) {
        for (int y = 0; y < data.height(); ++y) {
            for (int x = 0; x < data.width(); ++x) {
                if (data.at(x, y) == '#')
                    set({x, y});
            }
        }
        bounds = sim_bounds();
    }

    void set(const sr::vec2i& p) {
        pixels.insert(p);
    }

    bool is_light(const sr::vec2i& p) const {
        if (pixels.contains(p))
            return true;
        if (bounds.contains(p))
            return false;
        return space_bit;
    }

    uint16_t index_at(const sr::vec2i& p) const {
        uint16_t index = 0;
        for (int yo = -1; yo <= 1; ++yo) {
            for (int xo = -1; xo <= 1; ++xo) {
                index <<= 1;
                index |= static_cast<uint16_t>(is_light(p + sr::vec2i{xo, yo}));
            }
        }
        return index;
    }

    image_bounds sim_bounds() const {
        image_bounds result{
            {std::numeric_limits<int>::max(), std::numeric_limits<int>::max()},
            {std::numeric_limits<int>::min(), std::numeric_limits<int>::min()},
        };
        for (const auto& p : pixels) {
            result.min.x() = std::min(result.min.x(), p.x());
            result.min.y() = std::min(result.min.y(), p.y());
            result.max.x() = std::max(result.max.x(), p.x());
            result.max.y() = std::max(result.max.y(), p.y());
        }
        result.min -= {1, 1};
        result.max += {1, 1};
        return result;
    }

    size_t count_lit() const {
        return pixels.size();
    }

    void enhance(const sr::dynamic_bitset& filter, image& output) const {
        const size_t space_bit_transition = space_bit ? 0b111'111'111 : 0;
        output.space_bit = filter.get(space_bit_transition);
        output.pixels.clear();
        image_bounds b = sim_bounds();
        for (int y = b.min.y(); y <= b.max.y(); ++y) {
            for (int x = b.min.x(); x <= b.max.x(); ++x) {
                if (filter.get(index_at({x, y})))
                    output.set({x, y});
            }
        }
        output.bounds = b;
    }

    void reserve(size_t count) {
        pixels.reserve(count);
    }

private:
    std::unordered_set<sr::vec2i> pixels;
    // whether infinite space is light or dark
    bool space_bit = false;
    image_bounds bounds{};
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    auto& input = args.get_input_stream();

    std::string filter_str;
    std::getline(input, filter_str);

    std::string empty;
    std::getline(input, empty);

    auto image_data = sr::read_tilemap(input);

    sr::dynamic_bitset filter;
    for (char ch : filter_str) {
        filter.push_back(ch == '#');
    }

    // keep two images and reuse storage
    image images[2]{
        image(image_data),
        image(),
    };
    images[0].reserve(20000);
    images[1].reserve(20000);
    size_t dest = 1;

    // part 1
    for (int i = 0; i < 2; ++i) {
        images[!dest].enhance(filter, images[dest]);
        dest = !dest;
    }
    sr::solution(images[!dest].count_lit());

    // part 2
    for (int i = 0; i < 48; ++i) {
        images[!dest].enhance(filter, images[dest]);
        dest = !dest;
    }
    sr::solution(images[!dest].count_lit());

    return 0;
}
