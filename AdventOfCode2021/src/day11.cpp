#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

class world {
public:
    world(sr::array2d<int> m) {
        reset(std::move(m));
    }

    void reset(sr::array2d<int> m) {
        octopi = std::move(m);
        flashed.reserve(octopi.size());
        flashable.reserve(octopi.size());
    }

    size_t tick() {
        flashed.clear();
        flashable.clear();

        size_t count = 0;

        for (int& e : octopi)
            ++e;

        while (get_flashable()) {
            for (auto& point : flashable) {
                flash(point);
            }
            count += flashable.size();
        }

        for (auto p : flashed) {
            octopi.at(p) = 0;
        }

        return count;
    }

private:
    void flash(sr::vec2i p) {
        flashed.insert(p);
        sr::for_neighbors8(octopi, p, [&](int& e, auto p) {
            ++e;
        });
    }

    bool get_flashable() {
        flashable.clear();
        sr::for_each_2d(octopi, [&](int& e, auto p) {
            if (!flashed.contains(p) && e > 9)
                flashable.push_back(p);
        });
        return flashable.size();
    }

private:
    sr::array2d<int> octopi;
    std::unordered_set<sr::vec2i> flashed;
    std::vector<sr::vec2i> flashable;
};

sr::array2d<int> to_int_map(const sr::array2d<char>& m) {
    sr::array2d<int> out(m.width(), m.height());
    sr::for_each_2d(m, [&](char ch, auto point) {
        out.at(point) = ch - '0';
    });
    return out;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);
    auto char_tilemap = sr::read_tilemap(input);
    world w(to_int_map(char_tilemap));

    // part 1
    size_t flashes = 0;
    for (int i = 0; i < 100; ++i) {
        flashes += w.tick();
    }
    sr::solution(flashes);

    // part 2
    w.reset(to_int_map(char_tilemap));
    for (int i = 0;; ++i) {
        if (w.tick() == 100) {
            sr::solution(1 + i);
            break;
        }
    }

    return 0;
}
