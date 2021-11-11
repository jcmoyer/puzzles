#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

#include "robin_hood.hpp"

template <typename Vec>
bool vector_step(Vec& pos, const Vec& min, const Vec& max) {
    if (pos[Vec::size - 1] <= max[Vec::size - 1]) {
        ++pos[0];
        // carry rightward
        for (size_t i = 0; i < Vec::size - 1; ++i) {
            if (pos[i] > max[i]) {
                pos[i] = min[i];
                ++pos[i + 1];
            }
        }
        return pos[Vec::size - 1] <= max[Vec::size - 1];
    }
    return false;
}

template <typename Index>
struct world {
    robin_hood::unordered_flat_map<Index, char> cells;
    Index min, max;

    bool active(const Index& p) const {
        if (auto it = cells.find(p); it == cells.end()) {
            return false;
        } else {
            return it->second == '#';
        }
    }

    void compute_bounds() {
        auto [e0, e1] = std::minmax_element(cells.begin(), cells.end());
        min = e0->first - 1;
        max = e1->first + 1;
    }

    int count_active() const {
        int sum = 0;
        Index pos = min;

        do {
            sum += active(pos);
        } while (vector_step(pos, min, max));

        return sum;
    }

    void step() {
        robin_hood::unordered_flat_map<Index, char> newstate;

        Index pos = min;

        do {
            int neighbors_active = 0;

            Index offset_min = Index::elementwise_init(-1);
            Index offset_max = Index::elementwise_init(+1);
            Index offset = offset_min;

            do {
                bool all_zero = std::all_of(offset.begin(), offset.end(), [](auto&& elem) {
                    return elem == 0;
                });

                if (all_zero) {
                    continue;
                }

                if (active(pos + offset)) {
                    ++neighbors_active;
                }
            } while (vector_step(offset, offset_min, offset_max));

            if (active(pos)) {
                if (neighbors_active == 2 || neighbors_active == 3) {
                    // stay active
                    newstate[pos] = '#';
                }
            } else {
                if (neighbors_active == 3) {
                    // become active
                    newstate[pos] = '#';
                }
            }
        } while (vector_step(pos, min, max));

        cells = std::move(newstate);
        min -= 1;
        max += 1;
    }
};

template <typename Index>
int do_simulation(const sr::array2d<char>& tilemap) {
    world<Index> w;
    for (int y = 0; y < tilemap.height(); ++y) {
        for (int x = 0; x < tilemap.width(); ++x) {
            w.cells[Index{x, y}] = tilemap.at(x, y);
        }
    }

    w.compute_bounds();
    for (int i = 0; i < 6; ++i)
        w.step();
    return w.count_active();
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    sr::array2d<char> tilemap = sr::read_tilemap(input);

    sr::solution(do_simulation<sr::vec3i>(tilemap));
    sr::solution(do_simulation<sr::vec4i>(tilemap));

    return 0;
}
