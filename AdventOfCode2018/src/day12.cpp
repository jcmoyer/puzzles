// TODO: cleanup

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

struct rule {
    bool init = false;
    std::string search;
    char replace;

    rule() {}
    rule(std::string search, char replace) : init{true}, search{std::move(search)}, replace{replace} {}
};

int rule2key(const char* r) {
    return (*r++ == '#' ? 1 : 0) << 4 | (*r++ == '#' ? 1 : 0) << 3 | (*r++ == '#' ? 1 : 0) << 2 |
           (*r++ == '#' ? 1 : 0) << 1 | (*r++ == '#' ? 1 : 0) << 0;
}

struct dfa {
    dfa() : cells(total_width, '.') {}

    constexpr size_t map_index(int pot_number) const {
        return pot_number + first_pot;
    }

    constexpr int unmap_index(size_t i) const {
        return i - first_pot;
    }

    void set_state(const char* s) {
        for (int i = 0; i < strlen(s); ++i) {
            cells[first_pot + i] = s[i];
        }
    }

    void add_rule(const char* r, char replacement) {
        assert(strlen(r) == 5);
        rule_array[rule2key(r)] = rule{r, replacement};
    }

    void simulate() {
        std::string new_cells(cells.size(), '.');
        std::string_view sv(cells.data(), 5);

        for (int i = 0; i < cells.size() - 2; ++i) {
            int k = rule2key(sv.data());
            rule& r = rule_array[k];
            if (r.init) {
                new_cells[i + 2] = r.replace;
            }
            sv = std::string_view(sv.data() + 1, 5);
        }

        cells = std::move(new_cells);
        ++generation;
    }

    int pot_sum() const {
        int sum = 0;
        for (int i = 0; i < cells.size(); ++i) {
            if (cells[i] == '#')
                sum += unmap_index(i);
        }
        return sum;
    }

    void reset() {
        generation = 0;
        cells = std::string(total_width, '.');
        rule_array = {};
    }

    std::array<rule, 32> rule_array;
    std::string cells;
    int64_t generation = 0;
    static constexpr size_t total_width = 4096;
    static constexpr size_t first_pot = total_width / 2;
};

void add_rules(dfa& d) {
    d.add_rule("#....", '.');
    d.add_rule(".##.#", '#');
    d.add_rule("#..##", '.');
    d.add_rule("....#", '.');
    d.add_rule("###.#", '#');
    d.add_rule("...#.", '#');
    d.add_rule("#...#", '#');
    d.add_rule("#.###", '.');
    d.add_rule(".#...", '#');
    d.add_rule("...##", '.');
    d.add_rule("..###", '.');
    d.add_rule("####.", '.');
    d.add_rule("##.##", '.');
    d.add_rule("..##.", '.');
    d.add_rule(".#.##", '#');
    d.add_rule("#..#.", '#');
    d.add_rule(".....", '.');
    d.add_rule("#.#..", '.');
    d.add_rule("##.#.", '#');
    d.add_rule(".####", '#');
    d.add_rule("#####", '.');
    d.add_rule("#.##.", '#');
    d.add_rule(".#..#", '#');
    d.add_rule("##...", '.');
    d.add_rule("..#.#", '#');
    d.add_rule("##..#", '#');
    d.add_rule(".###.", '.');
    d.add_rule(".#.#.", '#');
    d.add_rule("#.#.#", '#');
    d.add_rule("###..", '.');
    d.add_rule(".##..", '.');
    d.add_rule("..#..", '.');
}

int main(int argc, char* argv[]) {
    const char* initial_state =
        ".#..##..#.....######.....#....####.##.#.#...#...##.#...###..####.##.##.####..######......#..##.##.##";

    dfa d;
    add_rules(d);
    d.set_state(initial_state);

    for (int64_t i = 0; i < 20; ++i) {
        d.simulate();
    }
    std::cout << d.pot_sum() << std::endl;

    d.reset();
    add_rules(d);
    d.set_state(initial_state);

    int last_sum = 0;
    std::deque<int> progression;

    for (int64_t i = 0; i < 50000000000; ++i) {
        d.simulate();

        int this_sum = d.pot_sum();
        int diff = this_sum - last_sum;
        progression.push_back(diff);
        last_sum = this_sum;

        if (progression.size() > 20)
            progression.pop_front();

        // this has happened very frequently; population is probably stable
        if (progression.size() == 20 && std::all_of(progression.begin(), progression.end(), [&progression](int diff) {
                return diff == progression[0];
            })) {
            // predict the future
            int64_t remaining_gens = 50000000000 - d.generation;
            int64_t future_pots = this_sum + remaining_gens * diff;
            std::cout << future_pots << std::endl;
            break;
        }
    }

    return 0;
}
