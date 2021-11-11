#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

static constexpr sr::point neighborhood[]{

    {-1, -1},
    {0, -1},
    {1, -1},

    {-1, 0},
    {1, 0},

    {-1, 1},
    {0, 1},
    {1, 1},

};

bool in_bounds(const sr::array2d<char>& tilemap, int x, int y) {
    return x >= 0 && x < tilemap.width() && y >= 0 && y < tilemap.height();
}

int count_directional_occupied(const sr::array2d<char>& tilemap, int x, int y, int maxiter = -1) {
    int sum = 0;
    for (int dx = -1; dx <= 1; ++dx) {
        for (int dy = -1; dy <= 1; ++dy) {
            if (dx == 0 && dy == 0)
                continue;
            int xx = x + dx;
            int yy = y + dy;
            int iter = 0;
            while (in_bounds(tilemap, xx, yy) && (maxiter == -1 || iter < maxiter)) {
                char tile = tilemap.at(xx, yy);
                if (tile == '#') {
                    ++sum;
                    break;
                } else if (tile == 'L') {
                    break;
                }
                xx += dx;
                yy += dy;
                ++iter;
            }
        }
    }
    return sum;
}

int count_adjacent_occupied(const sr::array2d<char>& tilemap, int x, int y) {
    return count_directional_occupied(tilemap, x, y, 1);
}

namespace std {
template <>
struct hash<sr::array2d<char>> {
    size_t operator()(const sr::array2d<char>& c) const {
        size_t h = 0;
        for (int y = 0; y < c.height(); ++y) {
            for (int x = 0; x < c.width(); ++x) {
                h += y * x + c.at(x, y);
            }
        }
        return h;
    }
};
}

enum class simulation_logic { adjacent, directional };

template <typename Cell>
struct ca {
    std::unordered_map<sr::array2d<Cell>, sr::array2d<Cell>> states;
    sr::array2d<Cell> last{};
    sr::array2d<Cell> buffer;
    sr::array2d<Cell> current;

    struct iteration_context {
        bool running = true;

        void abort() {
            running = false;
        }
    };

    struct update_context {
        struct cell_proxy {
            ca& owner;
            size_t x, y;

            cell_proxy(ca& owner_, size_t x_, size_t y_) : owner{owner_}, x{x_}, y{y_} {}

            cell_proxy& operator=(const Cell& val) {
                owner.buffer.at(x, y) = val;
                return *this;
            }

            operator Cell() const {
                return owner.current.at(x, y);
            }
        };

        ca& owner;
        size_t x, y;

        update_context(ca& owner_, size_t x_, size_t y_) : owner{owner_}, x{x_}, y{y_} {}

        cell_proxy self() const {
            return cell_proxy(owner, x, y);
        }

        bool in_bounds(int x, int y) const {
            return x >= 0 && x < owner.current.width() && y >= 0 && y < owner.current.height();
        }

        template <typename F>
        void for_each_neighbor(F func) const {
            for (int dx = -1; dx <= 1; ++dx) {
                for (int dy = -1; dy <= 1; ++dy) {
                    if (dx == 0 && dy == 0)
                        continue;
                    if (in_bounds(x + dx, y + dy)) {
                        func(owner.current.at(x + dx, y + dy));
                    }
                }
            }
        }

        template <typename F>
        void for_each_ray(int dx, int dy, F func) const {
            iteration_context ctx;
            int xx = (int)x + dx;
            int yy = (int)y + dy;
            while (ctx.running && in_bounds(xx, yy)) {
                func(ctx, owner.current.at(xx, yy));
                xx += dx;
                yy += dy;
            }
        }

        size_t count_neighbors(const Cell& elem) const {
            size_t sum = 0;
            for_each_neighbor([&](const Cell& c) {
                if (c == elem) {
                    ++sum;
                }
            });
            return sum;
        }
    };

public:
    ca() = default;

    ca(sr::array2d<Cell> arr) {
        reset(std::move(arr));
    }

    void reset(sr::array2d<Cell> arr) {
        buffer = arr;
        current = std::move(arr);
        last = {};
        states.clear();
    }

    void step() {
        if (auto it = states.find(current); it != states.end()) {
            last = current;
            current = it->second;
            return;
        }

        for (int y = 0; y < current.height(); ++y) {
            for (int x = 0; x < current.width(); ++x) {
                update_cell(update_context(*this, x, y));
            }
        }

        last = current;
        states[current] = buffer;
        current = buffer;
    }

    void run_to_stable() {
        while (current != last) {
            step();
        }
    }

protected:
    virtual void update_cell(const update_context& ctx) = 0;

private:
};

struct ca_part1 : public ca<char> {
public:
    using ca::ca;

protected:
    void update_cell(const update_context& ctx) override {
        if (ctx.self() == 'L' && ctx.count_neighbors('#') == 0) {
            ctx.self() = '#';
        } else if (ctx.self() == '#' && ctx.count_neighbors('#') >= 4) {
            ctx.self() = 'L';
        }
    }
};

struct ca_part2 : public ca<char> {
public:
    using ca::ca;

protected:
    size_t count_visible_occupied(const update_context& ctx) {
        int hits = 0;
        for (const auto& d : neighborhood) {
            ctx.for_each_ray(d.x, d.y, [&](iteration_context& ctx, char cell) {
                if (cell == '#') {
                    ++hits;
                    ctx.abort();
                } else if (cell == 'L') {
                    ctx.abort();
                }
            });
        }
        return hits;
    }

    void update_cell(const update_context& ctx) override {
        if (ctx.self() == 'L' && count_visible_occupied(ctx) == 0) {
            ctx.self() = '#';
        } else if (ctx.self() == '#' && count_visible_occupied(ctx) >= 5) {
            ctx.self() = 'L';
        }
    }
};

int run_to_stable(sr::array2d<char> tilemap, simulation_logic logic) {
    std::unordered_map<sr::array2d<char>, sr::array2d<char>> states;
    sr::array2d<char> last{};
    sr::array2d<char> new_tilemap = tilemap;

    while (tilemap != last) {
        if (auto it = states.find(tilemap); it != states.end()) {
            last = tilemap;
            tilemap = it->second;
        } else {
            for (int y = 0; y < tilemap.height(); ++y) {
                for (int x = 0; x < tilemap.width(); ++x) {
                    char tile = tilemap.at(x, y);

                    // yeah should be functions or w/e deal with it
                    if (logic == simulation_logic::adjacent) {
                        if (tile == 'L') {
                            if (count_adjacent_occupied(tilemap, x, y) == 0) {
                                new_tilemap.at(x, y) = '#';
                            }
                        } else if (tile == '#') {
                            if (count_adjacent_occupied(tilemap, x, y) >= 4) {
                                new_tilemap.at(x, y) = 'L';
                            }
                        }
                    } else {
                        if (tile == 'L') {
                            if (count_directional_occupied(tilemap, x, y) == 0) {
                                new_tilemap.at(x, y) = '#';
                            }
                        } else if (tile == '#') {
                            if (count_directional_occupied(tilemap, x, y) >= 5) {
                                new_tilemap.at(x, y) = 'L';
                            }
                        }
                    }
                }
            }

            last = tilemap;
            states[tilemap] = new_tilemap;
            tilemap = new_tilemap;
        }
    }

    return std::count_if(tilemap.begin(), tilemap.end(), [](char c) {
        return c == '#';
    });
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    sr::array2d<char> tilemap = sr::read_tilemap(input);

    ca_part1 p1(tilemap);
    p1.run_to_stable();
    sr::solution(std::count(p1.current.begin(), p1.current.end(), '#'));

    ca_part2 p2(tilemap);
    p2.run_to_stable();
    sr::solution(std::count(p2.current.begin(), p2.current.end(), '#'));

    return 0;
}
