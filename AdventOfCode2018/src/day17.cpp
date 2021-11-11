#include <algorithm>
#include <charconv>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/array2d.hpp"
#include "common/point.hpp"

class tile_flags {
public:
    enum flag_bits {
        // water: settle state signaled from a left/right wall and propagated by
        // water tiles; when both are set, there is a full row of water between
        // two correctly oriented walls
        LEFT_SETTLED = 0b00000001,
        RIGHT_SETTLED = 0b00000010,

        // sand/water: tile inside of a bucket?
        INTERIOR = 0b00000100,

        // clay: tile is a left/right wall?
        LEFT_INTERIOR = 0b00001000,
        RIGHT_INTERIOR = 0b00010000,

        // tile was visited during postprocessing
        VISITED = 0b00100000,

        // optimization
        FULL_SETTLED = 0b01000000
    };

    tile_flags() : flag{0} {}

#define FLAGDEF(name, k)                                                                                              \
    constexpr bool name() const {                                                                                     \
        return get_bit(k);                                                                                            \
    }                                                                                                                 \
    constexpr void name(bool state) {                                                                                 \
        set_bit(k, state);                                                                                            \
    }

    FLAGDEF(left_interior, LEFT_INTERIOR);
    FLAGDEF(right_interior, RIGHT_INTERIOR);
    FLAGDEF(visited, VISITED);
    FLAGDEF(interior, INTERIOR);
    FLAGDEF(left_settled, LEFT_SETTLED);
    FLAGDEF(right_settled, RIGHT_SETTLED);
    FLAGDEF(full_settled, FULL_SETTLED);
#undef FLAGDEF

    constexpr bool settled() const {
        return left_settled() && right_settled();
    }

private:
    constexpr void set_bit(flag_bits b, bool state) {
        if (state)
            flag |= b;
        else
            flag &= (~b);
    }

    constexpr bool get_bit(flag_bits b) const {
        return flag & b;
    }

    uint32_t flag;
};

struct tile {
    enum type { sand, water, clay, spring, bound };
    type ty;
    tile_flags f;

    type nty;
    tile_flags nf;

    void prepare() {
        nty = ty;
        nf = f;
    }

    void commit() {
        ty = nty;
        f = nf;
    }
};

struct world {
    world(int width, int height) : width{width}, height{height}, tiles(width, height, tile{tile::sand}) {
        dirty.reserve(width * height);
    }

    void set_clay(point p) {
        get_tile(p.x, p.y).ty = tile::clay;
    }

    void set_spring(point p) {
        spring = p;
        get_tile(spring.x, spring.y).ty = tile::spring;
    }

    int count_settled_water() const {
        return sum_settled;
    }

    int count_tiles_touched() {
        int sum = 0;
        for (int y = first_y; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                tile& t = get_tile(x, y);
                if (t.ty == tile::water) {
                    sum++;
                }
            }
        }
        return sum;
    }

    void update() {
        std::unordered_set<point> visited;
        std::vector<point> dirty_now = std::move(dirty);
        dirty = std::vector<point>();
        // somewhat tuned
        dirty.reserve(dirty_now.size() * 2);

        for (const auto& p : dirty_now) {
            if (visited.count(p))
                continue;
            tile& t = get_tile(p.x, p.y);
            t.prepare();
            if (t.ty == tile::water) {
                do_water(t, p.x, p.y);
            } else if (t.ty == tile::spring) {
                do_spring(t, p.x, p.y);
            } else if (t.ty == tile::clay) {
                do_clay(t, p.x, p.y);
            }
            visited.emplace(p);
        }

        // apply changes to dirty tiles
        for (const auto& p : dirty) {
            tile& t = get_tile(p.x, p.y);
            t.commit();
        }
    }

    void do_clay(tile& self, int x, int y) {
        // right interiors set right_settled on water left
        if (self.f.right_interior()) {
            tile& left = get_tile(x - 1, y);
            if (left.ty == tile::water && !left.f.right_settled()) {
                left.nf.right_settled(true);
                mark_dirty(x - 1, y, true);
            }
        }

        // left interiors set left_settled on water right
        if (self.f.left_interior()) {
            tile& right = get_tile(x + 1, y);
            if (right.ty == tile::water && !right.f.left_settled()) {
                right.nf.left_settled(true);
                mark_dirty(x + 1, y, true);
            }
        }
    }

    void do_spring(tile& self, int x, int y) {
        // spring only spawns water below
        tile& below = get_tile(x, y + 1);
        if (below.ty == tile::sand) {
            below.nty = tile::water;
            mark_dirty(x, y + 1);
        }
    }

    void do_water(tile& self, int x, int y) {
        // water only does something if the tile below is present
        tile& below = get_tile(x, y + 1);
        tile& left = get_tile(x - 1, y);
        tile& right = get_tile(x + 1, y);

        if (!self.f.full_settled() && self.f.left_settled() && self.f.right_settled()) {
            self.nf.full_settled(true);
            ++sum_settled;
        }

        switch (below.ty) {
        // fill sand below
        case tile::sand: {
            below.nty = tile::water;
            mark_dirty(x, y + 1);
            break;
        }
        // stack on water and clay, fill left-right sand with water
        case tile::water: {
            if (below.f.settled() && get_tile(x - 1, y).ty == tile::sand) {
                left.nty = tile::water;
                mark_dirty(x - 1, y);
            }
            if (below.f.settled() && get_tile(x + 1, y).ty == tile::sand) {
                right.nty = tile::water;
                mark_dirty(x + 1, y);
            }
            break;
        }
        case tile::clay: {
            if (left.ty == tile::sand) {
                left.nty = tile::water;
                mark_dirty(x - 1, y);
            }
            if (right.ty == tile::sand) {
                right.nty = tile::water;
                mark_dirty(x + 1, y);
            }
            break;
        }
        }

        // water propagates its left/right settled flags to adjacent water tiles, but only on interior cells
        if (self.f.interior()) {
            if (left.ty == tile::water && self.f.right_settled() != left.f.right_settled()) {
                left.nf.right_settled(self.f.right_settled());
                mark_dirty(x - 1, y);
            }

            if (right.ty == tile::water && self.f.left_settled() != right.f.left_settled()) {
                right.nf.left_settled(self.f.left_settled());
                mark_dirty(x + 1, y);
            }
        }
    }

    void dump(std::ostream& output) const {
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                const tile& t = get_tile(x, y);
                switch (t.ty) {
                case tile::sand: {
                    if (t.f.interior()) {
                        output << '.';
                    } else
                        output << ' ';
                    break;
                }
                case tile::water: {
                    if (t.f.settled())
                        output << '~';
                    else if (t.f.left_settled())
                        output << '<';
                    else if (t.f.right_settled())
                        output << '>';
                    else
                        output << '|';
                    break;
                }
                case tile::clay: {
                    if (t.f.left_interior() && t.f.right_interior())
                        output << 'X';
                    else if (t.f.right_interior())
                        output << 'R';
                    else if (t.f.left_interior())
                        output << 'L';
                    else
                        output << '#';
                    break;
                }
                case tile::spring:
                    output << 'O';
                    break;
                case tile::bound:
                    output << '\xDB';
                    break;
                }
            }
            output << std::endl;
        }
    }

    bool in_bounds(const point& p) const {
        return in_bounds(p.x, p.y);
    }

    bool in_bounds(int x, int y) const {
        return x >= 0 && x < width && y >= 0 && y < height;
    }

    tile& get_tile(int x, int y) {
        return tiles[point{x, y}];
    }

    const tile& get_tile(int x, int y) const {
        return tiles[point{x, y}];
    }

    void postprocess() {
        first_y = std::numeric_limits<int>::max();
        // find buckets (U shapes)
        for (int x = 0; x < width; ++x) {
            for (int y = 0; y < height; ++y) {
                tile& t = get_tile(x, y);
                if (t.ty == tile::clay) {
                    mark_bucket(x, y);

                    // first clay tile's Y coordinate is where water evaluation should start
                    first_y = std::min(first_y, y);
                }
                // also mark every tile as dirty
                mark_dirty(x, y, false);
            }
        }
        // eliminate the need for bounds checks
        for (int x = 0; x < width; ++x) {
            get_tile(x, 0).ty = tile::bound;
            get_tile(x, height - 1).ty = tile::bound;
        }
        for (int y = 0; y < height; ++y) {
            get_tile(0, y).ty = tile::bound;
            get_tile(width - 1, y).ty = tile::bound;
        }
    }

    void mark_bucket(int x, int y) {
        if (get_tile(x, y).f.visited())
            return;

        // TODO: remove winding stuff since I realized it's easier to iterate Y first
        // this implies that the left side of a U will ALWAYS be hit first

        int ybottom;
        for (ybottom = y; in_bounds(x, ybottom) && get_tile(x, ybottom).ty == tile::clay; ++ybottom) {
            get_tile(x, ybottom).f.visited(true);
        }
        // correct for OOB
        --ybottom;

        // which way does this bucket wind?
        const int WIND_CW = -1;
        const int WIND_CCW = +1;
        int winding = 0;
        if (get_tile(x - 1, ybottom).ty == tile::clay) {
            winding = WIND_CW;
        } else if (get_tile(x + 1, ybottom).ty == tile::clay) {
            winding = WIND_CCW;
        } else {
            // throw "what";
            return;
        }

        int x1 = x;
        for (x1 = x; in_bounds(x1, ybottom) && get_tile(x1, ybottom).ty == tile::clay; x1 += winding) {
            get_tile(x1, ybottom).f.visited(true);
        }
        // correct for OOB
        if (in_bounds(x1, ybottom))
            x1 -= winding;

        // crawl up opposite side
        int y1 = ybottom;
        for (y1 = ybottom; in_bounds(x1, y1) && get_tile(x1, y1).ty == tile::clay; --y1) {
            get_tile(x1, y1).f.visited(true);
        }
        if (in_bounds(x1, y1))
            ++y1;

        // these points define the U shape
        point v0{x, y};
        point v1{x, ybottom};
        point v2{x1, ybottom};
        point v3{x1, y1};

        // we're really only interested in the sides of the U since walls propagate interior/exterior behavior
        // these are v0..v1 and v2..v3
        for (int n = v0.y; n <= v1.y; ++n) {
            if (winding == WIND_CW) {
                get_tile(v0.x, n).f.right_interior(true);
            } else {
                get_tile(v0.x, n).f.left_interior(true);
            }
        }

        for (int n = v2.y; n >= v3.y; --n) {
            if (winding == WIND_CW) {
                get_tile(v2.x, n).f.left_interior(true);
            } else {
                get_tile(v2.x, n).f.right_interior(true);
            }
        }

        int ystart = std::max(v0.y, v3.y);
        int yend = ybottom;
        for (int yy = ystart; yy < yend; ++yy) {
            for (int xx = 1 + v0.x; xx < v2.x; ++xx) {
                get_tile(xx, yy).f.interior(true);
            }
        }

        ystart = std::min(v0.y, v3.y);
        for (int yy = ystart; yy < yend; ++yy) {
            for (int xx = 1 + v0.x; xx < v2.x; ++xx) {
                get_tile(xx, yy).f.interior(true);
                // additionally, make any buckets inside of this one an X-interior
                get_tile(xx, yy).f.left_interior(true);
                get_tile(xx, yy).f.right_interior(true);
            }
        }
    }

    void mark_dirty(int x, int y, bool neighborhood = true) {
        dirty.emplace_back(point{x, y});
        if (neighborhood) {
            // if (in_bounds(x, y-1 ))
            dirty.emplace_back(point{x, y - 1});
            // if (in_bounds(x - 1, y))
            dirty.emplace_back(point{x - 1, y});
            // if (in_bounds(x + 1, y))
            dirty.emplace_back(point{x + 1, y});
            // if (in_bounds(x, y + 1))
            dirty.emplace_back(point{x, y + 1});
        }
    }

    array2d<tile> tiles;
    std::vector<point> dirty;
    int sum_settled = 0;
    int width, height;
    point spring{500, 0};
    int first_y;
};

struct world_builder {
    std::unordered_map<point, tile> tiles;

    void add_clay(int x, int y) {
        tiles.emplace(std::make_pair(point{x, y}, tile{tile::clay}));
    }

    void add_spring() {
        tiles.emplace(std::make_pair(point{500, 0}, tile{tile::spring}));
    }

    world create_world() {
        add_spring();

        auto [min_x, max_x] = std::minmax_element(tiles.begin(), tiles.end(), [this](auto&& x, auto&& y) {
            return x.first.x < y.first.x;
        });
        auto [min_y, max_y] = std::minmax_element(tiles.begin(), tiles.end(), [this](auto&& x, auto&& y) {
            return x.first.y < y.first.y;
        });

        point min{min_x->first.x, min_y->first.y};
        point max{max_x->first.x, max_y->first.y};

        // HACK: inflate width by 2 tiles on both sides since the problem allows for infinite X
        min.x -= 2;
        max.x += 2;
        min.y--;
        max.y++;

        int width = max.x - min.x + 1;
        int height = max.y - min.y + 1;

        world w(width, height);
        for (const auto& [p, t] : tiles) {
            if (t.ty == tile::clay)
                w.set_clay(p - min);
            else if (t.ty == tile::spring)
                w.set_spring(p - min);
        }

        return w;
    }
};

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;

    std::regex COORD(R"(([xy])=(\d+), ([xy])=(\d+)\.\.(\d+))");

    world_builder b;

    while (std::getline(input, line)) {
        std::cmatch match;
        if (std::regex_match(line.data(), match, COORD)) {
            const char* axis1 = match[1].first;
            int axis1_x0;
            std::from_chars(match[2].first, match[2].second, axis1_x0);

            const char* axis2 = match[3].first;
            int axis2_x0;
            int axis2_x1;
            std::from_chars(match[4].first, match[4].second, axis2_x0);
            std::from_chars(match[5].first, match[5].second, axis2_x1);

            if (*axis1 == *axis2) {
                std::cerr << "bad coordinate; axes are the same" << std::endl;
                return 1;
            } else {
                if (*axis1 == 'x') {
                    for (int y = axis2_x0; y <= axis2_x1; ++y) {
                        b.add_clay(axis1_x0, y);
                    }
                } else {
                    for (int x = axis2_x0; x <= axis2_x1; ++x) {
                        b.add_clay(x, axis1_x0);
                    }
                }
            }
        }
    }

    world w = b.create_world();
    w.postprocess();

    // simulation is considered stable if this many cycles elapse
    // without a change in water population
    const int STABLE_THRESHOLD = 1000;

    int cycles_since_last_change = 0;
    int last_settled_sum = 0;

    for (;;) {
        w.update();

        int settled = w.count_settled_water();
        // std::cout << settled << std::endl;
        if (last_settled_sum != settled) {
            last_settled_sum = settled;
            cycles_since_last_change = 0;
            // std::cout << last_settled_sum << std::endl;
        }
        if (cycles_since_last_change >= STABLE_THRESHOLD) {
            break;
        }
        ++cycles_since_last_change;
    }

    std::ofstream output("day17result.txt");
    output << last_settled_sum << std::endl;
    w.dump(output);

    std::cout << w.count_tiles_touched() << std::endl;
    std::cout << w.count_settled_water() << std::endl;

    return 0;
}
