#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

enum hex_direction {
    northeast,
    northwest,
    southeast,
    southwest,
    east,
    west,
    max_dir,
};

sr::vec2i delta_vector(hex_direction d) {
    switch (d) {
    case northeast:
        return {1, 1};
    case northwest:
        return {0, 1};
    case southeast:
        return {1, -1};
    case southwest:
        return {0, -1};
    case east:
        return {1, 0};
    case west:
        return {-1, 0};
    }
    throw std::runtime_error("invalid direction");
}

sr::vec2i hex_move(const sr::vec2i& pos, hex_direction d) {
    auto delta = delta_vector(d);
    auto dest = pos + delta;
    // odd rows are offset
    if (delta.y() != 0 && std::abs(dest.y()) % 2 == 1)
        --dest.x();
    return dest;
}

enum tile_color { white, black };

struct tile {
    tile_color color = white;

    void flip() {
        if (color == white)
            color = black;
        else
            color = white;
    }
};

struct color_count_result {
    int adjacent_white = 0, adjacent_black = 0;
};

using direction_list = std::vector<hex_direction>;

class tilemap {
public:
    tile_color color_at(const sr::vec2i& pos) const {
        if (auto it = tiles.find(pos); it != tiles.end()) {
            return it->second.color;
        } else {
            return white;
        }
    }

    void flip_at(const sr::vec2i& pos) {
        tiles[pos].flip();
    }

    color_count_result count_adjacent_colors(const sr::vec2i& pos) const {
        color_count_result result;
        for (int i = 0; i < max_dir; ++i) {
            auto it = tiles.find(hex_move(pos, (hex_direction)i));
            tile_color c = white;

            if (it != tiles.end())
                c = it->second.color;

            if (c == white)
                ++result.adjacent_white;
            else
                ++result.adjacent_black;
        }
        return result;
    }

    void step_ca() {
        std::unordered_map<sr::vec2i, tile> new_map;

        std::unordered_set<sr::vec2i> check_positions;
        for (auto& [pos, tile] : tiles) {
            if (tile.color == black) {
                check_positions.insert(pos);
                for (int i = 0; i < max_dir; ++i) {
                    check_positions.insert(hex_move(pos, (hex_direction)i));
                }
            }
        }

        for (auto& pos : check_positions) {
            auto adj = count_adjacent_colors(pos);

            auto it = tiles.find(pos);
            tile_color color_here = white;
            if (it != tiles.end())
                color_here = it->second.color;

            if (color_here == black) {
                if (adj.adjacent_black == 0 || adj.adjacent_black > 2) {
                    // no need to write white here since tiles not present are already counted as white
                } else {
                    new_map[pos].color = black;
                }
            } else if (color_here == white) {
                if (adj.adjacent_black == 2) {
                    new_map[pos].color = black;
                }
            }
        }

        tiles = new_map;
    }

    size_t count_tiles(tile_color c) const {
        return std::ranges::count_if(tiles, [=](auto&& kvp) {
            return kvp.second.color == c;
        });
    }

private:
    std::unordered_map<sr::vec2i, tile> tiles;
};

direction_list parse_directions(std::string_view line) {
    direction_list dir;
    for (size_t i = 0; i < line.size(); ++i) {
        if (line[i] == 'e') {
            dir.push_back(east);
        } else if (line[i] == 'w') {
            dir.push_back(west);
        } else if (line[i] == 'n') {
            ++i;
            if (line[i] == 'e') {
                dir.push_back(northeast);
            } else if (line[i] == 'w') {
                dir.push_back(northwest);
            }
        } else if (line[i] == 's') {
            ++i;
            if (line[i] == 'e') {
                dir.push_back(southeast);
            } else if (line[i] == 'w') {
                dir.push_back(southwest);
            }
        }
    }
    return dir;
}

sr::vec2i walk_directions(const direction_list& dir, sr::vec2i pos = {}) {
    for (auto& d : dir) {
        pos = hex_move(pos, d);
    }
    return pos;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    tilemap map;

    for (const auto& line : sr::lines(input)) {
        direction_list dir = parse_directions(line);
        map.flip_at(walk_directions(dir));
    }

    sr::solution(map.count_tiles(black));

    for (int i = 0; i < 100; ++i) {
        map.step_ca();
    }

    sr::solution(map.count_tiles(black));

    return 0;
}
