#include <array>
#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

sr::array2d<char> rotate_ccw(const sr::array2d<char>& a) {
    // for an MxN matrix, the rotated form will have dimensions NxM
    sr::array2d<char> rotatedArray(a.height(), a.width());
    for (int y = 0; y < a.height(); ++y) {
        for (int x = 0; x < a.width(); ++x) {
            int dstX = y;
            int dstY = (a.width() - 1) - x;
            rotatedArray.at(dstX, dstY) = a.at(x, y);
        }
    }
    return rotatedArray;
}

sr::array2d<char> flip_x(const sr::array2d<char>& a) {
    // for an MxN matrix, the rotated form will have dimensions NxM
    sr::array2d<char> flipped(a.width(), a.height());
    for (int y = 0; y < a.height(); ++y) {
        for (int x = 0; x < a.width(); ++x) {
            int dstX = a.width() - x - 1;
            int dstY = y;
            flipped.at(dstX, dstY) = a.at(x, y);
        }
    }
    return flipped;
}

sr::array2d<char> flip_y(const sr::array2d<char>& a) {
    // for an MxN matrix, the rotated form will have dimensions NxM
    sr::array2d<char> flipped(a.width(), a.height());
    for (int y = 0; y < a.height(); ++y) {
        for (int x = 0; x < a.width(); ++x) {
            int dstX = x;
            int dstY = a.height() - y - 1;
            flipped.at(dstX, dstY) = a.at(x, y);
        }
    }
    return flipped;
}

struct extent2d {
    sr::vec2i min, max;
    sr::vec2i step;
};

extent2d extent_from_direction(sr::direction d, int w, int h) {
    extent2d ex;
    switch (d) {
    case sr::north:
        ex.min.x() = 0;
        ex.max.x() = w - 1;
        ex.min.y() = 0;
        ex.max.y() = 0;
        ex.step = {1, 0};
        break;
    case sr::south:
        ex.min.x() = 0;
        ex.max.x() = w - 1;
        ex.min.y() = h - 1;
        ex.max.y() = h - 1;
        ex.step = {1, 0};
        break;
    case sr::east:
        ex.min.x() = w - 1;
        ex.max.x() = w - 1;
        ex.min.y() = 0;
        ex.max.y() = h - 1;
        ex.step = {0, 1};
        break;
    case sr::west:
        ex.min.x() = 0;
        ex.max.x() = 0;
        ex.min.y() = 0;
        ex.max.y() = h - 1;
        ex.step = {0, 1};
        break;
    }
    return ex;
}

bool compare_edge(const sr::array2d<char>& a, const sr::array2d<char>& b, sr::direction side_a, sr::direction side_b) {
    extent2d ex_a = extent_from_direction(side_a, a.width(), a.height());
    extent2d ex_b = extent_from_direction(side_b, b.width(), b.height());

    sr::vec2i i = ex_a.min, j = ex_b.min;
    for (; i <= ex_a.max && j <= ex_b.max; i += ex_a.step, j += ex_b.step) {
        if (a.at(i.x(), i.y()) != b.at(j.x(), j.y())) {
            return false;
        }
    }
    return true;
}

struct edge_info {
    std::unordered_map<sr::direction, int64_t> shared;
};

std::string extract_edge(const sr::array2d<char>& a, sr::direction which) {
    auto ex = extent_from_direction(which, a.width(), a.height());
    std::string edge;
    for (auto i = ex.min; i <= ex.max; i += ex.step) {
        edge += a.at(i.x(), i.y());
    }
    return edge;
}

struct tile {
    sr::array2d<char> data;
    // std::array<size_t, 8> edges;

    std::unordered_set<std::string> edges;

    int id;

    bool solved = false;
    bool is_corner = false;
    bool is_outer = false;
    bool is_inner = false;

    std::vector<std::string> corner_edges;

    void trim_border() {
        sr::array2d<char> new_data(data.width() - 2, data.height() - 2);
        for (int y = 1; y < data.height() - 1; ++y) {
            for (int x = 1; x < data.width() - 1; ++x) {
                new_data.at(x - 1, y - 1) = data.at(x, y);
            }
        }
        data = new_data;
    }

    bool solve_for_edges(const std::unordered_set<std::string>& required_edges, size_t required_count = -1) {
        if (required_count == -1)
            required_count = required_edges.size();
        // initial rotation
        int i = 0;
        sr::array2d<char> buf = data;
        int sum = 0;
        for (auto&& e_here : edges) {
            sum += required_edges.contains(e_here);
        }
        if (sum >= required_count) {
            solved = true;
            return true;
        } else {
            return false;
        }
    }

    bool find_orientation_matching_edge(std::string_view required_edge, sr::direction position) {
        // initial rotation
        int i = 0;
        sr::array2d<char> buf = data;
        do {
            std::string edge_at_pos = extract_edge(buf, position);
            if (edge_at_pos == required_edge) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_x(data);
        do {
            std::string edge_at_pos = extract_edge(buf, position);
            if (edge_at_pos == required_edge) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_y(data);
        do {
            std::string edge_at_pos = extract_edge(buf, position);
            if (edge_at_pos == required_edge) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        return false;
    }

    bool find_orientation_matching_edge2(sr::unordered_string_map<sr::direction> required_edges) {
        // initial rotation
        int i = 0;
        sr::array2d<char> buf = data;
        do {
            int matches = 0;
            for (auto&& [edge, dir] : required_edges) {
                matches += edge == extract_edge(buf, dir);
            }
            if (matches == required_edges.size()) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_x(data);
        do {
            int matches = 0;
            for (auto&& [edge, dir] : required_edges) {
                matches += edge == extract_edge(buf, dir);
            }
            if (matches == required_edges.size()) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_y(data);
        do {
            int matches = 0;
            for (auto&& [edge, dir] : required_edges) {
                matches += edge == extract_edge(buf, dir);
            }
            if (matches == required_edges.size()) {
                data = buf;
                solved = true;
                return true;
            }
            buf = rotate_ccw(buf);
        } while (i++ < 3);
        return false;
    }
};

struct count_result {
    sr::array2d<char> map;
    int64_t roughness;
    int64_t monsters;
};

count_result count_sea_monsters(const sr::array2d<char>& a) {
    count_result result;

    // construct a set of offset vectors for the sea monster
    // clang-format off
        constexpr std::string_view sea_monster =
            "                  # \n"
            "#    ##    ##    ###\n"
            " #  #  #  #  #  #   \n";
    // clang-format on
    std::vector<sr::vec2i> monster_offsets;
    int x = 0, y = 0;
    for (int i = 0; i < sea_monster.size(); ++i) {
        if (sea_monster[i] == '#') {
            monster_offsets.push_back({x, y});
            ++x;
        } else if (sea_monster[i] == '\n') {
            ++y;
            x = 0;
        } else {
            ++x;
        }
    }

    // make a copy so we can mutate
    sr::array2d<char> map = a;
    int monsters_found = 0;
    for (size_t y = 0; y < map.height(); ++y) {
        for (size_t x = 0; x < map.width(); ++x) {
            bool found_monster = std::ranges::all_of(monster_offsets, [&](auto&& offset) {
                try {
                    return map.at(x + offset.x(), y + offset.y()) == '#';
                } catch (const std::out_of_range&) {
                    // tried to look beyond the bounds of the map
                    return false;
                }
            });

            if (found_monster) {
                std::ranges::for_each(monster_offsets, [&](auto&& offset) {
                    map.at(x + offset.x(), y + offset.y()) = 'X';
                });
                ++monsters_found;
            }
        }
    }

    // fmt::print("found {} monsters\n", monsters_found);

    result.map = std::move(map);
    result.monsters = monsters_found;
    result.roughness = std::ranges::count(result.map, '#');
    return result;
}

void dump_array(std::ostream& file, const sr::array2d<char>& charmap, int spacing = 8) {
    for (size_t y = 0; y < charmap.height(); ++y) {
        for (size_t x = 0; x < charmap.height(); ++x) {
            file << charmap.at(x, y);
            if ((x + 1) % spacing == 0)
                file << " ";
        }
        if ((y + 1) % spacing == 0)
            file << "\n";
        file << "\n";
    }
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<tile> tiles;

    std::vector<std::string> lines = sr::read_lines<std::string>(input);
    sr::split(lines.begin(), lines.end(), "", [&](auto first, auto last) {
        int id;
        sr::parse(R"(Tile (\d+?):)", *first, id);

        ++first;

        size_t w = first->size();
        size_t h = std::distance(first, last);
        sr::array2d<char> tile(w, h);
        size_t y = 0;

        while (first != last) {
            for (size_t x = 0; x < w; ++x) {
                tile.at(x, y) = (*first)[x];
            }
            ++y;
            ++first;
        }

        auto& t = tiles.emplace_back();
        t.data = std::move(tile);
        t.id = id;
    });

    std::vector<std::string> edge_data;

    sr::unordered_string_map<size_t> edge_id_map;

    for (tile& t : tiles) {
        // initial rotation
        int i = 0;
        sr::array2d<char> buf = t.data;
        do {
            std::string n = extract_edge(buf, sr::north);
            std::string s = extract_edge(buf, sr::south);
            std::string w = extract_edge(buf, sr::west);
            std::string e = extract_edge(buf, sr::east);
            edge_data.emplace_back(n);
            edge_data.emplace_back(s);
            edge_data.emplace_back(w);
            edge_data.emplace_back(e);
            buf = rotate_ccw(buf);
            edge_id_map[n]++;
            edge_id_map[s]++;
            edge_id_map[w]++;
            edge_id_map[e]++;
            t.edges.insert(n);
            t.edges.insert(s);
            t.edges.insert(w);
            t.edges.insert(e);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_x(t.data);
        do {
            std::string n = extract_edge(buf, sr::north);
            std::string s = extract_edge(buf, sr::south);
            std::string w = extract_edge(buf, sr::west);
            std::string e = extract_edge(buf, sr::east);
            edge_data.emplace_back(n);
            edge_data.emplace_back(s);
            edge_data.emplace_back(w);
            edge_data.emplace_back(e);
            buf = rotate_ccw(buf);
            edge_id_map[n]++;
            edge_id_map[s]++;
            edge_id_map[w]++;
            edge_id_map[e]++;
            t.edges.insert(n);
            t.edges.insert(s);
            t.edges.insert(w);
            t.edges.insert(e);
        } while (i++ < 3);
        // flipped
        i = 0;
        buf = flip_y(t.data);
        do {
            std::string n = extract_edge(buf, sr::north);
            std::string s = extract_edge(buf, sr::south);
            std::string w = extract_edge(buf, sr::west);
            std::string e = extract_edge(buf, sr::east);
            edge_data.emplace_back(n);
            edge_data.emplace_back(s);
            edge_data.emplace_back(w);
            edge_data.emplace_back(e);
            buf = rotate_ccw(buf);
            edge_id_map[n]++;
            edge_id_map[s]++;
            edge_id_map[w]++;
            edge_id_map[e]++;
            t.edges.insert(n);
            t.edges.insert(s);
            t.edges.insert(w);
            t.edges.insert(e);
        } while (i++ < 3);
    }

    std::vector<size_t> corners;

    int64_t prod = 1;
    for (size_t i = 0; i < tiles.size(); ++i) {
        tile& t = tiles[i];
        int sixes = 0;
        std::vector<std::string> correct_edges;
        for (auto&& edge : t.edges) {
            if (edge_id_map.at(edge) == 6) {
                ++sixes;
                correct_edges.push_back(edge);
            }
        }
        if (sixes == 4) {
            prod *= t.id;
            t.corner_edges = std::move(correct_edges);
            corners.push_back(i);
            // printf("corner %d\n", t.id);
            t.is_corner = true;
        } else if (sixes == 2) {
            // printf("outer %d\n", t.id);
            t.is_outer = true;
        } else {
            // printf("inner %d\n", t.id);
            t.is_inner = true;
        }
    }
    sr::solution(prod);

    // pick a corner

    tile* tile0 = &tiles[corners[0]];
    for (int i = 0; i < 4; ++i) {
        tile0 = &tiles[corners[i]];
        // try to solve the "top" and "left" edges (may be incorrectly rotated but this is necessary for how we fill
        // the map in later)

        std::string e0 = tile0->corner_edges[0];
        std::string e0r(e0.rbegin(), e0.rend());
        tile0->edges.erase(e0r);
        std::erase(tile0->corner_edges, e0r);
        auto e1i = std::ranges::find_if(tile0->corner_edges, [&](auto&& edge) {
            return edge != e0;
        });
        std::string e1r(e1i->rbegin(), e1i->rend());
        tile0->edges.erase(e1r);
        std::erase(tile0->corner_edges, e1r);

        if (!tile0->find_orientation_matching_edge2({{e0, sr::north}, {*e1i, sr::west}})) {
            if (!tile0->find_orientation_matching_edge2({{e0, sr::north}, {e1r, sr::west}})) {
            }
        }
        if (tile0->solved) {
            break;
        }
    }

    if (!tile0->solved) {
        throw std::runtime_error("uhhh");
    }
    // now tile0 is ready to be used to find adjacent tiles
    // unfortunately we don't know which axis is which

    size_t dimension = (size_t)std::sqrt(tiles.size());
    sr::array2d<tile*> map(dimension, dimension);
    map.at(0, 0) = tile0;

    // pass 1: solve outers
    for (size_t y = 0; y < map.height(); ++y) {
        for (size_t x = 0; x < map.width(); ++x) {
            tile* up = y > 0 ? map.at(x, y - 1) : nullptr;
            tile* left = x > 0 ? map.at(x - 1, y) : nullptr;

            for (size_t i = 0; i < tiles.size(); ++i) {
                tile& t = tiles[i];
                if (left && left->id != t.id && !t.solved) {
                    if (t.find_orientation_matching_edge(extract_edge(left->data, sr::east), sr::west)) {
                        map.at(x, y) = &t;
                        break;
                    }
                } else if (up && up->id != t.id && !t.solved) {
                    if (t.find_orientation_matching_edge(extract_edge(up->data, sr::south), sr::north)) {
                        map.at(x, y) = &t;
                        break;
                    }
                }
            }
        }
    }

    std::ranges::for_each(tiles, [](auto&& t) {
        t.trim_border();
    });

    int tile_width = tile0->data.width();
    int tile_height = tile0->data.height();
    sr::array2d<char> charmap(tile_width * dimension, tile_height * dimension);
    for (size_t ty = 0; ty < map.height(); ++ty) {
        for (size_t tx = 0; tx < map.width(); ++tx) {
            for (size_t ry = 0; ry < tile_height; ++ry) {
                for (size_t rx = 0; rx < tile_width; ++rx) {
                    size_t x = tx * tile_width + rx;
                    size_t y = ty * tile_height + ry;

                    charmap.at(x, y) = map.at(tx, ty)->data.at(rx, ry);
                }
            }
        }
    }

    sr::array2d<char> buf = charmap;
    int i = 0;
    do {
        auto r = count_sea_monsters(buf);
        if (r.monsters) {
            sr::solution(r.roughness);
            return 0;
        }
        buf = rotate_ccw(buf);
    } while (i++ < 3);
    buf = flip_x(charmap);
    i = 0;
    do {
        auto r = count_sea_monsters(buf);
        if (r.monsters) {
            sr::solution(r.roughness);
            return 0;
        }
        buf = rotate_ccw(buf);
    } while (i++ < 3);
    buf = flip_y(charmap);
    i = 0;
    do {
        auto r = count_sea_monsters(buf);
        if (r.monsters) {
            sr::solution(r.roughness);
            return 0;
        }
        buf = rotate_ccw(buf);
    } while (i++ < 3);

    return 0;
}
