#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

bool large_cave(std::string_view name) {
    return std::ranges::all_of(name, [](char ch) {
        return std::isupper(ch);
    });
}

bool large_cave(uint8_t name) {
    return name >= 32;
}

class id_map {
public:
    using id_type = uint8_t;

    id_type get_or_create_id(std::string_view s) {
        if (auto it = ids.find(s); it != ids.end()) {
            return it->second;
        } else {
            if (large_cave(s)) {
                auto [it, _] = ids.insert_or_assign(std::string(s), create_large_id());
                return it->second;
            } else {
                auto [it, _] = ids.insert_or_assign(std::string(s), create_small_id());
                return it->second;
            }
        }
    }

private:
    id_type create_small_id() {
        id_type id = next_small_id++;
        if (id >= 32) {
            throw std::runtime_error("out of small ids");
        }
        return id;
    }

    id_type create_large_id() {
        id_type id = next_large_id++;
        if (id >= 64) {
            throw std::runtime_error("out of large ids");
        }
        return id;
    }

private:
    sr::unordered_string_map<id_type> ids;
    id_type next_small_id = 0;
    id_type next_large_id = 32;
};

struct bit_set {
    uint64_t bits = 0;

    bool on(uint8_t id) const {
        return (bits & (1ull << id)) > 0;
    }

    void set(uint8_t id) {
        bits |= (1ull << id);
    }
};

struct search_state {
    bit_set visited;
    id_map::id_type cursor;
    bool small_cave_twice = false;
};

struct evaluator_result {
    bool accept;
    bool set_small_cave_twice;
};

evaluator_result part1_evaluator(const search_state& s, uint8_t to) {
    return {large_cave(to) || !s.visited.on(to), false};
}

evaluator_result part2_evaluator(const search_state& s, uint8_t to) {
    if (large_cave(to)) {
        return {true, false};
    } else {
        if (!s.visited.on(to)) {
            return {true, false};
        } else if (s.visited.on(to) && !s.small_cave_twice) {
            return {true, true};
        }
    }
    return {false, true};
}

template <typename Evaluator>
size_t visit(const sr::graph<id_map::id_type>& g, id_map::id_type start, id_map::id_type end, Evaluator eval) {
    size_t path_count = 0;
    std::vector<search_state> states;
    std::vector<search_state> new_states;
    states.push_back(search_state{{}, start});
    while (states.size()) {
        // fork all states
        while (states.size()) {
            search_state s = states.back();
            states.pop_back();
            if (s.cursor == end) {
                ++path_count;
                continue;
            }
            s.visited.set(s.cursor);
            auto& e = g.edges.at(s.cursor);
            for (auto&& [to, _] : e) {
                evaluator_result result = eval(s, to);
                if (result.accept) {
                    auto& newstate = new_states.emplace_back();
                    newstate = s;
                    if (result.set_small_cave_twice) {
                        newstate.small_cave_twice = true;
                    }
                    newstate.cursor = to;
                }
            }
        }
        std::swap(states, new_states);
    }
    return path_count;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    sr::graph<id_map::id_type> map;
    id_map ids;

    for (const auto& line : sr::lines(input)) {
        std::string from, to;
        sr::parse(R"((\w+)\-(\w+))", line, from, to);
        map.get_or_create(ids.get_or_create_id(from));
        map.get_or_create(ids.get_or_create_id(to));
        map.add_edge(ids.get_or_create_id(from), ids.get_or_create_id(to), {});
        map.add_edge(ids.get_or_create_id(to), ids.get_or_create_id(from), {});
    }
    // remove edges pointing to start
    for (auto& [from, edges] : map.edges) {
        edges.erase(ids.get_or_create_id("start"));
    }
    // remove edges pointing from end
    map.edges.at(ids.get_or_create_id("end")).clear();

    sr::solution(visit(map, ids.get_or_create_id("start"), ids.get_or_create_id("end"), part1_evaluator));
    sr::solution(visit(map, ids.get_or_create_id("start"), ids.get_or_create_id("end"), part2_evaluator));

    return 0;
}
