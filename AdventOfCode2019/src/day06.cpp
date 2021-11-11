#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

struct object {
    object(std::string n) : name{std::move(n)} {}

    std::string name;

    std::string orbitee;
    std::vector<std::string> orbiters;
};

struct search {
    std::string name;
    int dist;
};

class object_graph {
public:
    object& get_or_create_obj(const std::string& name) {
        if (auto it = m.find(name); it != m.end()) {
            return it->second;
        } else {
            auto [new_it, succ] = m.emplace(std::make_pair(name, object{name}));
            return new_it->second;
        }
    }

    const object& obj(const std::string& name) const {
        return m.at(name);
    }

    void add_edge(std::string orbiter, std::string orbitee) {
        object& orbiter_ob = get_or_create_obj(orbiter);
        object& orbitee_ob = get_or_create_obj(orbitee);
        orbitee_ob.orbiters.push_back(std::move(orbiter));
        orbiter_ob.orbitee = std::move(orbitee);
    }

    int count_total_orbits(std::string objname) const {
        int total_here = 0;
        while (objname != "COM") {
            auto it = m.find(objname);
            if (it == m.end()) {
                throw std::runtime_error("edge leads to object not in graph");
            }
            objname = it->second.orbitee;
            ++total_here;
        }
        return total_here;
    }

    int count_total_orbits() const {
        int sum = 0;
        for (const auto& [k, v] : m) {
            if (k == "COM")
                continue;

            sum += count_total_orbits(k);
        }
        return sum;
    }

    int count_object_distance(std::string obj1, std::string obj2) const {
        std::deque<search> frontier;
        std::unordered_set<std::string> visited;

        const auto& start_obj = obj(obj1);
        frontier.push_back({start_obj.orbitee, 1});
        for (const auto& name : start_obj.orbiters) {
            frontier.push_back({name, 1});
        }

        visited.emplace(obj1);

        while (frontier.size()) {
            search current_search = frontier.front();
            frontier.pop_front();

            visited.insert(current_search.name);

            if (current_search.name == obj2) {
                return current_search.dist;
            }

            const auto& current_obj = obj(current_search.name);
            if (current_obj.name != "COM" && !visited.count(current_obj.orbitee))
                frontier.push_back({current_obj.orbitee, 1 + current_search.dist});

            for (const auto& name : current_obj.orbiters) {
                if (!visited.count(name))
                    frontier.push_back({name, 1 + current_search.dist});
            }
        }

        throw std::runtime_error("could not find path from obj1 to obj2");
    }

private:
    std::unordered_map<std::string, object> m;
};

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);

    object_graph g;

    for (const auto& line : sr::lines(input)) {
        std::string orbitee;
        std::string orbiter;

        sr::parse(R"(([\d\w]+)\)([\d\w]+)$)", line, orbitee, orbiter);

        g.add_edge(std::move(orbiter), std::move(orbitee));
    }

    fmt::print("{}\n", g.count_total_orbits());
    fmt::print("{}\n", g.count_object_distance(g.obj("YOU").orbitee, g.obj("SAN").orbitee));

    return 0;
}
