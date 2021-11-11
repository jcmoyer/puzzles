#include <cassert>
#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <sstream>
#include <unordered_map>

#include <sr/sr.hpp>

struct recipe_input {
    std::string name;
    int64_t qty;
};

struct recipe {
    std::vector<recipe_input> inputs;
    std::string output_component;
    int64_t output_qty;
};

using recipe_index = std::unordered_map<std::string, recipe>;

struct component_stats {
    component_stats(std::string name_) : name{std::move(name_)} {}
    std::string name;
    int64_t total_available = 0;
    int64_t total_consumed = 0;
    int64_t total_produced = 0;
};

class component_journal {
public:
    std::unordered_map<std::string, component_stats> components;

    void populate_components(const recipe_index& recipes) {
        for (const auto& [k, v] : recipes) {
            if (!components.count(k)) {
                components.emplace(k, component_stats(k));
            }

            for (const auto& input : v.inputs) {
                components.emplace(input.name, component_stats(input.name));
            }
        }

        // ORE is special, it does not appear in the recipe list and can be produced infinitely for zero cost
        components.emplace("ORE", component_stats("ORE"));
    }

    void produce_one(const recipe_index& recipes, const std::string& name) {
        const recipe& rec = recipes.at(name);
        component_stats& output_stats = components.at(name);

        // consume each input, producing more inputs if required
        for (const auto& input : rec.inputs) {
            component_stats& stats = components.at(input.name);
            while (stats.total_available < input.qty) {
                // ORE can be produced infinitely
                if (input.name == "ORE") {
                    component_stats& ore = components.at("ORE");
                    ore.total_available += input.qty;
                    ore.total_produced += input.qty;
                } else {
                    produce_one(recipes, input.name);
                }
            }
            stats.total_available -= input.qty;
            stats.total_consumed += input.qty;
        }

        output_stats.total_available += rec.output_qty;
        output_stats.total_produced += rec.output_qty;
    }
};

void trim_left(std::string& s) {
    std::string::iterator it;
    for (it = s.begin(); *it == ' '; ++it) {
    }
    s.erase(s.begin(), it);
}

std::vector<std::string> split(const std::string& text, char delim) {
    std::stringstream ss(text);
    std::string part;
    std::vector<std::string> results;
    while (std::getline(ss, part, delim)) {
        results.push_back(part);
    }
    return results;
}

template <typename Stream>
recipe_index load_recipes(Stream&& stream) {
    recipe_index recipes;
    for (const auto& line : sr::lines(stream)) {
        std::string inputs, outputs;
        sr::parse("(.+) => (.+)$", line, inputs, outputs);

        int out_qty;
        std::string out_name;
        sr::parse("(\\d+) (\\w+)", outputs, out_qty, out_name);

        recipe rec;

        for (auto& input : split(inputs, ',')) {
            trim_left(input);
            int qty;
            std::string name;
            auto& i = rec.inputs.emplace_back();
            sr::parse("(\\d+) (\\w+)", input, qty, name);
            i.name = name;
            i.qty = qty;
        }

        rec.output_component = out_name;
        rec.output_qty = out_qty;
        recipes[out_name] = rec;
    }
    return recipes;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    recipe_index recipes = load_recipes(std::ifstream(filename));

    int64_t fuel_ore_cost;
    int64_t fuel_ore_byproduct;
    {
        component_journal cj;
        cj.populate_components(recipes);
        cj.produce_one(recipes, "FUEL");
        fmt::print("{}   {}\n", cj.components.at("ORE").total_produced, cj.components.at("FUEL").total_produced);
        fuel_ore_byproduct = cj.components.at("ORE").total_produced;
        fuel_ore_cost = cj.components.at("ORE").total_consumed;
    }

    // brute force method; need to optimize this
    {
        int64_t iter = 0;
        component_journal cj;
        cj.populate_components(recipes);
        while (cj.components.at("ORE").total_produced <= 1000000000000) {
            cj.produce_one(recipes, "FUEL");
            if (iter % 10000 == 0)
                fmt::print(
                    "{}   {}\n", cj.components.at("ORE").total_produced, cj.components.at("FUEL").total_produced);
            ++iter;
        }
        fmt::print("{}   {}\n", cj.components.at("ORE").total_produced, cj.components.at("FUEL").total_produced);
    }

    return 0;
}
