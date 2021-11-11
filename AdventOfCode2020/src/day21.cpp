#include <fstream>
#include <numeric>
#include <ranges>
#include <string>
#include <string_view>

#include <sr/sr.hpp>

struct food {
    sr::unordered_string_set<> ingredients;
    sr::unordered_string_set<> allergens;

    bool contains_ingredient(std::string_view name) const {
        return ingredients.contains(name);
    }

    bool contains_allergen(std::string_view name) const {
        return allergens.contains(name);
    }
};

struct everything {
    std::vector<food> foods;
    sr::unordered_string_set<> ingredients;
    sr::unordered_string_set<> allergens;

    food& add_food() {
        return foods.emplace_back();
    }

    void insert_ingredient(std::string_view name) {
        ingredients.emplace(name);
    }
};

// solver based on the hand solved solution
//
// tl;dr:
//
// rows = ingredients
// cols = allergens
//
// populate the matrix with ones where an ingredient doesn't contain an allergen
// call .solve() for your solution
//
// all this does is looks for rows with a single zero, then fills that column with ones
// until no more rows were filled, in which case you have a solution or the input was ambiguous
// whenever this happens, that row is marked as associated with the column that contained a hole

struct solver_result {
    sr::unordered_string_map<std::string> associated;
    sr::unordered_string_set<> unassociated;
};

class ingredient_allergen_matrix {
public:
    template <typename IngredientRng, typename AllergenRng>
    ingredient_allergen_matrix(IngredientRng ingredients, AllergenRng allergens) {
        declare_ingredients(ingredients);
        declare_allergens(allergens);
        create_matrix();
    }

    void set(std::string_view ig_name, std::string_view al_name, bool val) {
        auto ig_it = ig_map.find(ig_name);
        auto al_it = al_map.find(al_name);
        if (ig_it == ig_map.end() || al_it == al_map.end()) {
            throw std::runtime_error("bad ingredient or allergen name");
        }
        matrix.at(al_it->second, ig_it->second) = val;
    }

    solver_result solve() {
        solver_result result;

        while (iterate(result))
            ;

        /*if (associated.size() != matrix.height())
            throw std::runtime_error("unable to solve, not enough information to disambiguate");*/

        // aggregate all of the results that were never associated
        for (const auto& ig_name : ig_names) {
            if (!result.associated.contains(ig_name)) {
                result.unassociated.insert(ig_name);
            }
        }

        return result;
    }

private:
    template <typename IngredientRng>
    void declare_ingredients(IngredientRng names) {
        size_t index = 0;
        std::ranges::for_each(names, [&](const auto& name) {
            ig_names.emplace_back(name);
            ig_map[name] = index++;
        });
    }

    template <typename AllergenRng>
    void declare_allergens(AllergenRng names) {
        size_t index = 0;
        std::ranges::for_each(names, [&](const auto& name) {
            al_names.emplace_back(name);
            al_map[name] = index++;
        });
    }

    void create_matrix() {
        matrix.resize(al_map.size(), ig_map.size());
    }

    size_t find_single_hole(size_t row) const {
        size_t last_hole = -1;
        for (size_t al = 0; al < matrix.width(); ++al) {
            if (matrix.at(al, row) == 0) {
                if (last_hole == -1)
                    last_hole = al;
                else
                    // found a second hole, so this row is still ambiguous
                    return -1;
            }
        }
        return last_hole;
    }

    bool iterate(solver_result& result) {
        bool solved_row = false;

        // look for rows with a single empty column
        for (size_t ig = 0; ig < matrix.height(); ++ig) {
            size_t hole = find_single_hole(ig);

            // fill that column with ones to remove it from the set of possibilities
            // and associate it with the row we found it in
            if (hole != -1) {
                for (size_t i = 0; i < matrix.height(); ++i) {
                    matrix.at(hole, i) = true;
                }
                result.associated[ig_names[ig]] = al_names[hole];
                solved_row = true;
            }
        }

        return solved_row;
    }

    std::vector<std::string> ig_names;
    std::vector<std::string> al_names;
    sr::unordered_string_map<size_t> ig_map;
    sr::unordered_string_map<size_t> al_map;
    // TODO: lib defect, array2d uses vector internally so we can't use bool here :V
    sr::array2d<uint_fast32_t> matrix;
};

everything load_everything(std::istream& input) {
    everything every;
    for (const auto& line : sr::lines(input)) {
        std::string_view ingreds_str;
        std::string_view alerg_str;
        sr::parse(R"((.+?) \(contains (.+?)\))", line, ingreds_str, alerg_str);

        auto& f = every.add_food();

        // TODO: lib defect, support splitting on subrange so we don't have to strip whitespace after the fact
        sr::split(alerg_str, ',', [&](auto first, auto last) {
            std::string allerg_name(first, last);

            allerg_name.erase(std::remove(allerg_name.begin(), allerg_name.end(), ' '), allerg_name.end());
            f.allergens.insert(allerg_name);
            every.allergens.insert(std::move(allerg_name));
        });

        sr::split(ingreds_str, ' ', [&](auto first, auto last) {
            every.insert_ingredient(std::string_view(first, last));
            f.ingredients.insert(std::string(first, last));
        });
    }
    return every;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);
    everything every = load_everything(input);

    ingredient_allergen_matrix m(every.ingredients, every.allergens);

    // tell the solver what we know
    for (auto&& f : every.foods) {
        // the ingredients missing from this food cannot contain the allergens associated with it
        sr::unordered_string_set<> missing;

        for (const auto& ig : every.ingredients) {
            if (!f.contains_ingredient(ig)) {
                missing.emplace(ig);
            }
        }
        // however, it's possible the ingredient is any of the others
        // so we set the cells it's not possible to be in the matrix to 1 (i.e. already-solved)
        for (const auto& ig : missing) {
            for (const auto& al : f.allergens) {
                m.set(ig, al, true);
            }
        }
    }

    // solve p1 and p2 simultaneously
    auto result = m.solve();

    // p1
    sr::solution(sr::accumulate(result.unassociated, 0ll, [&](int64_t sum, auto&& ig) {
        return sum + sr::accumulate(every.foods, 0ll, [&](int64_t sum, auto&& f) {
            return sum + f.contains_ingredient(ig);
        });
    }));

    // p2
    std::vector<std::string> dangerous;
    for (auto&& kvp : result.associated) {
        dangerous.emplace_back(kvp.first);
    }

    // sort dangerous ingredients by associated allergen
    std::ranges::sort(dangerous, [&](auto&& d0, auto&& d1) {
        return result.associated[d0] < result.associated[d1];
    });

    sr::solution(sr::join(dangerous, ','));

    return 0;
}
