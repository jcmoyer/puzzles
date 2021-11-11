#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <sstream>

#include <sr/sr.hpp>

struct int_range {
    int min, max;

    bool contains(int x) const {
        return x >= min && x <= max;
    }
};

struct property_value {
    int_range r0, r1;

    bool contains(int x) const {
        return r0.contains(x) || r1.contains(x);
    }
};

template <typename T>
void split_parse(std::string_view s, std::vector<T>& elems, char delim) {
    sr::split(s.data(), s.data() + s.size(), delim, [&](auto first, auto last) {
        elems.emplace_back(sr::parse_type<T>::parse(first, last));
    });
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::unordered_map<std::string, property_value> props;
    std::vector<int> your_ticket;
    std::vector<std::vector<int>> nearby_ticket;

    for (const auto& line : sr::lines(input)) {
        try {
            std::string propname;
            int_range r0, r1;

            sr::parse(R"((.+?): (\d+?)\-(\d+?) or (\d+?)\-(\d+?))", line, propname, r0.min, r0.max, r1.min, r1.max);

            props[propname] = {r0, r1};
        } catch (const sr::bad_match&) {
            if (line.find(',') != line.npos) {
                if (your_ticket.size() == 0) {
                    split_parse<int>(line, your_ticket, ',');
                } else {
                    split_parse<int>(line, nearby_ticket.emplace_back(), ',');
                }
            }
        }
    }

    // remove definitely invalid tickets and count the invalid cells for part 1
    int64_t sum = 0;
    for (auto it = nearby_ticket.begin(); it != nearby_ticket.end();) {
        bool valid_ticket = true;
        for (size_t i = 0; i < (*it).size(); ++i) {
            bool any_in_range = false;
            for (const auto& [name, p] : props) {
                if (p.contains((*it)[i])) {
                    any_in_range = true;
                }
            }
            if (!any_in_range) {
                sum += (*it)[i];
                valid_ticket = false;
            }
        }
        if (valid_ticket)
            ++it;
        else
            it = nearby_ticket.erase(it);
    }
    sr::solution(sum);

    // for each column, go down each row and count how many cells are valid for each property
    // total_counts[i]["property"] where i is the column ID, "property" is the property name, and the result is the
    // number cells in range for that property
    std::vector<std::unordered_map<std::string, int>> total_counts;
    for (int col = 0; col < 20; ++col) {
        std::unordered_map<std::string, int> counts;

        for (int row = 0; row < nearby_ticket.size(); ++row) {
            for (const auto& [name, p] : props) {
                if (p.contains(nearby_ticket[row][col]))
                    ++counts[name];
            }
        }

        total_counts.emplace_back(std::move(counts));
    }

    // now that we know how many cells are valid for each property in each column, we can filter the dataset to only
    // possible candidates for property-column mapping
    //
    // we know which columns are possible candidates because the valid row count == total row count
    // that is, if there are 190 rows, and there are only 185 cell matches for a column, one of the values was out of
    // range therefore it cannot possibly be associated with that property
    for (int col = 0; col < 20; ++col) {
        for (auto it = total_counts[col].begin(); it != total_counts[col].end();) {
            if (it->second != nearby_ticket.size())
                it = total_counts[col].erase(it);
            else
                ++it;
        }
    }

    // 1. look for columns where there is only one valid property name
    // 2. remove that property name from all columns, and add an association in colmap
    // 3. repeat until there are no more columns with only one valid property name
    // 4. as long as there's no ambiguity, colmap.size() == props.size() and maps a column name to its ID
    std::unordered_map<std::string, int> colmap;
    while (true) {
        for (int col = 0; col < 20; ++col) {
            if (total_counts[col].size() == 1) {
                std::string name = total_counts[col].begin()->first;
                colmap[name] = col;

                for (int col = 0; col < 20; ++col) {
                    total_counts[col].erase(name);
                }

                goto next;
            }
        }
        break;
    next:;
    }

    sr::solution(std::accumulate(colmap.begin(), colmap.end(), 1LL, [&](int64_t prod, auto&& kvp) {
        return prod * (kvp.first.starts_with("departure") ? your_ticket[kvp.second] : 1);
    }));

    return 0;
}
