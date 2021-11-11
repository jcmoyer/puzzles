#include <deque>
#include <fstream>
#include <numeric>
#include <unordered_set>

#include <sr/sr.hpp>

struct bag_edge {
    int qty;
};

using bag_graph = sr::graph<std::string, sr::empty, bag_edge, sr::string_hash, sr::string_equal>;

const std::regex bag_def_re(R"((.+?) bags contain (.+?)\.)");
const std::regex containee_re(R"((\d+?) (.+?) bags?)");

int part1(const bag_graph& g) {
    auto rng = g.back_dfs("shiny gold");
    return (int)std::distance(rng.begin(), rng.end()) - 1;
}

int part2(const bag_graph& g, const std::string& start = "shiny gold") {
    auto edges = g.get_edges(start);
    return std::accumulate(edges.begin(), edges.end(), 0, [&](int sum, auto&& edge) {
        return sum + edge.props.qty + edge.props.qty * part2(g, edge.to);
    });
}

void process_bag_definition(bag_graph& g, std::string_view str) {
    std::string_view container;
    std::string containee_str;
    sr::parse(bag_def_re, str, container, containee_str);

    auto& container_info = g.get_or_create(container);
    auto first = std::sregex_iterator(containee_str.begin(), containee_str.end(), containee_re);
    auto last = std::sregex_iterator();
    for (auto it = first; it != last; ++it) {
        if (std::smatch m = *it; !m.empty()) {
            int qty = std::stoi(m.str(1));
            std::string subbag_name = m.str(2);
            g.get_or_create(subbag_name);
            g.add_edge(container, subbag_name, {.qty = qty});
        }
    }
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    bag_graph g;

    for (const auto& line : sr::lines(input)) {
        process_bag_definition(g, line);
    }

    sr::solution(part1(g));
    sr::solution(part2(g));

    return 0;
}
