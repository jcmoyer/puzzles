#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector jolts = sr::read_lines<int>(input);

    const int charge_j = 0;
    const int device_j = *std::max_element(jolts.begin(), jolts.end()) + 3;

    jolts.push_back(charge_j);
    jolts.push_back(device_j);

    sr::graph<int> g;
    for (int j : jolts) {
        g.insert(j, {});
    }

    // Part 1: count number of differences in a valid chain using all adapters
    int ones = 0;
    int threes = 0;

    for (size_t i = 0; i < jolts.size(); ++i) {
        // Must form a valid chain; e.g. 1 2 4 should count as 1 one and 0 threes.
        if (g.contains(jolts[i] + 1)) {
            ++ones;
        } else {
            threes += g.contains(jolts[i] + 3);
        }
    }

    sr::solution(ones * threes);

    // Part 2: build edges and count the number of paths to the final node in the graph
    for (int j : jolts) {
        for (int i = 1; i <= 3; ++i) {
            if (g.contains(j + i)) {
                g.add_edge(j, j + i, {});
            }
        }
    }

    // iterate back-to-front and compute number of paths based on previous result
    std::sort(jolts.begin(), jolts.end());
    std::unordered_map<int, int64_t> paths;
    for (size_t i = jolts.size() - 1; i != -1; --i) {
        int key = jolts[i];
        auto edges = g.get_edges(key);
        if (edges.size() == 1 && edges[0].to == device_j) {
            // initial case - final adapter in the chain connects to device
            paths[key] = 1;
        } else {
            paths[key] = std::accumulate(edges.begin(), edges.end(), 0LL, [&](int64_t sum, auto&& edge) {
                return sum + paths[edge.to];
            });
        }
    }

    sr::solution(paths[charge_j]);

    return 0;
}
