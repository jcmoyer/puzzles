#include <fstream>
#include <iostream>
#include <numeric>
#include <vector>

struct node {
    std::vector<node> children;
    std::vector<int> metadata;
};

node read_node(std::istream& input) {
    int n_child;
    int n_meta;
    input >> n_child;
    input >> n_meta;

    node n;

    for (int i = 0; i < n_child; ++i) {
        n.children.push_back(read_node(input));
    }

    for (int i = 0; i < n_meta; ++i) {
        int val;
        input >> val;
        n.metadata.push_back(val);
    }

    return n;
}

template <typename F>
void visit_metadata(const node& root, const F& func) {
    for (int m : root.metadata)
        func(m);
    for (const node& n : root.children)
        visit_metadata(n, func);
}

int compute_value(const node& n) {
    if (n.children.size() == 0) {
        return std::reduce(n.metadata.begin(), n.metadata.end());
    }
    return std::reduce(n.metadata.begin(), n.metadata.end(), 0, [&n](int state, int metadata) {
        int index = metadata - 1;
        if (index >= 0 && index < n.children.size()) {
            return state + compute_value(n.children[index]);
        } else {
            return state;
        }
    });
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);

    auto root = read_node(input);
    int metadata_sum = 0;
    visit_metadata(root, [&metadata_sum](int metadata) {
        metadata_sum += metadata;
    });
    std::cout << metadata_sum << std::endl;

    std::cout << compute_value(root) << std::endl;

    return 0;
}
