#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

enum node_type { pair, num };

struct tree_node {
    node_type ty;
    size_t parent = -1; // -1 is no parent
    size_t left, right;
    int64_t val;
};

struct tree_context {
    std::vector<tree_node> nodes;
    size_t create_node(node_type ty, size_t parent = -1) {
        auto& n = nodes.emplace_back();
        n.ty = ty;
        n.parent = parent;
        return nodes.size() - 1;
    }
    tree_node& get_node(size_t h) {
        return nodes[h];
    }
    // invalidates all node handles
    void clear() {
        nodes.clear();
    }
};

size_t parse_one(tree_context& tc, std::string_view& s, size_t parent);

size_t parse_digits(tree_context& tc, std::string_view& s, size_t parent) {
    size_t h = tc.create_node(num, parent);
    std::string buf;
    while (isdigit(s[0])) {
        buf += s[0];
        s.remove_prefix(1);
    }
    tc.nodes[h].val = std::stoi(buf);
    return h;
}

size_t parse_pair(tree_context& tc, std::string_view& s, size_t parent) {
    size_t h = tc.create_node(pair, parent);
    assert(s[0] == '[');
    s.remove_prefix(1);
    tc.nodes[h].left = parse_one(tc, s, h);
    assert(s[0] == ',');
    s.remove_prefix(1);
    tc.nodes[h].right = parse_one(tc, s, h);
    assert(s[0] == ']');
    s.remove_prefix(1);
    return h;
}

size_t parse_one(tree_context& tc, std::string_view& s, size_t parent) {
    char first = s.at(0);
    if (first == '[') {
        return parse_pair(tc, s, parent);
    } else if (std::isdigit(first)) {
        return parse_digits(tc, s, parent);
    } else {
        throw std::runtime_error("unexpected char");
    }
}

int64_t magnitude(const tree_context& tc, size_t node) {
    const auto& n = tc.nodes[node];
    switch (n.ty) {
    case num:
        return n.val;
    case pair:
        return 3 * magnitude(tc, n.left) + 2 * magnitude(tc, n.right);
    }
    throw std::runtime_error("invalid type");
}

size_t combine_nodes(tree_context& tc, size_t left, size_t right) {
    size_t new_node = tc.create_node(pair);
    tc.nodes[new_node].left = left;
    tc.nodes[new_node].right = right;
    tc.nodes[left].parent = new_node;
    tc.nodes[right].parent = new_node;
    return new_node;
}

void print_tree(tree_context& tc, size_t node) {
    const auto& n = tc.nodes[node];
    switch (n.ty) {
    case num:
        printf("%d", n.val);
        break;
    case pair:
        printf("[");
        print_tree(tc, n.left);
        printf(",");
        print_tree(tc, n.right);
        printf("]");
        break;
    }
}

enum search_bias {
    bias_left,
    bias_right,
};

size_t search_number_down(tree_context& tc, size_t from, search_bias b) {
    const auto& n = tc.nodes[from];
    switch (n.ty) {
    case num:
        return from;
    case pair:
        if (b == bias_left) {
            size_t left_num = search_number_down(tc, n.left, b);
            if (left_num != -1)
                return left_num;
            size_t right_num = search_number_down(tc, n.right, b);
            if (right_num != -1)
                return right_num;
        } else {
            size_t right_num = search_number_down(tc, n.right, b);
            if (right_num != -1)
                return right_num;
            size_t left_num = search_number_down(tc, n.left, b);
            if (left_num != -1)
                return left_num;
        }
    }
    return -1;
}

size_t search_left_number(tree_context& tc, size_t from) {
    auto& n = tc.nodes[from];
    if (n.parent == -1)
        return -1;
    auto& p = tc.nodes[n.parent];
    if (p.ty == pair) {
        if (tc.nodes[p.left].ty == num)
            return p.left;
        else if (tc.nodes[p.left].ty == pair && p.left != from) {
            size_t num = search_number_down(tc, p.left, bias_right);
            if (num != -1)
                return num;
        }
        return search_left_number(tc, n.parent);
    } else {
        return search_left_number(tc, n.parent);
    }
}

size_t search_right_number(tree_context& tc, size_t from) {
    auto& n = tc.nodes[from];
    if (n.parent == -1)
        return -1;
    auto& p = tc.nodes[n.parent];
    if (p.ty == pair) {
        if (tc.nodes[p.right].ty == num)
            return p.right;
        else if (tc.nodes[p.right].ty == pair && p.right != from) {
            size_t num = search_number_down(tc, p.right, bias_left);
            if (num != -1)
                return num;
        }
        return search_right_number(tc, n.parent);
    } else {
        return search_right_number(tc, n.parent);
    }
}

bool try_explode(tree_context& tc, size_t node, int nest = 0) {
    // If any pair is nested inside four pairs, the leftmost such pair explodes.
    switch (tc.get_node(node).ty) {
    case num:
        break;
    case pair:
        if (nest == 4) {
            // Exploding pairs will always consist of two regular numbers.
            size_t left_num = search_left_number(tc, node);
            if (left_num != -1) {
                tc.nodes[left_num].val += tc.nodes[tc.get_node(node).left].val;
            }

            size_t right_num = search_right_number(tc, node);
            if (right_num != -1) {
                tc.nodes[right_num].val += tc.nodes[tc.get_node(node).right].val;
            }

            // Then, the entire exploding pair is replaced with the regular number 0.
            size_t replace = tc.create_node(num, tc.get_node(node).parent);
            tc.nodes[replace].val = 0;
            if (tc.nodes[tc.get_node(node).parent].left == node) {
                tc.nodes[tc.get_node(node).parent].left = replace;
            } else {
                tc.nodes[tc.get_node(node).parent].right = replace;
            }
            return true;

            break;

        } else {
            if (try_explode(tc, tc.get_node(node).left, nest + 1))
                return true;
            if (try_explode(tc, tc.get_node(node).right, nest + 1))
                return true;
        }
        break;
    }
    return false;
}

bool try_split(tree_context& tc, size_t node) {
    // If any pair is nested inside four pairs, the leftmost such pair explodes.
    switch (tc.get_node(node).ty) {
    case num:
        if (tc.get_node(node).val >= 10) {
            double base_val = tc.get_node(node).val / 2.0;
            int new_left = std::floor(base_val);
            int new_right = std::ceil(base_val);

            size_t replace = tc.create_node(pair, tc.get_node(node).parent);
            if (tc.nodes[tc.get_node(node).parent].left == node) {
                tc.nodes[tc.get_node(node).parent].left = replace;
            } else {
                tc.nodes[tc.get_node(node).parent].right = replace;
            }
            tc.nodes[replace].left = tc.create_node(num, replace);
            tc.nodes[replace].right = tc.create_node(num, replace);
            tc.nodes[tc.get_node(replace).left].val = new_left;
            tc.nodes[tc.get_node(replace).right].val = new_right;
            return true;
        }
        break;
    case pair:
        if (try_split(tc, tc.get_node(node).left))
            return true;
        if (try_split(tc, tc.get_node(node).right))
            return true;
        break;
    }
    return false;
}

void reduce_tree(tree_context& tc, size_t root) {
    while (true) {
        // print_tree(tc, root);
        // std::cout << "\n";
        if (!try_explode(tc, root)) {
            if (!try_split(tc, root)) {
                break;
            }
        }
    }
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    tree_context tc;

    std::vector<size_t> roots;
    std::vector<std::string> lines = sr::read_lines<std::string>(input, sr::ignore_empty);

    // part 1
    for (auto& line : lines) {
        std::string_view view(line);
        roots.push_back(parse_one(tc, view, -1));
        if (roots.size() == 2) {
            size_t new_root = combine_nodes(tc, roots[0], roots[1]);
            roots.clear();
            roots.push_back(new_root);
            reduce_tree(tc, new_root);
        }
    }
    sr::solution(magnitude(tc, roots[0]));

    // part 2
    int64_t max_mag = 0;
    for (size_t i = 0; i < lines.size(); ++i) {
        for (size_t j = 0; j < lines.size(); ++j) {
            if (i == j)
                continue;
            roots.clear();
            tc.clear();
            std::string_view view;
            view = lines[i];
            roots.push_back(parse_one(tc, view, -1));
            view = lines[j];
            roots.push_back(parse_one(tc, view, -1));
            size_t new_root = combine_nodes(tc, roots[0], roots[1]);
            reduce_tree(tc, new_root);
            max_mag = std::max(max_mag, magnitude(tc, new_root));
        }
    }
    sr::solution(max_mag);

    return 0;
}
