#include <fstream>

#include <sr/sr.hpp>

class node_handle {
public:
    constexpr node_handle() {
        index = -1;
    }

    constexpr operator bool() const {
        return valid();
    }

    constexpr bool valid() const {
        return !invalid();
    }

    constexpr bool invalid() const {
        return index == -1;
    }

    constexpr auto operator<=>(const node_handle&) const = default;

private:
    constexpr explicit node_handle(size_t val) {
        index = val;
    }

    size_t index;
    friend class tree_context;
};

enum node_type { pair, num };

struct tree_node {
    node_handle parent;
    node_type ty{};
    union {
        // when ty == pair
        struct {
            node_handle left, right;
        };
        // when ty == num
        uint32_t val{};
    };
};

struct tree_context {
public:
    constexpr node_handle create_node(node_type ty, node_handle parent = {}) {
        auto& n = nodes.emplace_back();
        n.ty = ty;
        n.parent = parent;
        return node_handle{nodes.size() - 1};
    }

    constexpr void replace_node(node_handle h, node_type ty) {
        assert(h.valid());
        nodes[h.index].ty = ty;
    }

    constexpr tree_node& operator[](node_handle h) {
        assert(h.valid());
        return nodes[h.index];
    }

    constexpr const tree_node& operator[](node_handle h) const {
        assert(h.valid());
        return nodes[h.index];
    }

    // invalidates all node handles
    constexpr void clear() {
        nodes.clear();
    }

private:
    std::vector<tree_node> nodes;
};

node_handle parse_one(tree_context& tc, std::string_view& s, node_handle parent = {});

node_handle parse_digits(tree_context& tc, std::string_view& s, node_handle parent) {
    node_handle h = tc.create_node(num, parent);
    std::string buf;
    while (isdigit(s[0])) {
        buf += s[0];
        s.remove_prefix(1);
    }
    tc[h].val = std::stoi(buf);
    return h;
}

node_handle parse_pair(tree_context& tc, std::string_view& s, node_handle parent) {
    node_handle h = tc.create_node(pair, parent);
    assert(s[0] == '[');
    s.remove_prefix(1);
    tc[h].left = parse_one(tc, s, h);
    assert(s[0] == ',');
    s.remove_prefix(1);
    tc[h].right = parse_one(tc, s, h);
    assert(s[0] == ']');
    s.remove_prefix(1);
    return h;
}

node_handle parse_one(tree_context& tc, std::string_view& s, node_handle parent) {
    char first = s.at(0);
    if (first == '[') {
        return parse_pair(tc, s, parent);
    } else if (std::isdigit(first)) {
        return parse_digits(tc, s, parent);
    } else {
        throw std::runtime_error("unexpected char");
    }
}

int64_t magnitude(const tree_context& tc, node_handle node) {
    const auto& n = tc[node];
    switch (n.ty) {
    case num:
        return n.val;
    case pair:
        return 3 * magnitude(tc, n.left) + 2 * magnitude(tc, n.right);
    }
    throw std::runtime_error("invalid type");
}

node_handle combine_nodes(tree_context& tc, node_handle left, node_handle right) {
    node_handle new_node = tc.create_node(pair);
    tc[new_node].left = left;
    tc[new_node].right = right;
    tc[left].parent = new_node;
    tc[right].parent = new_node;
    return new_node;
}

enum search_bias {
    bias_left,
    bias_right,
};

node_handle search_number_down(const tree_context& tc, node_handle from, search_bias b) {
    const auto& n = tc[from];
    switch (n.ty) {
    case num:
        return from;
    case pair:
        if (b == bias_left) {
            node_handle left_num = search_number_down(tc, n.left, b);
            if (left_num)
                return left_num;
            node_handle right_num = search_number_down(tc, n.right, b);
            if (right_num)
                return right_num;
        } else {
            node_handle right_num = search_number_down(tc, n.right, b);
            if (right_num)
                return right_num;
            node_handle left_num = search_number_down(tc, n.left, b);
            if (left_num)
                return left_num;
        }
    }
    return {};
}

node_handle search_left_number(const tree_context& tc, node_handle from) {
    auto& n = tc[from];
    if (!n.parent)
        return {};
    auto& p = tc[n.parent];
    assert(p.ty == pair);
    if (p.left != from) {
        node_handle result = search_number_down(tc, p.left, bias_right);
        if (result)
            return result;
        return search_left_number(tc, p.left);
    } else {
        return search_left_number(tc, n.parent);
    }
}

node_handle search_right_number(const tree_context& tc, node_handle from) {
    auto& n = tc[from];
    if (!n.parent)
        return {};
    auto& p = tc[n.parent];
    assert(p.ty == pair);
    if (p.right != from) {
        auto result = search_number_down(tc, p.right, bias_left);
        if (result)
            return result;
        return search_right_number(tc, p.right);
    } else {
        return search_right_number(tc, n.parent);
    }
}

bool try_explode(tree_context& tc, node_handle node, int nest = 0) {
    if (tc[node].ty != pair) {
        return false;
    }
    // If any pair is nested inside four pairs, the leftmost such pair explodes.
    // Exploding pairs will always consist of two regular numbers.
    if (nest == 4) {
        node_handle left_num = search_left_number(tc, node);
        if (left_num) {
            tc[left_num].val += tc[tc[node].left].val;
        }

        node_handle right_num = search_right_number(tc, node);
        if (right_num) {
            tc[right_num].val += tc[tc[node].right].val;
        }

        // Then, the entire exploding pair is replaced with the regular number 0.
        tc.replace_node(node, num);
        tc[node].val = 0;
        return true;
    } else {
        return try_explode(tc, tc[node].left, nest + 1) || try_explode(tc, tc[node].right, nest + 1);
    }
}

bool try_split(tree_context& tc, node_handle node) {
    switch (tc[node].ty) {
    case num:
        if (tc[node].val >= 10) {
            double base_val = tc[node].val / 2.0;
            int new_left = std::floor(base_val);
            int new_right = std::ceil(base_val);

            tc.replace_node(node, pair);
            tc[node].left = tc.create_node(num, node);
            tc[node].right = tc.create_node(num, node);
            tc[tc[node].left].val = new_left;
            tc[tc[node].right].val = new_right;
            return true;
        }
        break;
    case pair:
        return try_split(tc, tc[node].left) || try_split(tc, tc[node].right);
    }
    return false;
}

void reduce_tree(tree_context& tc, node_handle root) {
    while (true) {
        if (!try_explode(tc, root)) {
            if (!try_split(tc, root)) {
                break;
            }
        }
    }
}

node_handle copy_tree(
    const tree_context& source, tree_context& dest, node_handle source_handle, node_handle parent = {}) {
    const auto& source_node = source[source_handle];
    node_handle dest_handle = dest.create_node(source_node.ty, parent);
    switch (source_node.ty) {
    case num:
        dest[dest_handle].val = source_node.val;
        break;
    case pair:
        dest[dest_handle].left = copy_tree(source, dest, source_node.left, dest_handle);
        dest[dest_handle].right = copy_tree(source, dest, source_node.right, dest_handle);
        break;
    }
    return dest_handle;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    tree_context init_trees;
    std::vector<node_handle> init_roots;

    for (auto& line : sr::lines(input)) {
        std::string_view view(line);
        init_roots.push_back(parse_one(init_trees, view));
    }

    // part 1
    tree_context tc = init_trees;
    node_handle left = init_roots[0];
    for (size_t i = 1; i < init_roots.size(); ++i) {
        left = combine_nodes(tc, left, init_roots[i]);
        reduce_tree(tc, left);
    }
    sr::solution(magnitude(tc, left));

    // part 2
    int64_t max_mag = 0;
    for (size_t i = 0; i < init_roots.size(); ++i) {
        for (size_t j = 0; j < init_roots.size(); ++j) {
            if (i == j)
                continue;
            tc.clear();
            node_handle left = copy_tree(init_trees, tc, init_roots[i]);
            node_handle right = copy_tree(init_trees, tc, init_roots[j]);
            node_handle root = combine_nodes(tc, left, right);
            reduce_tree(tc, root);
            max_mag = std::max(max_mag, magnitude(tc, root));
        }
    }
    sr::solution(max_mag);

    return 0;
}
