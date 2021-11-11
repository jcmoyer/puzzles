#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/array2d.hpp"
#include "common/point.hpp"

bool alpha(char ch) {
    return ch >= 'A' && ch <= 'Z';
}

struct regex_node {
    enum type { undefined, text, group, branch, root };
    type t;
};

struct regex_root : regex_node {
    std::vector<regex_node*> children;
    regex_root() {
        t = root;
    }
};

struct regex_text : regex_node {
    std::string sequence;
    regex_text() {
        t = text;
    }
};

struct regex_group : regex_node {
    std::vector<regex_node*> branches;
    bool ignorable;
    regex_group() {
        t = group;
        ignorable = false;
    }
};

struct regex_branch : regex_node {
    std::vector<regex_node*> children;
    regex_branch() {
        t = branch;
    }
};

struct regex_context {
    // eventually allocate these contiguously?
    std::vector<std::unique_ptr<regex_node>> nodes;

    template <typename N>
    N* create_node() {
        nodes.emplace_back(std::make_unique<N>());
        return static_cast<N*>(nodes.back().get());
    }
};

struct parse_error {};

class regex_parser {
public:
    regex_parser(std::string_view v) : _tokens{v}, _pos{0} {}

    regex_root* parse(regex_context& ctx);

private:
    regex_node* parse_primary(regex_context& ctx);
    regex_branch* parse_branch(regex_context& ctx);
    regex_group* parse_group(regex_context& ctx);
    regex_text* parse_text(regex_context& ctx);

    int current() const;
    int peek() const;
    bool next();

    std::string_view _tokens;
    std::size_t _pos;

    static constexpr int EOS = std::char_traits<char>::eof();
};

bool is_group(const regex_node& n) {
    return n.t == regex_node::group;
}
bool is_text(const regex_node& n) {
    return n.t == regex_node::text;
}
bool is_branch(const regex_node& n) {
    return n.t == regex_node::branch;
}
bool is_root(const regex_node& n) {
    return n.t == regex_node::root;
}

// this just takes the longest branch where possible
// TODO: test for other inputs, this currently ignores ignorable groups like
// (A|B|)
int64_t get_longest_path(const regex_node& root) {
    int64_t sum = 0;

    if (is_root(root)) {
        const regex_root& r = (const regex_root&)root;
        for (const auto* child : r.children) {
            sum += get_longest_path(*child);
        }
    } else if (is_group(root)) {
        const regex_group& g = (const regex_group&)root;
        if (g.ignorable) {
            return 0;
        }
        std::vector<int64_t> branch_sizes;
        for (const auto* branch : g.branches) {
            branch_sizes.push_back(get_longest_path(*branch));
        }
        sum += *std::max_element(branch_sizes.begin(), branch_sizes.end());
    } else if (is_branch(root)) {
        const regex_branch& b = (const regex_branch&)root;
        for (const auto* child : b.children) {
            sum += get_longest_path(*child);
        }
    } else if (is_text(root)) {
        const regex_text& t = (const regex_text&)root;
        sum += t.sequence.size();
    }

    return sum;
}

point walk_path(std::string_view v) {
    point r{0, 0};
    for (char c : v) {
        switch (c) {
        case 'W':
            r.x--;
            break;
        case 'E':
            r.x++;
            break;
        case 'N':
            r.y--;
            break;
        case 'S':
            r.y++;
            break;
        default:
            throw std::runtime_error("bad input");
        }
    }
    return r;
}

using path = std::string;
using path_vector = std::vector<path>;

template <typename F>
void visit_all_paths(const regex_node& root, const F& path_visitor, path_vector paths_so_far = path_vector(),
    path_vector dead_ends = path_vector()) {
    if (is_root(root)) {
        const regex_root& r = (const regex_root&)root;
        paths_so_far.emplace_back();
        for (const auto* child : r.children) {
            if (child->t == regex_node::text) {
                const regex_text& t = (const regex_text&)*child;
                for (std::size_t i = 0; i < t.sequence.size(); ++i) {
                    for (auto& s : paths_so_far) {
                        s.push_back(t.sequence[i]);
                        path_visitor(s);
                    }
                }
            } else if (child->t == regex_node::group) {
                visit_all_paths(*child, path_visitor, paths_so_far, dead_ends);
            }
        }
    } else if (is_group(root)) {
        const regex_group& g = (const regex_group&)root;
        std::vector<std::vector<std::string>> branch_paths;
        for (const auto* branch : g.branches) {
            // apply each branch to all current paths
            std::vector<std::string> subpaths = paths_so_far;
            visit_all_paths(*branch, path_visitor, subpaths, dead_ends);
            branch_paths.push_back(std::move(subpaths));
        }
        if (!g.ignorable) {
            paths_so_far.clear();
            for (auto& p : branch_paths)
                for (auto& u : p)
                    paths_so_far.push_back(std::move(u));
        } else {
            for (auto& p : branch_paths)
                for (auto& u : p)
                    dead_ends.push_back(std::move(u));
        }
    } else if (is_branch(root)) {
        const regex_branch& b = (const regex_branch&)root;
        for (const auto* child : b.children) {
            if (child->t == regex_node::text) {
                const regex_text& t = (const regex_text&)*child;
                for (std::size_t i = 0; i < t.sequence.size(); ++i) {
                    for (auto& s : paths_so_far) {
                        s.push_back(t.sequence[i]);
                        path_visitor(s);
                    }
                }
            } else if (child->t == regex_node::group) {
                visit_all_paths(*child, path_visitor, paths_so_far, dead_ends);
            }
        }
    }
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;
    std::getline(input, line);
    regex_parser parser(line);
    regex_context ctx;
    auto root = parser.parse(ctx);

    // part 1
    std::cout << get_longest_path(*root) << std::endl;

    // part 2
    std::unordered_map<point, std::size_t> distance_map;
    visit_all_paths(*root, [&](const std::string& path) {
        // discard paths that are too short
        if (path.size() < 1000)
            return;

        point location = walk_path(path);

        if (auto it = distance_map.find(location); it != distance_map.end()) {
            // we already saw this room; if one of the paths is shorter, take that one
            if (path.size() < it->second) {
                distance_map[location] = path.size();
            }
        } else {
            distance_map[location] = path.size();
        }
    });

    std::cout << distance_map.size() << std::endl;

    return 0;
}

regex_root* regex_parser::parse(regex_context& ctx) {
    regex_root* root = ctx.create_node<regex_root>();
    regex_node* child;

    if (current() != '^')
        throw parse_error{};
    next();

    while (child = parse_primary(ctx)) {
        root->children.push_back(child);

        if (current() == '$') {
            next();
            break;
        }
    }

    return root;
}

regex_node* regex_parser::parse_primary(regex_context& ctx) {
    if (current() == '(') {
        return parse_group(ctx);
    } else {
        return parse_text(ctx);
    }
}

regex_branch* regex_parser::parse_branch(regex_context& ctx) {
    regex_branch* b = ctx.create_node<regex_branch>();

    while (current() != '|' && current() != ')') {
        if (current() == '(') {
            b->children.push_back(parse_group(ctx));
        } else {
            b->children.push_back(parse_text(ctx));
        }
    }

    return b;
}

regex_group* regex_parser::parse_group(regex_context& ctx) {
    if (current() != '(')
        throw parse_error{};
    next();

    regex_group* g = ctx.create_node<regex_group>();

    for (;;) {
        if (current() == ')') {
            next();
            break;
        } else if (current() == '|' && peek() == ')') {
            g->ignorable = true;
            next();
            next();
            break;
        } else if (current() == '|') {
            next();
        } else {
            g->branches.push_back(parse_branch(ctx));
        }
    }

    return g;
}

regex_text* regex_parser::parse_text(regex_context& ctx) {
    regex_text* t = ctx.create_node<regex_text>();
    while (alpha(current())) {
        t->sequence.push_back(current());
        next();
    }
    return t;
}

int regex_parser::current() const {
    if (_pos < _tokens.size())
        return _tokens[_pos];
    else
        return EOS;
}

int regex_parser::peek() const {
    if (_pos < _tokens.size())
        return _tokens[_pos + 1];
    else
        return EOS;
}

bool regex_parser::next() {
    if (_pos + 1 < _tokens.size()) {
        ++_pos;
        return true;
    } else {
        return false;
    }
}
