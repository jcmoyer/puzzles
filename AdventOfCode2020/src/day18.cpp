#include <fstream>
#include <numeric>
#include <variant>

#include <sr/sr.hpp>

inline bool is_num(char ch) {
    return ch >= '0' && ch <= '9';
}

struct token_lparen {};
struct token_rparen {};
struct token_num {
    int64_t val;
};
struct token_binop {
    char kind;
};

using token = std::variant<token_lparen, token_rparen, token_num, token_binop>;

class lexer {
public:
    void lex(std::string_view s, std::vector<token>& tokbuf) {
        pos = 0;
        tokbuf.clear();
        while (pos < s.size()) {
            if (s[pos] == '(') {
                tokbuf.push_back(token_lparen{});
                ++pos;
            } else if (s[pos] == ')') {
                tokbuf.push_back(token_rparen{});
                ++pos;
            } else if (is_num(s[pos])) {
                int64_t n = read_num(s);
                tokbuf.push_back(token_num{n});
            } else if (s[pos] == '+') {
                tokbuf.push_back(token_binop{'+'});
                ++pos;
            } else if (s[pos] == '*') {
                tokbuf.push_back(token_binop{'*'});
                ++pos;
            } else {
                ++pos;
            }
        }
    }

private:
    int64_t read_num(std::string_view s) {
        const char* first = s.data() + pos;
        const char* last = first;
        while (pos < s.size() && is_num(s[pos])) {
            ++pos;
            ++last;
        }
        int64_t val;
        std::from_chars_result result = std::from_chars(first, last, val);
        if (result.ec == std::errc()) {
            return val;
        } else {
            throw std::runtime_error("could not parse integer");
        }
    }

    size_t pos = 0;
};

using astnode_handle = size_t;

struct astnode_mul {
    astnode_handle left;
    astnode_handle right;
};

struct astnode_add {
    astnode_handle left;
    astnode_handle right;
};

struct astnode_num {
    int64_t val;
};

using astnode = std::variant<astnode_mul, astnode_add, astnode_num>;

class ast_context {
public:
    astnode_handle create_node() {
        nodes.emplace_back();
        return nodes.size() - 1;
    }

    astnode_handle create_node(astnode x) {
        nodes.push_back(std::move(x));
        return nodes.size() - 1;
    }

    astnode& get_node(astnode_handle h) {
        return nodes[h];
    }

    const astnode& get_node(astnode_handle h) const {
        return nodes[h];
    }

    void clear() {
        nodes.clear();
    }

private:
    std::vector<astnode> nodes;
};

struct prec_info {
    int prec;
    bool left;
};

using binop_precedences = std::unordered_map<char, prec_info>;

class parser {
public:
    void set_binop_precedences(const binop_precedences& p) {
        prec = &p;
    }

    astnode_handle parse(ast_context& ctx, const std::vector<token>& tokens) {
        toks = &tokens;
        i = 0;
        return parse_expr(ctx, 1);
    }

private:
    astnode_handle parse_atom(ast_context& ctx) {
        if (match<token_lparen>()) {
            next();
            auto n = parse_expr(ctx, 1);
            if (!match<token_rparen>()) {
                throw std::runtime_error("unmatched (");
            }
            next();
            return n;
        } else if (match<token_num>()) {
            auto n = ctx.create_node(astnode_num{std::get<token_num>(current()).val});
            next();
            return n;
        } else {
            throw std::runtime_error("unknown token");
        }
    }

    const prec_info& precedence_info(const token_binop& op) {
        if (prec == nullptr) {
            throw std::runtime_error("no precedence information");
        }
        if (auto it = prec->find(op.kind); it != prec->end()) {
            return it->second;
        } else {
            throw std::runtime_error("no precedence information");
        }
    }

    astnode_handle parse_expr(ast_context& ctx, int min_prec) {
        auto lhs = parse_atom(ctx);

        while (!at_end()) {
            const auto& cur = current();
            bool binop = std::holds_alternative<token_binop>(cur);

            if (!binop) {
                break;
            }

            const prec_info& info = precedence_info(std::get<token_binop>(cur));

            if (info.prec < min_prec) {
                break;
            }

            next();
            auto rhs = parse_expr(ctx, info.left ? 1 + info.prec : info.prec);
            lhs = make_binop(ctx, cur, lhs, rhs);
        }

        return lhs;
    }

    astnode_handle make_binop(ast_context& ctx, const token& t, astnode_handle lhs, astnode_handle rhs) {
        char binop_kind = std::get<token_binop>(t).kind;
        if (binop_kind == '*') {
            return ctx.create_node(astnode_mul{lhs, rhs});
        } else if (binop_kind == '+') {
            return ctx.create_node(astnode_add{lhs, rhs});
        } else {
            throw std::runtime_error("unknown binop type");
        }
    }

    template <typename T>
    bool match() const {
        return std::holds_alternative<T>(current());
    }

    const token& current() const {
        return (*toks)[i];
    }

    void next() {
        ++i;
    }

    bool at_end() const {
        return i == toks->size();
    }

    const std::vector<token>* toks;
    const binop_precedences* prec = nullptr;
    size_t i = 0;
};

int64_t eval(const ast_context& ctx, astnode_handle n) {
    const auto& node = ctx.get_node(n);
    if (auto num = std::get_if<astnode_num>(&node)) {
        return num->val;
    } else if (auto mul = std::get_if<astnode_mul>(&node)) {
        return eval(ctx, mul->left) * eval(ctx, mul->right);
    } else if (auto add = std::get_if<astnode_add>(&node)) {
        return eval(ctx, add->left) + eval(ctx, add->right);
    } else {
        throw std::runtime_error("unexpected variant");
    }
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<std::string> exprs;
    for (const auto& line : sr::lines(input)) {
        exprs.push_back(line);
    }

    int64_t sum1 = 0;
    int64_t sum2 = 0;
    binop_precedences prec1 = {
        {'+', {1, true}},
        {'*', {1, true}},
    };
    binop_precedences prec2 = {
        {'+', {2, true}},
        {'*', {1, true}},
    };
    parser p;
    lexer l;
    ast_context ctx;
    std::vector<token> tokbuf;

    for (auto& e : exprs) {
        l.lex(e, tokbuf);
        p.set_binop_precedences(prec1);
        sum1 += eval(ctx, p.parse(ctx, tokbuf));
        p.set_binop_precedences(prec2);
        sum2 += eval(ctx, p.parse(ctx, tokbuf));
    }

    sr::solution(sum1);
    sr::solution(sum2);

    return 0;
}
