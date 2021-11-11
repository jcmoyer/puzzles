// this solution is incomplete, never implemented backtracking properly and ended up using a python library

#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <variant>

#include <sr/sr.hpp>

inline bool is_num(char ch) {
    return ch >= '0' && ch <= '9';
}

struct token_or {};
struct token_num {
    int64_t val;
};
struct token_strlit {
    std::string val;
};
struct token_colon {};

using token = std::variant<token_or, token_num, token_strlit, token_colon>;

class lexer {
public:
    void lex(std::string_view s, std::vector<token>& tokbuf) {
        pos = 0;
        tokbuf.clear();
        while (pos < s.size()) {
            if (s[pos] == '|') {
                tokbuf.push_back(token_or{});
                ++pos;
            } else if (s[pos] == '"') {
                tokbuf.push_back(token_strlit{read_strlit(s)});
            } else if (is_num(s[pos])) {
                int64_t n = read_num(s);
                tokbuf.push_back(token_num{n});
            } else if (s[pos] == ':') {
                tokbuf.push_back(token_colon{});
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

    std::string read_strlit(std::string_view s) {
        ++pos;
        const char* first = s.data() + pos;
        const char* last = first;
        while (pos < s.size() && s[pos] != '"') {
            ++pos;
            ++last;
        }
        ++pos;
        return std::string(first, last);
    }

    size_t pos = 0;
};

using astnode_handle = size_t;

struct astnode_patterndef {
    int64_t id;
    astnode_handle child;
};

struct astnode_or {
    astnode_handle left;
    astnode_handle right;
};

struct astnode_seq {
    std::vector<int64_t> val;
};

struct astnode_str {
    std::string val;
};

using astnode = std::variant<astnode_or, astnode_seq, astnode_patterndef, astnode_str>;

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
        return parse_patterndef(ctx);
    }

private:
    astnode_handle parse_patterndef(ast_context& ctx) {
        if (match<token_num>()) {
            auto def = astnode_patterndef{};
            def.id = std::get<token_num>(current()).val;
            next();
            if (!match<token_colon>())
                throw std::runtime_error("expected colon");
            next();
            def.child = parse_expr(ctx, 1);
            return ctx.create_node(def);
        } else {
            throw std::runtime_error("expected number");
        }
    }

    astnode_handle parse_atom(ast_context& ctx) {
        if (match<token_strlit>()) {
            astnode_str str;
            str.val = std::get<token_strlit>(current()).val;
            return ctx.create_node(str);
        }

        if (!match<token_num>()) {
            throw std::runtime_error("expected string or number");
        }

        astnode_seq seq;

        while (!at_end() && match<token_num>()) {
            seq.val.push_back(std::get<token_num>(current()).val);
            next();
        }
        return ctx.create_node(seq);
    }

    astnode_handle parse_expr(ast_context& ctx, int min_prec) {
        auto lhs = parse_atom(ctx);

        while (!at_end()) {
            const auto& cur = current();
            bool binop = std::holds_alternative<token_or>(cur);

            if (!binop) {
                break;
            }

            // const prec_info& info = precedence_info(std::get<token_binop>(cur));
            const prec_info& info = {1, true};

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
        return ctx.create_node(astnode_or{lhs, rhs});
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

struct patterndef {
    int64_t id;
    std::vector<int64_t> children;
};

using pattern_registry = std::unordered_map<int64_t, astnode_handle>;
//
// std::string pattern_match(const ast_context& ctx, astnode_handle which, const pattern_registry& reg) {
//    const auto& node = ctx.get_node(which);
//    if (auto def = std::get_if<astnode_patterndef>(&node)) {
//        if (def->id == 8) {
//            auto _42 = pattern_match(ctx, reg.at(42), reg);
//            std::string buf = "(";
//            buf += _42;
//            buf += ")+";
//            return buf;
//        } else if (def->id == 11) {
//            auto _42 = pattern_match(ctx, reg.at(42), reg);
//            auto _31 = pattern_match(ctx, reg.at(31), reg);
//            return fmt::format("(?P<balance>{}(?P&balance){}|{}{})", _42, _31, _42, _31);
//        } else {
//            return pattern_match(ctx, def->child, reg);
//        }
//    } else if (auto def = std::get_if<astnode_or>(&node)) {
//        std::string result;
//        result += "(";
//        result += pattern_match(ctx, def->left, reg);
//        result += "|";
//        result += pattern_match(ctx, def->right, reg);
//        result += ")";
//        return result;
//    } else if (auto def = std::get_if<astnode_seq>(&node)) {
//        std::string result;
//        for (auto& s : def->val) {
//            result += pattern_match(ctx, reg.at(s), reg);
//        }
//        return result;
//    } else if (auto def = std::get_if<astnode_str>(&node)) {
//        return def->val;
//    }
//}

enum match_status {
    m_success,
    m_error,
    m_recursive,
};

struct match_result {
    match_status success;
    size_t new_i;

    operator bool() const {
        return success == m_success;
    }
};

struct matcher {
    matcher(const ast_context& ctx, const pattern_registry& reg) : ctx{ctx}, reg{reg} {}

    bool match(std::string_view s) {
        auto result = match_recursive(s, {0}, reg.at(0));
        // only true if found full match
        return std::any_of(result.begin(), result.end(), [&](auto&& r) {
            return r.new_i == s.size();
        });
    }

    /* match_result match_recursive(std::string_view s, size_t i, size_t n, size_t depth) {
         const auto& node = ctx.get_node(n);
         if (auto def = std::get_if<astnode_patterndef>(&node)) {
             return match_recursive(s, i, def->child, depth + 1);
         } else if (auto def = std::get_if<astnode_or>(&node)) {
             auto result = match_recursive(s, i, def->left, depth + 1);
             if (result.success == m_success) {
                 return result;
             } else {
                 result = match_recursive(s, i, def->right, depth + 1);
                 if (result.success == m_success) {
                     return result;
                 } else {
                     return {m_error, i};
                 }
             }
         } else if (auto def = std::get_if<astnode_seq>(&node)) {
             size_t cursor = i;
             for (const auto child : def->val) {
                 auto result = match_recursive(s, cursor, reg.at(child), depth + 1);
                 if (result.success == m_success) {
                     cursor = result.new_i;
                 } else {
                     return {result.success, i};
                 }
             }
             return {m_success, cursor};
         } else if (auto def = std::get_if<astnode_str>(&node)) {
             if (i >= s.size()) {
                 return {m_recursive, i};
             } else {
                 return {s[i] == def->val[0] ? m_success : m_error, i + 1};
             }
         }
         throw std::runtime_error("what");
     }*/

    // I think the problem is something like:
    //
    // 0: 1 | 3
    // 1: 2 | 1 2
    // 2: "a"
    // 3: "b"
    // aa
    //
    // I think this matches as 0 0 instead of just 0
    // The problem doesn't seem to state this but it's implied by (<1>)+ working in regex?

    // s - string we're matching against
    // i - index into that string to match against
    // n - ast node we're currently examining in the tree
    //
    // the grammar is something like:
    //
    // patterndef -> INTEGER ':' (or | seq)
    // or         -> seq | seq
    // seq        -> INTEGER [INTEGER+]
    //
    // nodes usually look something like:
    //
    // patterndef -> or
    //              /  \
    //             or  seq
    //            /  \   \
    //          seq  seq str
    //
    // this function does the following:
    //
    // 1. we start at the ast node for patterndef 0 (caller does this)
    // 2. we look at its child (or-node in the input)
    // 3. we start matching the branches of the or-node against 's'
    // 4. if the left branch matches (recursively), we return success
    // 5. otherwise, we check the right branch
    // 6. at a seq-node, we check each child sequentially, if any child fails to match we do not advance 'i'
    // 7. at a str-nodes, we terminate the recursion and increment 'i' (caller's responsibility to adjust)
    //
    // I think the problem is something like:
    //
    // 0: 1 | 1 0
    // 1: "a"
    // aa
    //
    // we match 1, but then fail because we haven't consumed the entire string
    std::vector<match_result> match_recursive(std::string_view s, size_t i, astnode_handle n, int depth = 0) {
        if (depth > 24)
            return {};

        const auto& node = ctx.get_node(n);
        if (auto def = std::get_if<astnode_patterndef>(&node)) {
            // RN: RX RY | RZ
            return match_recursive(s, i, def->child, depth + 1);
        } else if (auto or_ = std::get_if<astnode_or>(&node)) {
            // X Y | Z W
            auto result_left = match_recursive(s, i, or_->left, depth + 1);
            auto result_right = match_recursive(s, i, or_->right, depth + 1);

            std::vector<match_result> results;
            for (auto& r : result_left)
                if (r)
                    results.push_back(r);
            for (auto& r : result_right)
                if (r)
                    results.push_back(r);

            return results;
        } else if (auto seq = std::get_if<astnode_seq>(&node)) {
            // X Y Z
            std::vector<size_t> cursor = {i};
            std::vector<match_result> results;
            for (const auto child : seq->val) {
                results.clear();
                for (size_t c : cursor) {
                    auto result_for_cursor = match_recursive(s, c, reg.at(child), depth + 1);
                    for (auto& r : result_for_cursor)
                        if (r)
                            results.push_back(r);
                }
                if (results.size()) {
                    cursor.clear();
                    for (auto& r : results) {
                        cursor.push_back(r.new_i);
                    }
                }
            }
            return results;
        } else if (auto str = std::get_if<astnode_str>(&node)) {
            // "a"
            if (i >= s.size()) {
                return {};
            } else {
                return {{s[i] == str->val[0] ? m_success : m_error, {i + 1}}};
            }
        }
    }

    // match_result match_recursive(std::string_view s, size_t i, astnode_handle n) {
    //    const auto& node = ctx.get_node(n);
    //    if (auto def = std::get_if<astnode_patterndef>(&node)) {
    //        // RN: RX RY | RZ
    //        return match_recursive(s, i, def->child);
    //    } else if (auto or_ = std::get_if<astnode_or>(&node)) {
    //        // X Y | Z W
    //        auto result_left = match_recursive(s, i, or_->left);
    //        auto result_right = match_recursive(s, i, or_->right);
    //
    //        if (result_left)
    //            return result_left;

    //        if (result_right)
    //            return result_right;

    //        return {m_error, i};
    //    } else if (auto seq = std::get_if<astnode_seq>(&node)) {
    //        // X Y Z
    //        size_t cursor = i;
    //        for (const auto child : seq->val) {
    //            auto result = match_recursive(s, cursor, reg.at(child));
    //            if (result) {
    //                cursor = result.new_i;
    //            } else {
    //                return {m_error, i};
    //            }
    //        }
    //        return {m_success, cursor};
    //    } else if (auto str = std::get_if<astnode_str>(&node)) {
    //        // "a"
    //        if (i >= s.size()) {
    //            return {m_error, i};
    //        } else {
    //            return {s[i] == str->val[0] ? m_success : m_error, i + 1};
    //        }
    //    }
    //}

    const ast_context& ctx;
    const pattern_registry& reg;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<std::string> defs;
    std::vector<std::string> msgs;

    for (const auto& line : sr::lines(input)) {
        if (line.starts_with("8:")) {
            defs.push_back("8: 42 | 42 8");
            // defs.push_back(
            //    "8: 42 | 42 42 | 42 42 42 | 42 42 42 42 | 42 42 42 42 42 | 42 42 42 42 42 42 | 42 42 42 42 42 42 42 |
            //    " "42 42 42 42 42 42 42 42 | 42 42 42 42 42 42 42 42 42 | 42 42 42 42 42 42 42 42 42 42");
            // defs.push_back("8: 42 42 42 42 42 42 42 42 | 42 42 42 42 42 42 42 | 42 42 42 42 42 42 | 42 42 42 42 42
            // |"
            //               "42 42 42 42 | 42 42 42 | 42 42 | 42");
        } else if (line.starts_with("11:")) {
            defs.push_back("11: 42 31 | 42 11 31");
            // defs.push_back(
            //    "11: 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31 | "
            //    "42 42 42 42 42 31 31 31 31 31 | 42 42 42 42 42 42 31 31 31 31 31 31 | "
            //    "42 42 42 42 42 42 42 31 31 31 31 31 31 31 | 42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 | "
            //    "42 42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 31 | "
            //    "42 42 42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 31 31");

            // defs.push_back(
            //    "11: 42 42 42 42 42 42 42 42 31 31 31 31 31 31 31 31 | 42 42 42 42 42 42 42 31 31 31 31 31 31 31 |"
            //"42 42 42 42 42 42 31 31 31 31 31 31 | 42 42 42 42 42 31 31 31 31 31 | 42 42 42 42 31 31 31 31 |"
            //"42 42 42 31 31 31 | 42 42 31 31 | 42 31");
        }

        //
        // 8: 42 | 42 8
        // 11 : 42 31 | 42 11 31

        else if (line.find(':') != line.npos)
            defs.push_back(line);
        else if (line.size())
            msgs.push_back(line);
    }

    parser p;
    lexer l;
    ast_context ctx;
    std::vector<token> tokbuf;

    std::unordered_map<int64_t, astnode_handle> pats;

    std::vector<astnode_handle> def_handles;
    for (auto& e : defs) {
        l.lex(e, tokbuf);
        auto r = p.parse(ctx, tokbuf);
        def_handles.push_back(r);
    }

    std::for_each(def_handles.begin(), def_handles.end(), [&](auto&& n) {
        pats[std::get<astnode_patterndef>(ctx.get_node(n)).id] = n;
    });

    int64_t sum = 0;

    int64_t times = 0;

    matcher mtch(ctx, pats);

    /*std::string px = pattern_match(ctx, pats[0], pats);
    px.insert(px.begin(), '^');
    px.insert(px.end(), '$');*/
    // std::regex re(px);

    for (const auto& m : msgs) {
        bool matches = mtch.match(m);
        sum += matches;

        ++times;
        fmt::print("{}/{}: match={} {} {}\n", times, msgs.size(), matches, m, sum);
    }
    sr::solution(sum);

    return 0;
}
