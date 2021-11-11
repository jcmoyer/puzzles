#include <fmt/format.h>
#include <fstream>

#include <sr/sr.hpp>

constexpr bool in_range(int i, int min, int max) {
    return i >= min && i <= max;
}

constexpr auto range_validator(int min, int max) {
    return [=](int x) {
        return in_range(x, min, max);
    };
}

bool is_valid(const char* re, std::string_view str) {
    try {
        sr::parse(re, str);
    } catch (const sr::bad_match&) {
        return false;
    }
    return true;
}

template <typename T, typename Validator>
bool is_valid(const char* re, std::string_view str, Validator v) {
    T val;
    try {
        sr::parse(re, str, val);
    } catch (const sr::bad_match&) {
        return false;
    }
    return v(val);
}

template <typename T, typename U, typename Validator>
bool is_valid(const char* re, std::string_view str, Validator v) {
    T val_t;
    U val_u;
    try {
        sr::parse(re, str, val_t, val_u);
    } catch (const sr::bad_match&) {
        return false;
    }
    return v(val_t, val_u);
}

bool is_valid_byr(std::string_view str) {
    return is_valid<int>(R"((\d{4}))", str, range_validator(1920, 2002));
}

bool is_valid_iyr(std::string_view str) {
    return is_valid<int>(R"((\d{4}))", str, range_validator(2010, 2020));
}

bool is_valid_eyr(std::string_view str) {
    return is_valid<int>(R"((\d{4}))", str, range_validator(2020, 2030));
}

bool is_valid_hgt(std::string_view str) {
    return is_valid<int, std::string_view>(R"((\d+)(\w\w))", str, [](int hgt, std::string_view unit) {
        return (unit == "cm" && in_range(hgt, 150, 193)) || (unit == "in" && in_range(hgt, 59, 76));
    });
}

bool is_valid_hcl(std::string_view str) {
    return is_valid(R"(#[0-9a-f]{6})", str);
}

bool is_valid_ecl(std::string_view str) {
    return is_valid(R"(amb|blu|brn|gry|grn|hzl|oth)", str);
}

bool is_valid_pid(std::string_view str) {
    return is_valid(R"(\d{9})", str);
}

class passport {
public:
    passport() = default;

    bool is_valid() const {
        return attrib.count("byr") && attrib.count("iyr") && attrib.count("eyr") && attrib.count("hgt") &&
               attrib.count("hcl") && attrib.count("ecl") && attrib.count("pid");
    }

    bool is_valid2() const {
        using namespace std::string_view_literals;

        struct validation {
            using field_validator = bool (*)(std::string_view);

            std::string_view field_name;
            field_validator validator;
        };

        constexpr validation validations[] = {
            {"byr"sv, is_valid_byr},
            {"iyr"sv, is_valid_iyr},
            {"eyr"sv, is_valid_eyr},
            {"hgt"sv, is_valid_hgt},
            {"hcl"sv, is_valid_hcl},
            {"ecl"sv, is_valid_ecl},
            {"pid"sv, is_valid_pid},
        };

        return std::all_of(std::cbegin(validations), std::cend(validations), [&](const validation& kvp) {
            auto it = attrib.find(kvp.field_name);
            return it != attrib.end() && kvp.validator(it->second);
        });
    }

    void add_attrib(std::string name, std::string val) {
        attrib.insert_or_assign(std::move(name), std::move(val));
    }

private:
    sr::unordered_string_map<std::string> attrib;
};

class passport_list {
public:
    void add_passport(passport p) {
        pps.emplace_back(std::move(p));
    }

    int count_valid() const {
        return (int)std::ranges::count_if(pps, [](const auto& pp) {
            return pp.is_valid();
        });
    }

    int count_valid2() const {
        return (int)std::ranges::count_if(pps, [](const auto& pp) {
            return pp.is_valid2();
        });
    }

private:
    std::vector<passport> pps;
};

class passport_parser {
public:
    passport_parser() = default;

    void parse_fragment(std::string_view frag) {
        size_t i = 0;
        while (i < frag.size()) {
            char ch = frag[i];
            switch (_state) {
            case S_NAME:
                handle_name(ch);
                break;
            case S_VAL:
                handle_val(ch);
                break;
            case S_LINE:
                handle_line(ch);
                break;
            }
            ++i;
        }
    }

    // must be called when done passing fragments
    void finish() {
        commit();
    }

    const passport_list& passports() const {
        return _pplist;
    }

private:
    void handle_name(char ch) {
        if (ch == ':') {
            _state = S_VAL;
        } else {
            _curname += ch;
        }
    }

    void handle_val(char ch) {
        if (ch == ' ' || ch == '\n') {
            _state = S_NAME;
            _current.add_attrib(_curname, _curval);
            _curname.clear();
            _curval.clear();
            if (ch == '\n') {
                _state = S_LINE;
            }
        } else {
            _curval += ch;
        }
    }

    void handle_line(char ch) {
        if (ch == '\n') {
            commit();
        } else {
            _state = S_NAME;
            _curname += ch;
        }
    }

    void commit() {
        _pplist.add_passport(std::move(_current));
        _current = passport{};
    }

    enum parse_state { S_NAME, S_VAL, S_LINE };

    parse_state _state = S_NAME;
    passport_list _pplist;
    passport _current;
    std::string _curname;
    std::string _curval;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    passport_parser parser;

    char buf[4096];
    while (input) {
        input.read(buf, sizeof(buf));
        parser.parse_fragment(std::string_view(buf, input.gcount()));
    }
    parser.finish();

    sr::solution(parser.passports().count_valid());
    sr::solution(parser.passports().count_valid2());

    return 0;
}
