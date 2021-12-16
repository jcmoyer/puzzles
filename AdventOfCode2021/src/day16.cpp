#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

std::string hex2bin(std::string_view in) {
    std::string out;
    std::unordered_map<char, const char*> substitutions{
        // clang-format off
        {'0', "0000"},
        {'1', "0001"},
        {'2', "0010"},
        {'3', "0011"},
        {'4', "0100"},
        {'5', "0101"},
        {'6', "0110"},
        {'7', "0111"},
        {'8', "1000"},
        {'9', "1001"},
        {'A', "1010"},
        {'B', "1011"},
        {'C', "1100"},
        {'D', "1101"},
        {'E', "1110"},
        {'F', "1111"},
        // clang-format on
    };
    for (char ch : in) {
        if (substitutions.contains(std::toupper(ch))) {
            out += substitutions[std::toupper(ch)];
        } else {
            throw std::runtime_error("bad char");
        }
    }
    return out;
}

uint64_t bin2int(std::string_view in) {
    uint64_t out = 0;

    uint8_t shift = in.size() - 1;

    for (size_t i = 0; i < in.size(); ++i, --shift) {
        if (in[i] == '1') {
            out |= (1ull << shift);
        }
    }

    return out;
}

struct literal_payload {
    uint64_t val;
};

struct operator_total_bit_length {
    uint64_t bits;
};

struct operator_total_child_length {
    uint64_t child_count;
};

enum packet_type {
    p_literal,
    p_operator_bit_len,
    p_operator_child_len,
};

struct packet {
    uint64_t version;
    uint64_t type_id;
    uint64_t bit_length;
    packet_type payload_type;
    union {
        literal_payload lit;
        operator_total_bit_length oper_bit_len;
        operator_total_child_length oper_child_len;
    };
    std::vector<packet> children;
};

struct packet_parser {
    std::string_view binstr;
    size_t offset = 0;

    std::string_view rest() const {
        return binstr.substr(offset);
    }

    uint64_t read_bin(size_t len) {
        uint64_t val = bin2int(rest().substr(0, len));
        offset += len;
        return val;
    }

    packet parse_one() {
        // Every packet begins with a standard header: the first three bits encode the packet version, and the next
        // three bits encode the packet type ID.
        size_t packet_start = offset;
        packet p;
        p.version = read_bin(3);
        p.type_id = read_bin(3);

        if (p.type_id == 4) {
            // literal packet
            uint64_t group_header = 0;
            uint64_t val = 0;
            do {
                group_header = read_bin(1);
                val <<= 4;
                val |= read_bin(4);
            } while (group_header == 1);
            p.payload_type = p_literal;
            p.lit.val = val;
        } else {
            // operator packet
            uint64_t len_type_id = read_bin(1);
            if (len_type_id == 0) {
                // If the length type ID is 0, then the next 15 bits are a number that represents the total length in
                // bits of the sub-packets contained by this packet.
                uint64_t sub_len = read_bin(15);
                p.payload_type = p_operator_bit_len;
                p.oper_bit_len.bits = sub_len;
                size_t remaining = sub_len;
                while (remaining > 0) {
                    p.children.push_back(parse_one());
                    remaining -= p.children.back().bit_length;
                }
            } else {
                // If the length type ID is 1, then the next 11 bits are a number that represents the number of
                // sub-packets immediately contained by this packet.
                uint64_t sub_packet_count = read_bin(11);
                p.payload_type = p_operator_child_len;
                p.oper_child_len.child_count = sub_packet_count;
                size_t remaining = sub_packet_count;
                while (remaining > 0) {
                    p.children.push_back(parse_one());
                    --remaining;
                }
            }
        }

        p.bit_length = offset - packet_start;

        return p;
    }
};

uint64_t sum_versions(const packet& p) {
    uint64_t v = p.version;
    for (auto& child : p.children) {
        v += sum_versions(child);
    }
    return v;
}

uint64_t eval(const packet& p) {
    switch (p.type_id) {
    case 0: {
        uint64_t sum = 0;
        for (const auto& c : p.children) {
            sum += eval(c);
        }
        return sum;
    }

    case 1: {
        uint64_t product = 1;
        for (const auto& c : p.children) {
            product *= eval(c);
        }
        return product;
    }

    case 2: {
        uint64_t min = -1;
        for (const auto& c : p.children) {
            min = std::min(min, eval(c));
        }
        return min;
    }

    case 3: {
        uint64_t max = 0;
        for (const auto& c : p.children) {
            max = std::max(max, eval(c));
        }
        return max;
    }

    case 4:
        return p.lit.val;

    case 5:
        return eval(p.children[0]) > eval(p.children[1]);

    case 6:
        return eval(p.children[0]) < eval(p.children[1]);

    case 7:
        return eval(p.children[0]) == eval(p.children[1]);
    }

    throw std::runtime_error("unexpected type id");
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string line;
    std::getline(input, line);

    std::string binstring = hex2bin(line);

    packet_parser pp;
    pp.binstr = binstring;

    packet p = pp.parse_one();

    sr::solution(sum_versions(p));
    sr::solution(eval(p));

    return 0;
}
