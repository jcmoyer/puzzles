#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

enum class packet_type : uint8_t {
    sum = 0,
    product = 1,
    min = 2,
    max = 3,
    literal = 4,
    greater = 5,
    less = 6,
    equal = 7,
};

struct packet {
    std::vector<packet> children;
    uint64_t bit_length;
    uint64_t val;
    uint8_t version;
    packet_type type_id;
};

packet parse_root(sr::bitset_reader& reader);

void parse_operator(sr::bitset_reader& reader, packet& p) {
    uint8_t len_type_id = reader.read<uint8_t>(1);
    if (len_type_id == 0) {
        // If the length type ID is 0, then the next 15 bits are a number that represents the total length in
        // bits of the sub-packets contained by this packet.
        uint64_t sub_len = reader.read<uint64_t>(15);
        p.val = sub_len;
        size_t remaining = sub_len;
        while (remaining > 0) {
            auto& node = p.children.emplace_back(parse_root(reader));
            remaining -= node.bit_length;
        }
    } else {
        // If the length type ID is 1, then the next 11 bits are a number that represents the number of
        // sub-packets immediately contained by this packet.
        uint64_t sub_packet_count = reader.read<uint64_t>(11);
        p.val = sub_packet_count;
        size_t remaining = sub_packet_count;
        while (remaining > 0) {
            p.children.emplace_back(parse_root(reader));
            --remaining;
        }
    }
}

void parse_literal(sr::bitset_reader& reader, packet& p) {
    uint8_t group_header = 0;
    uint64_t val = 0;
    do {
        group_header = reader.read<uint8_t>(1);
        val <<= 4;
        val |= reader.read<uint64_t>(4);
    } while (group_header);
    p.val = val;
}

packet parse_root(sr::bitset_reader& reader) {
    // Every packet begins with a standard header: the first three bits encode the packet version, and the next
    // three bits encode the packet type ID.
    size_t packet_start = reader.position();
    packet p;
    p.version = reader.read<uint8_t>(3);
    p.type_id = (packet_type)reader.read<uint8_t>(3);

    if (p.type_id == packet_type::literal) {
        parse_literal(reader, p);
    } else {
        parse_operator(reader, p);
    }

    p.bit_length = reader.position() - packet_start;

    return p;
}

uint64_t sum_versions(const packet& p) {
    uint64_t v = p.version;
    for (auto& child : p.children) {
        v += sum_versions(child);
    }
    return v;
}

uint64_t eval(const packet& p) {
    switch (p.type_id) {
    case packet_type::sum: {
        uint64_t sum = 0;
        for (const auto& c : p.children) {
            sum += eval(c);
        }
        return sum;
    }

    case packet_type::product: {
        uint64_t product = 1;
        for (const auto& c : p.children) {
            product *= eval(c);
        }
        return product;
    }

    case packet_type::min: {
        uint64_t min = -1;
        for (const auto& c : p.children) {
            min = std::min(min, eval(c));
        }
        return min;
    }

    case packet_type::max: {
        uint64_t max = 0;
        for (const auto& c : p.children) {
            max = std::max(max, eval(c));
        }
        return max;
    }

    case packet_type::literal:
        return p.val;

    case packet_type::greater:
        return eval(p.children[0]) > eval(p.children[1]);

    case packet_type::less:
        return eval(p.children[0]) < eval(p.children[1]);

    case packet_type::equal:
        return eval(p.children[0]) == eval(p.children[1]);
    }

    throw std::runtime_error("unexpected type id");
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::string line;
    std::getline(input, line);

    sr::dynamic_bitset bs = sr::bitset_from_bytes(sr::hex_to_bytes(line));
    sr::bitset_reader br(bs);
    packet p = parse_root(br);

    sr::solution(sum_versions(p));
    sr::solution(eval(p));

    return 0;
}
