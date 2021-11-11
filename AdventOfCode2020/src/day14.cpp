#include <array>
#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <variant>

#include <sr/sr.hpp>

struct cmd_set_mask {
    std::string mask;
};

struct cmd_write_mem {
    uint64_t base_addr;
    uint64_t val;
};

using cmd = std::variant<cmd_set_mask, cmd_write_mem>;
using cmd_list = std::vector<cmd>;
using memory = std::unordered_map<uint64_t, uint64_t>;

class part1_solver {
public:
    void handle(memory& mem, const cmd_set_mask& c) {
        ones_mask = 0;
        zero_mask = 0;
        uint64_t bit = 1;
        std::for_each(c.mask.rbegin(), c.mask.rend(), [&](char ch) {
            if (ch == '0') {
                zero_mask |= bit;
            } else if (ch == '1') {
                ones_mask |= bit;
            }
            bit <<= 1;
        });
        zero_mask = ~zero_mask;
    }

    void handle(memory& mem, const cmd_write_mem& c) {
        mem[c.base_addr] = (c.val & zero_mask) | ones_mask;
    }

private:
    uint64_t ones_mask;
    uint64_t zero_mask;
};

class part2_solver {
public:
    void handle(memory& mem, const cmd_set_mask& c) {
        current_wops.clear();
        ones_mask = 0;
        zero_mask = 0;
        uint64_t bit = 1;
        uint8_t shift = 0;
        std::for_each(c.mask.rbegin(), c.mask.rend(), [&](char ch) {
            if (ch == 'X') {
                current_wops.emplace_back(addr_write_op{shift, 1});
                zero_mask |= bit;
            } else if (ch == '1') {
                ones_mask |= bit;
            }
            bit <<= 1;
            ++shift;
        });
        zero_mask = ~zero_mask;
    }

    void handle(memory& mem, const cmd_write_mem& c) {
        auto combs = sr::index_combinations(current_wops.size());
        while (combs.next()) {
            uint64_t addr = (c.base_addr & zero_mask) | ones_mask;
            for (size_t i : combs) {
                const auto& wop = current_wops[i];
                addr &= ~(1ull << wop.bit);
                addr |= (static_cast<uint64_t>(wop.val) << wop.bit);
            }
            mem[addr] = c.val;
        }
    }

private:
    struct addr_write_op {
        uint8_t bit;
        uint8_t val;
    };

    std::vector<addr_write_op> current_wops;
    uint64_t ones_mask;
    uint64_t zero_mask;
};

template <typename Solver>
uint64_t process(const cmd_list& cmds) {
    memory mem;
    Solver solver;
    for (const cmd& c : cmds) {
        std::visit(
            [&](auto&& c) {
                solver.handle(mem, c);
            },
            c);
    }

    return std::accumulate(mem.begin(), mem.end(), 0LL, [](uint64_t s, auto&& kvp) {
        return s + kvp.second;
    });
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<cmd> cmds;

    std::regex re_mask(R"(mask = ([X01]+))");
    std::regex re_write_mem(R"(mem\[(\d+?)\] = (\d+))");

    for (const auto& line : sr::lines(input)) {
        try {
            std::string mask;
            sr::parse(re_mask, line, mask);
            cmds.emplace_back(cmd_set_mask{std::move(mask)});
        } catch (const sr::bad_match&) {
            cmd_write_mem cmd_wm;
            sr::parse(re_write_mem, line, cmd_wm.base_addr, cmd_wm.val);
            cmds.emplace_back(std::move(cmd_wm));
            // intentionally throw here if no match could be made
        }
    }

    sr::solution(process<part1_solver>(cmds));
    sr::solution(process<part2_solver>(cmds));

    return 0;
}
