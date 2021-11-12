#include <algorithm>
#include <array>
#include <charconv>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <regex>
#include <set>
#include <string>
#include <vector>

#include <sr/sr.hpp>

#include "common/elfasm.hpp"

using point = sr::vec2i;

// 4 element int array with value semantics for convenience
using int_seq = std::array<int, 4>;

instruction load_instruction(const int_seq& s) {
    return {(opcode)s[0], s[1], s[2], s[3]};
}

struct opstats {
    opcode op_id;
    std::set<int> possible;
};

struct test_case {
    int_seq before, instr_bytes, after;
};

struct problem_data {
    std::vector<test_case> tests;
    std::vector<instruction> program;
};

problem_data load_problem_data(std::istream& input) {
    problem_data data;

    static std::regex BEFORE(R"(Before: \[(\d+), (\d+), (\d+), (\d+)\])");
    static std::regex INSTR(R"((\d+) (\d+) (\d+) (\d+))");
    static std::regex AFTER(R"(After:  \[(\d+), (\d+), (\d+), (\d+)\])");

    const std::size_t NO_TEST_CASE = std::numeric_limits<std::size_t>::max();
    std::size_t test_case_id = NO_TEST_CASE;

    std::string line;

    while (std::getline(input, line)) {
        std::cmatch match;
        if (std::regex_match(line.data(), match, BEFORE)) {
            data.tests.emplace_back();
            test_case_id = data.tests.size() - 1;
            for (int i = 1; i <= 4; ++i) {
                std::from_chars(match[i].first, match[i].second, data.tests[test_case_id].before[i - 1]);
            }
        } else if (std::regex_match(line.data(), match, AFTER)) {
            for (int i = 1; i <= 4; ++i) {
                std::from_chars(match[i].first, match[i].second, data.tests[test_case_id].after[i - 1]);
            }
            test_case_id = NO_TEST_CASE;
        } else if (std::regex_match(line.data(), match, INSTR)) {
            if (test_case_id != NO_TEST_CASE) {
                for (int i = 1; i <= 4; ++i) {
                    std::from_chars(match[i].first, match[i].second, data.tests[test_case_id].instr_bytes[i - 1]);
                }
            } else {
                instruction ins;
                std::from_chars(match[1].first, match[1].second, (int&)ins.op);
                std::from_chars(match[2].first, match[2].second, ins.a);
                std::from_chars(match[3].first, match[3].second, ins.b);
                std::from_chars(match[4].first, match[4].second, ins.c);
                data.program.push_back(ins);
            }
        }
    }

    return data;
}

#include <chrono>

void assign_seq(std::vector<int64_t>& v, const int_seq& s) {
    for (std::size_t i = 0; i < s.size(); ++i) {
        v[i] = s[i];
    }
}

bool equals_seq(const std::vector<int64_t>& v, const int_seq& s) {
    for (std::size_t i = 0; i < s.size(); ++i) {
        if (v[i] != s[i])
            return false;
    }
    return true;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    problem_data data = load_problem_data(input);

    // Part 1
    // mapping of opcode to opstats
    std::vector<opstats> stats;
    stats.resize(OPCODE_MAX);

    int n_tests_work = 0;
    for (const test_case& t : data.tests) {
        interpreter interp(4);
        int n_ops_work = 0;
        for (int i = 0; i < OPCODE_MAX; ++i) {
            assign_seq(interp.registers, t.before);
            program p;

            instruction inst = load_instruction(t.instr_bytes);
            inst.op = (opcode)i;
            p.instructions.push_back(inst);
            // interp.do_instr(inst);
            interp.program(&p);
            interp.step();
            // test failed; move to next opcode
            if (!equals_seq(interp.registers, t.after))
                continue;
            ++n_ops_work;
            // add to stats
            auto& s = stats[inst.op];
            s.op_id = inst.op;
            s.possible.insert(t.instr_bytes[0]);
        }
        if (n_ops_work >= 3) {
            // found 3+ ops for this test case
            ++n_tests_work;
        }
    }

    std::cout << n_tests_work << std::endl;

    // Part 2
    // mapping of raw integer IDs to opcodes
    std::vector<opcode> opmap;
    opmap.resize(OPCODE_MAX);

    for (; stats.size() > 0;) {
        auto next_stat = std::min_element(stats.begin(), stats.end(), [&](opstats& a, opstats& b) {
            return a.possible.size() < b.possible.size();
        });
        if (next_stat->possible.size() == 1) {
            int id = *next_stat->possible.begin();
            opmap[id] = next_stat->op_id;
            stats.erase(next_stat);
            // remove this ID from remaining opcode ID sets
            for (auto& ent : stats) {
                ent.possible.erase(id);
            }
        } else {
            throw std::runtime_error("unsolvable problem");
        }
    }

    interpreter interp(4);
    program p;
    p.instructions = data.program;
    interp.program(&p);

    for (std::size_t i = 0; i < p.instructions.size(); ++i) {
        p.instructions[i].op = (opcode)opmap[(int)p.instructions[i].op];
        interp.step();
    }

    std::cout << interp.registers[0] << std::endl;

    return 0;
}
