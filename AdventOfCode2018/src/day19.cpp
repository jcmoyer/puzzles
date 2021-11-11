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

#include "common/array2d.hpp"
#include "common/elfasm.hpp"
#include "common/point.hpp"

// 4 element int array with value semantics for convenience
template <typename T = int64_t, std::size_t N = 4>
struct int_seq {
    static constexpr std::size_t SIZE = N;
    std::array<T, SIZE> values;
    int_seq() {
        values.fill(0);
    }
    T& operator[](std::size_t index) {
        return values[index];
    }
    const T& operator[](std::size_t index) const {
        return values[index];
    }
    bool operator==(const int_seq& rhs) const {
        return std::memcmp(values.data(), rhs.values.data(), sizeof(int) * SIZE) == 0;
    }
    bool operator!=(const int_seq& rhs) const {
        return !(*this == rhs);
    }
    int_seq& operator=(const int_seq& rhs) {
        std::memcpy(values.data(), rhs.values.data(), sizeof(int) * SIZE);
        return *this;
    }
};

std::string prologue() {
    return "int main() {\n";
}

std::string prog2c(int init_ip, std::vector<instruction> instructions) {
    std::string prog;

    std::string ip_ref = "R[";
    ip_ref += std::to_string(init_ip);
    ip_ref += "]";

    prog += "#include <stdio.h>\n";
    prog += "int main() {\n";
    prog += "int R[6];\n";
    prog += "R[0] = 1; R[1] = 0; R[2] = 0; R[3] = 0; R[4] = 0; R[5] = 0;\n";
    prog += "while (" + ip_ref + " >= 0 && " + ip_ref + " < " + std::to_string(instructions.size()) + ") {\n";
    prog += "switch (" + ip_ref + ") {\n";
    int ip = 0;
    for (auto& i : instructions) {
        char buf[1024] = {0};
        switch (i.op) {
        case addr:
            sprintf(buf, "R[%d] = R[%d] + R[%d];", i.c, i.a, i.b);
            break;
        case addi:
            sprintf(buf, "R[%d] = R[%d] + %d;", i.c, i.a, i.b);
            break;
        case mulr:
            sprintf(buf, "R[%d] = R[%d] * R[%d];", i.c, i.a, i.b);
            break;
        case muli:
            sprintf(buf, "R[%d] = R[%d] * %d;", i.c, i.a, i.b);
            break;
        case banr:
            sprintf(buf, "R[%d] = R[%d] & R[%d];", i.c, i.a, i.b);
            break;
        case bani:
            sprintf(buf, "R[%d] = R[%d] & %d;", i.c, i.a, i.b);
            break;
        case borr:
            sprintf(buf, "R[%d] = R[%d] | R[%d];", i.c, i.a, i.b);
            break;
        case bori:
            sprintf(buf, "R[%d] = R[%d] | %d;", i.c, i.a, i.b);
            break;
        case setr:
            sprintf(buf, "R[%d] = R[%d];", i.c, i.a);
            break;
        case seti:
            sprintf(buf, "R[%d] = %d;", i.c, i.a);
            break;
        case gtir:
            sprintf(buf, "R[%d] = %d > R[%d];", i.c, i.a, i.b);
            break;
        case gtri:
            sprintf(buf, "R[%d] = R[%d] > %d;", i.c, i.a, i.b);
            break;
        case gtrr:
            sprintf(buf, "R[%d] = R[%d] > R[%d];", i.c, i.a, i.b);
            break;
        case eqir:
            sprintf(buf, "R[%d] = %d == R[%d];", i.c, i.a, i.b);
            break;
        case eqri:
            sprintf(buf, "R[%d] = R[%d] == %d;", i.c, i.a, i.b);
            break;
        case eqrr:
            sprintf(buf, "R[%d] = R[%d] == R[%d];", i.c, i.a, i.b);
            break;
        }
        prog += "case " + std::to_string(ip++) + ": " + buf + " break;\n";
    }
    prog += "}\n";
    prog += ip_ref + "++;\n";
    prog += "}\n";
    prog += "printf(\"%d\\n\", R[" + std::to_string(0) + "]);\n";
    prog += "return 0;\n}\n";
    return prog;
}

struct problem_data {
    int ip_register = 0;
    std::vector<instruction> program;
};

problem_data load_problem_data(std::istream& input) {
    problem_data data;

    static std::regex DECL(R"(#ip (\d+))");
    static std::regex INSTR(R"((\w+) (\d+) (\d+) (\d+))");

    std::string line;

    while (std::getline(input, line)) {
        std::cmatch match;
        if (std::regex_match(line.data(), match, DECL)) {
            std::from_chars(match[1].first, match[1].second, data.ip_register);
        } else if (std::regex_match(line.data(), match, INSTR)) {
            instruction i;
            i.op = str2opcode(std::string_view(match[1].first, match[1].second - match[1].first));
            std::from_chars(match[2].first, match[2].second, i.a);
            std::from_chars(match[3].first, match[3].second, i.b);
            std::from_chars(match[4].first, match[4].second, i.c);
            data.program.push_back(i);
        }
    }

    return data;
}

std::vector<jump> find_ajmps(std::vector<instruction>& program, int ip_reg) {
    std::vector<jump> results;
    for (size_t i = 0; i < program.size(); ++i) {
        const auto& ins = program[i];
        if (ins.op == seti && ins.c == ip_reg) {
            results.push_back(jump{i, (size_t)ins.a});
        }
    }
    return results;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    problem_data data = load_problem_data(input);

    interpreter interp(6);
    interp.bind_instruction_pointer(data.ip_register);

    program p;
    p.instructions = data.program;
    interp.program(&p);

    while (interp.step()) {
    }

    std::cout << interp.registers[0] << std::endl;

    return 0;
}
