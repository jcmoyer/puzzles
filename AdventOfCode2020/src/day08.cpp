#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <unordered_map>

#include <sr/sr.hpp>

enum opcode {
    OP_NOP,
    OP_JMP,
    OP_ACC,
};

namespace sr {
template <>
struct parse_type<opcode> {
    static opcode parse(const char* first, const char* last) {
        std::string_view s(first, last - first);
        if (s == "nop")
            return OP_NOP;
        if (s == "jmp")
            return OP_JMP;
        if (s == "acc")
            return OP_ACC;
        throw sr::bad_format{};
    }
};
}

struct instruction {
    opcode op;
    int param;
};

using program = std::vector<instruction>;

enum class cpu_state {
    running,
    dup_instr,
    success,
};

class cpu {
public:
    void set_program(program p) {
        reset();
        instructions = std::move(p);
        visited.resize(instructions.size());
    }

    void reset() {
        acc = 0;
        ip = 0;
        state = cpu_state::running;
        instructions.clear();
        visited.clear();
    }

    void step() {
        if (state != cpu_state::running) {
            return;
        }

        if (visited[ip]) {
            state = cpu_state::dup_instr;
            return;
        } else {
            visited[ip] = true;
        }

        const instruction& instr = instructions[ip];

        switch (instr.op) {
        case OP_NOP:
            ++ip;
            break;
        case OP_JMP:
            ip += instr.param;
            break;
        case OP_ACC:
            acc += instr.param;
            ++ip;
            break;
        default:
            throw std::runtime_error("invalid instruction");
        }

        if (ip >= instructions.size()) {
            state = cpu_state::success;
        }
    }

    cpu_state run() {
        while (state == cpu_state::running) {
            step();
        }
        return state;
    }

    int accumulator() const {
        return acc;
    }

private:
    int acc = 0;
    size_t ip = 0;
    cpu_state state = cpu_state::running;
    program instructions;
    std::vector<bool> visited;
};

struct program_repairer {
public:
    program_repairer(const program& p) : prog{p} {}

    program next_program() {
        program new_prog = prog;

        advance_cursor();
        new_prog[replace_cursor].op = new_prog[replace_cursor].op == OP_NOP ? OP_JMP : OP_NOP;

        return new_prog;
    }

private:
    void advance_cursor() {
        for (size_t i = replace_cursor + 1; i < prog.size(); ++i) {
            if (prog[i].op == OP_NOP || prog[i].op == OP_JMP) {
                replace_cursor = i;
                break;
            }
        }
    }

    const program& prog;
    size_t replace_cursor = -1;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    program initial_prog;

    for (const auto& line : sr::lines(input)) {
        auto& i = initial_prog.emplace_back();
        sr::parse(R"((\w+?) ([\+\-]\d+))", line, i.op, i.param);
    }

    cpu vm;
    vm.set_program(initial_prog);
    vm.run();
    sr::solution(vm.accumulator());

    program_repairer rep(initial_prog);
    do {
        vm.set_program(rep.next_program());
    } while (vm.run() != cpu_state::success);
    sr::solution(vm.accumulator());

    return 0;
}
