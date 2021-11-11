#ifndef COMMON_ELFASM_HPP
#define COMMON_ELFASM_HPP

#include <string_view>

enum opcode {
#define ELF_OPCODE(name) name,
#include "common/elfasm.def"
    OPCODE_MAX
};

opcode str2opcode(std::string_view v) {
#define ELF_OPCODE(name)                                                                                              \
    if (v == "" #name)                                                                                                \
        return name;
#include "common/elfasm.def"
    throw "invalid op";
}

std::string_view opcode2str(opcode op) {
    switch (op) {
#define ELF_OPCODE(name)                                                                                              \
    case name:                                                                                                        \
        return "" #name;
#include "common/elfasm.def"
    }
    throw "invalid op";
}

struct instruction {
    opcode op;
    int a;
    int b;
    int c;
};

struct program {
    std::vector<instruction> instructions;
};

struct interpreter {
    using register_type = int64_t;

    static constexpr std::size_t NO_REGISTER_MAPPING = std::numeric_limits<std::size_t>::max();

    std::vector<register_type> registers;
    program* _program;
    register_type _ip;
    std::size_t _ip_reg = NO_REGISTER_MAPPING;

    interpreter(std::size_t register_count) : registers(register_count), _program{nullptr} {}

    bool step() {
        std::size_t ip = get_instruction_pointer();
        if (ip < _program->instructions.size()) {
            do_instruction(_program->instructions[ip]);
            bump_instruction_pointer();
            return true;
        } else {
            return false;
        }
    }

    void program(program* p) {
        _program = p;
        reset_instruction_pointer();
    }

    const ::program* program() const {
        return _program;
    }

    void bind_instruction_pointer(std::size_t reg) {
        if (reg < registers.size()) {
            _ip_reg = reg;
        } else {
            throw std::range_error("reg out of range");
        }
    }

    std::size_t get_instruction_pointer() const {
        if (_ip_reg != NO_REGISTER_MAPPING) {
            return registers[_ip_reg];
        } else {
            return _ip;
        }
    }

    void reset() {
        reset_instruction_pointer();
        reset_registers();
    }

    void reset_registers() {
        for (std::size_t i = 0; i < registers.size(); ++i) {
            registers[i] = 0;
        }
    }

private:
    void bump_instruction_pointer() {
        if (_ip_reg != NO_REGISTER_MAPPING) {
            ++registers[_ip_reg];
        } else {
            ++_ip;
        }
    }

    void reset_instruction_pointer() {
        if (_ip_reg != NO_REGISTER_MAPPING) {
            registers[_ip_reg] = 0;
        } else {
            _ip = 0;
        }
    }

    void do_instruction(const instruction& i) {
        switch (i.op) {
        case addr:
            registers[i.c] = registers[i.a] + registers[i.b];
            break;
        case addi:
            registers[i.c] = registers[i.a] + i.b;
            break;
        case mulr:
            registers[i.c] = registers[i.a] * registers[i.b];
            break;
        case muli:
            registers[i.c] = registers[i.a] * i.b;
            break;
        case banr:
            registers[i.c] = registers[i.a] & registers[i.b];
            break;
        case bani:
            registers[i.c] = registers[i.a] & i.b;
            break;
        case borr:
            registers[i.c] = registers[i.a] | registers[i.b];
            break;
        case bori:
            registers[i.c] = registers[i.a] | i.b;
            break;
        case setr:
            registers[i.c] = registers[i.a];
            break;
        case seti:
            registers[i.c] = i.a;
            break;
        case gtir:
            registers[i.c] = i.a > registers[i.b];
            break;
        case gtri:
            registers[i.c] = registers[i.a] > i.b;
            break;
        case gtrr:
            registers[i.c] = registers[i.a] > registers[i.b];
            break;
        case eqir:
            registers[i.c] = i.a == registers[i.b];
            break;
        case eqri:
            registers[i.c] = registers[i.a] == i.b;
            break;
        case eqrr:
            registers[i.c] = registers[i.a] == registers[i.b];
            break;
        }
    }
};

struct instruction_info {
    int64_t count = 0;
    bool exit = false;
};

struct profiler_results {
    std::vector<instruction_info> _iinfo;
    const instruction_info& get_instruction_info(std::size_t i) const {
        return _iinfo[i];
    }
};

struct profiler {
public:
    profiler() {}

    void run(interpreter& interp, profiler_results& results) {
        const program* prog = interp.program();
        if (results._iinfo.size() < prog->instructions.size()) {
            results._iinfo.resize(prog->instructions.size());
        }
        int64_t last_ip = 0;
        int64_t ip = 0;
        for (;;) {
            last_ip = ip;
            ip = interp.get_instruction_pointer();
            if (!interp.step()) {
                results._iinfo[last_ip].exit = true;
                break;
            }
            instruction_info& iinfo = results._iinfo[ip];
            ++iinfo.count;
        }
    }
};

void dump_profiler_report(const profiler_results& results, const program& prog, std::ostream& output) {
    char buf[512]{0};

    sprintf(buf, "%-8s | %-8s | %s", "count", "address", "instruction");
    output << buf << std::endl;

    for (std::size_t i = 0; i < prog.instructions.size(); ++i) {

        const instruction& instr = prog.instructions[i];
        const auto& info = results.get_instruction_info(i);

        if (info.exit) {
            sprintf(buf,
                "%-8d | %08d | %s %-08x %08x %-08x    < EXIT",
                info.count,
                i,
                opcode2str(instr.op).data(),
                instr.a,
                instr.b,
                instr.c);
        } else {
            sprintf(buf,
                "%-8d | %08d | %s %-08x %08x %-08x",
                info.count,
                i,
                opcode2str(instr.op).data(),
                instr.a,
                instr.b,
                instr.c);
        }

        output << buf << std::endl;
    }
}

struct jump {
    std::size_t from;
    std::size_t to;
};

struct instruction_range {
    std::size_t first, last;
};

struct cfg_context;
struct cfg_node {
    cfg_context* ctx;
    instruction_range range;

    void split(std::size_t n) {}
};

struct cfg_context {};

std::vector<jump> cfa(program& p, int64_t reg) {
    cfg_context ctx;
    cfg_node node;

    for (auto& i : p.instructions) {
        if (i.c != reg)
            continue;
    }

    return {};
}

#endif
