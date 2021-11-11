#include "aoc/intcode.hpp"

// clang-format off
enum opcode {
    op_add    = 1, // Ar Br Cw      C <- A + B
    op_mul    = 2, // Ar Br Cw      C <- A * B
    op_input  = 3, // Aw            A <- input
    op_output = 4, // Aw            output(A)
    op_jnz    = 5, // Ar Br         if A != 0 then ip = B
    op_jz     = 6, // Ar Br         if A == 0 then ip = B
    op_lt     = 7, // Ar Br Cw      C <- A < B
    op_eq     = 8, // Ar Br Cw      C <- A == B
    op_rbase  = 9, // Ar            rbase <- rbase + A
    op_halt   = 99 //               halts execution
};

constexpr int op_parameter_count(opcode op) {
    switch (op) {
    case op_add:    return 3;
    case op_mul:    return 3;
    case op_input:  return 1;
    case op_output: return 1;
    case op_jnz:    return 2;
    case op_jz:     return 2;
    case op_lt:     return 3;
    case op_eq:     return 3;
    case op_rbase:  return 1;
    case op_halt:   return 0;
    }
    return 0;
}
// clang-format on

// Parameters that an instruction writes to will never be in immediate mode.
enum operand_mode {
    // The operand is a pointer to a value
    position = 0,
    // The operand is a value
    immediate = 1,
    // The operand is an offset from rbase
    relative = 2,
};

struct memory_ref {
    intcode_memory* container = nullptr;
    size_t index = 0;

    const memory_ref& operator=(int64_t val) const {
        container->at(index) = val;
        return *this;
    }

    operator int64_t&() const {
        return container->at(index);
    }
};

struct instruction {
    static constexpr size_t MAX_PARAMS = 3;

    opcode op;
    operand_mode modes[MAX_PARAMS];
    size_t parameter_count;
    memory_ref params[MAX_PARAMS];

    size_t size() const {
        return 1 + parameter_count;
    }
};

instruction decode_instruction(intcode_memory& mem, size_t where) {
    instruction instr{};
    int64_t raw = mem[where];

    instr.op = static_cast<opcode>(raw % 100);
    raw /= 100;
    instr.modes[0] = static_cast<operand_mode>(raw % 10);
    raw /= 10;
    instr.modes[1] = static_cast<operand_mode>(raw % 10);
    raw /= 10;
    instr.modes[2] = static_cast<operand_mode>(raw % 10);

    instr.parameter_count = op_parameter_count(instr.op);

    if (where + instr.parameter_count >= mem.size()) {
        throw intcode_error("not enough elements to decode instruction from");
    }

    for (size_t i = 0; i < instr.parameter_count; ++i) {
        instr.params[i] = memory_ref{&mem, where + 1 + i};
    }

    return instr;
}

// Returns an int reference into either mem or instr.params depending on the parameter's operand mode.
// Memory access is bounds-checked.
auto operand_ref(intcode_memory& mem, instruction& instr, size_t param, size_t rbase) {
    if (instr.modes[param] == position) {
        size_t address = instr.params[param];
        if (address >= mem.size()) {
            mem.resize(1 + address);
        }
        return memory_ref{&mem, address};
    } else if (instr.modes[param] == relative) {
        size_t address = rbase + instr.params[param];
        if (address >= mem.size()) {
            mem.resize(1 + address);
        }
        return memory_ref{&mem, address};
    } else {
        return instr.params[param];
    }
}

void intcode_vm::step() {
    if (ip >= mem.size())
        throw intcode_error("instruction pointer out of bounds");

    auto instr = decode_instruction(mem, ip);

    switch (instr.op) {
    case op_add: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        memory_ref z = operand_ref(mem, instr, 2, relbase);
        z = x + y;
        ip += instr.size();
        break;
    }
    case op_mul: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        memory_ref z = operand_ref(mem, instr, 2, relbase);
        z = x * y;
        ip += instr.size();
        break;
    }
    case op_input: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        if (input.size()) {
            x = input.front();
            input.pop_front();
            ip += instr.size();
            if (state == wait_input)
                state = exec;
        } else {
            state = wait_input;
        }
        break;
    }
    case op_output: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        output_handler(x);
        ip += instr.size();
        break;
    }
    case op_jnz: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        if (x != 0) {
            ip = y;
        } else {
            ip += instr.size();
        }
        break;
    }
    case op_jz: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        if (x == 0) {
            ip = y;
        } else {
            ip += instr.size();
        }
        break;
    }
    case op_lt: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        memory_ref z = operand_ref(mem, instr, 2, relbase);
        z = x < y;
        ip += instr.size();
        break;
    }
    case op_eq: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        memory_ref y = operand_ref(mem, instr, 1, relbase);
        memory_ref z = operand_ref(mem, instr, 2, relbase);
        z = x == y;
        ip += instr.size();
        break;
    }
    case op_rbase: {
        memory_ref x = operand_ref(mem, instr, 0, relbase);
        relbase += x;
        ip += instr.size();
        break;
    }
    case op_halt:
        state = done;
        return;
    default:
        throw intcode_error("invalid opcode");
    }
}

void intcode_vm::set_program(intcode_program prog) {
    mem = std::move(prog);
    state = exec;
    ip = 0;
    relbase = 0;
}

void intcode_vm::run() {
    if (state == wait_input) {
        step();
    }

    while (state == exec) {
        step();
    }
}
