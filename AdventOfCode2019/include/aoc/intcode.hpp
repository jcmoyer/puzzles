#ifndef INTCODE_HPP
#define INTCODE_HPP

#include <cstdint>
#include <deque>
#include <functional>
#include <istream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

using intcode_memory = std::vector<int64_t>;
using intcode_program = intcode_memory;

template <class CharT, class Traits>
intcode_program load_program(std::basic_istream<CharT, Traits>& input) {
    std::string val;
    intcode_program prog;
    while (std::getline(input, val, ',')) {
        prog.push_back(std::stoll(val));
    }
    return prog;
}

struct intcode_error : std::runtime_error {
    using std::runtime_error::runtime_error;
};

class intcode_vm {
public:
    void set_program(intcode_program prog);

    // Runs the program until it either completes or it awaits more input.
    // Programs that have been suspended because there is no more input can be resumed by calling run again.
    // Calling run on a program that has already halted will have no effect.
    void run();

    inline int64_t read_memory(size_t slot) const;
    inline void write_memory(size_t slot, int64_t val);

    inline void push_input(int64_t val);
    inline void set_output_handler(std::function<void(int64_t)> h);

    inline bool is_halted() const;

private:
    enum vmstate {
        exec,
        wait_input,
        done,
    };

    void step();

    intcode_program mem;
    std::size_t ip = 0;
    int64_t relbase = 0;
    vmstate state = done;
    std::deque<int64_t> input;
    std::function<void(int64_t)> output_handler = [](int64_t) {};
};

inline int64_t intcode_vm::read_memory(size_t slot) const {
    return mem.at(slot);
}

inline void intcode_vm::write_memory(size_t slot, int64_t val) {
    mem.at(slot) = val;
}

inline void intcode_vm::push_input(int64_t val) {
    input.push_back(val);
}

inline void intcode_vm::set_output_handler(std::function<void(int64_t)> h) {
    output_handler = std::move(h);
}

inline bool intcode_vm::is_halted() const {
    return state == done;
}

#endif
