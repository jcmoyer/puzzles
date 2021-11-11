#include <algorithm>
#include <array>
#include <atomic>
#include <charconv>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <mutex>
#include <regex>
#include <set>
#include <string>
#include <thread>
#include <vector>

#include "common/array2d.hpp"
#include "common/elfasm.hpp"
#include "common/point.hpp"

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

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    problem_data data = load_problem_data(input);

    //

    /*
    std::vector<std::thread> threads;
    int64_t steps_min = std::numeric_limits<int64_t>::max();
    int64_t r0_steps = std::numeric_limits<int64_t>::max();

    std::mutex steps_mut;
    std::atomic<bool> running = true;
    for (int i = 0; i < 4; ++i) {
      threads.emplace_back([&, base = 50000 + i, offset = 4]() {
        interpreter interp(6);
        interp.bind_instruction_pointer(data.ip_register);
        program p;
        p.instructions = data.program;
        interp.program(&p);

        int64_t r0 = base + offset;
        int64_t steps = 0;
        for (;;) {
          if (!running) return;
          steps = 0;
          interp.reset();
          interp.registers[0] = r0;
          while (interp.step()) {
            ++steps;
            if (steps > 50000) {
              std::cout << "kill " << r0 << std::endl;
              goto kill;
            }
          }
          steps_mut.lock();
          if (steps < steps_min) {
            steps_min = steps;
            r0_steps = r0;
            std::cout << r0 << " : " << steps_min << std::endl;
          }
          steps_mut.unlock();
        kill:
          r0 += offset;
        }
      });
    }
    */

    {
        interpreter interp(6);
        interp.registers[0] = 3345459;
        interp.bind_instruction_pointer(data.ip_register);
        program p;
        p.instructions = data.program;
        interp.program(&p);
        profiler prof;
        profiler_results prof_results;
        prof.run(interp, prof_results);
        dump_profiler_report(prof_results, p, std::cout);
        std::cin.get();
        return 0;
    }

    std::vector<std::thread> threads;
    int64_t steps_min = std::numeric_limits<int64_t>::min();
    int64_t r0_steps = std::numeric_limits<int64_t>::min();

    std::mutex steps_mut;
    std::atomic<bool> running = true;
    for (int i = 0; i < 4; ++i) {
        threads.emplace_back([&, base = 0 + i, offset = 4]() {
            interpreter interp(6);
            interp.bind_instruction_pointer(data.ip_register);
            program p;
            p.instructions = data.program;
            interp.program(&p);

            int64_t r0 = base + offset;
            int64_t steps = 0;
            for (;;) {
                if (!running)
                    return;
                steps = 0;
                interp.reset();
                interp.registers[0] = r0;
                while (interp.step()) {
                    ++steps;
                    if (steps > 50000) {
                        std::cout << "kill " << r0 << std::endl;
                        goto kill;
                    }
                }
                steps_mut.lock();
                if (steps >= steps_min) {
                    steps_min = steps;
                    r0_steps = r0;
                    std::cout << r0 << " : " << steps_min << std::endl;
                }
                steps_mut.unlock();
            kill:
                r0 += offset;
            }
        });
    }

    std::cin.get();
    running = false;
    for (auto& t : threads)
        t.join();
    std::cout << r0_steps << " : " << steps_min << std::endl;
    std::cin.get();

    return 0;
}
