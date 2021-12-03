#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

struct instr {
    std::string dir;
    int amt;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    std::vector<instr> ent;
    for (const auto& line : sr::lines(input)) {
        instr& i = ent.emplace_back();
        sr::parse(R"((\w+) (\d+))", line, i.dir, i.amt);
    }

    int pos = 0;
    int depth = 0;
    int aim = 0;

    for (auto& e : ent) {
        if (e.dir == "forward") {
            pos += e.amt;
        } else if (e.dir == "down") {
            depth += e.amt;
        } else if (e.dir == "up") {
            depth -= e.amt;
        }
    }
    sr::solution(pos * depth);

    pos = 0;
    depth = 0;
    aim = 0;
    for (auto& e : ent) {
        if (e.dir == "forward") {
            pos += e.amt;
            depth += aim * e.amt;
        } else if (e.dir == "down") {
            aim += e.amt;
        } else if (e.dir == "up") {
            aim -= e.amt;
        }
    }
    sr::solution(pos * depth);
    return 0;
}
