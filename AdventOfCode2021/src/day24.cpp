#include <variant>
#include <z3++.h>

#include <sr/sr.hpp>

using lit_or_var = std::variant<int64_t, std::string>;

struct instr {
    std::string op;
    lit_or_var arg1;
    lit_or_var arg2;
};

struct instr_constants {
    z3::expr k0, k1, k2;
};

int main(int argc, char* argv[]) {
    using namespace z3;

    auto args = sr::parse_command_line(argc, argv);

    std::vector<instr> program;

    for (const auto& line : sr::lines(args.get_input_stream())) {
        std::string op, a, b;
        try {
            sr::parse(R"(^(\w+) (\-?[\d\w]+) (\-?[\d\w]+)$)", line, op, a, b);
        } catch (const sr::bad_match&) {
            sr::parse(R"(^(\w+) (\-?[\d\w]+)$)", line, op, a);
        }

        auto& ins = program.emplace_back();
        ins.op = op;
        try {
            ins.arg1 = std::stoll(a);
        } catch (std::invalid_argument&) {
            ins.arg1 = a;
        }
        try {
            ins.arg2 = std::stoll(b);
        } catch (std::invalid_argument&) {
            ins.arg2 = b;
        }
    }

    z3::context c;
    z3::optimize s(c);

    // w values (inputs)
    std::vector<z3::expr> ws;

    // z values (outputs)
    std::vector<z3::expr> zs;

    // init variables
    for (size_t i = 0; i < 15; ++i) {
        std::string kname;

        kname = "z" + std::to_string(i);
        zs.emplace_back(c.bv_const(kname.c_str(), 64));

        if (i > 0) {
            kname = "w" + std::to_string(i);
            ws.emplace_back(c.bv_const(kname.c_str(), 64));
        }
    }

    // constrain inputs to range 0..9
    for (auto& w : ws) {
        s.add(w > 0 && w <= 9);
    }

    // output 0 is an input to the first solver step, init to zero
    s.add(zs[0] == 0);
    // final z value is zero for correct serials
    s.add(zs[zs.size() - 1] == 0);

    // [+0]  inp w                                               ; w finalized
    // [+1]  mul x 0   ; x = z % 26
    // [+2]  add x z   ;
    // [+3]  mod x 26  ;
    // [+4]  div z 1   ; z = z / K0          ; varies (K0)
    // [+5]  add x 14  ; x = x + K1          ; varies (K1)
    // [+6]  eql x w   ; x = x != w                              ; x finalized
    // [+7]  eql x 0   ;
    // [+8]  mul y 0   ; y = 25 * x + 1
    // [+9]  add y 25  ;
    // [+10] mul y x   ;
    // [+11] add y 1   ;
    // [+12] mul z y   ; z = z * y
    // [+13] mul y 0   ; y = (w + K2) * x                        ; y finalized
    // [+14] add y w   ;
    // [+15] add y 8   ;                     ; varies (K2)
    // [+16] mul y x   ;
    // [+17] add z y   ; z = z + y                               ; z finalized

    // extract constants from program (Kx)
    std::vector<instr_constants> constants;
    for (size_t i = 0; i < 14; ++i) {
        auto& k = constants.emplace_back(instr_constants{
            c.bv_val(std::get<int64_t>(program[i * 18 + 4].arg2), 64),
            c.bv_val(std::get<int64_t>(program[i * 18 + 5].arg2), 64),
            c.bv_val(std::get<int64_t>(program[i * 18 + 15].arg2), 64),
        });
    }

    // add constraints for each step
    for (size_t i = 1; i < 15; ++i) {
        const instr_constants k = constants[i - 1]; // constants extracted from the input for this step
        const expr& w = ws[i - 1];                  // input for this step
        const expr& z = zs[i];                      // output for this step
        const expr& zlast = zs[i - 1];              // output from last step
        std::string kname;

        kname = "step_x0_" + std::to_string(i);
        auto x0 = c.bv_const(kname.c_str(), 64);
        s.add(x0 == zlast % 26);

        kname = "step_z0_" + std::to_string(i);
        auto z0 = c.bv_const(kname.c_str(), 64);
        s.add(z0 == zlast / k.k0);

        kname = "step_x1_" + std::to_string(i);
        auto x1 = c.bv_const(kname.c_str(), 64);
        s.add(x1 == x0 + k.k1);

        kname = "step_x2_" + std::to_string(i);
        auto x2 = c.bv_const(kname.c_str(), 64);
        s.add(x2 == ite(x1 != w, c.bv_val(1, 64), c.bv_val(0, 64)));

        kname = "step_y0_" + std::to_string(i);
        auto y0 = c.bv_const(kname.c_str(), 64);
        s.add(y0 == 25 * x2 + 1);

        kname = "step_z1_" + std::to_string(i);
        auto z1 = c.bv_const(kname.c_str(), 64);
        s.add(z1 == z0 * y0);

        kname = "step_y1_" + std::to_string(i);
        auto y1 = c.bv_const(kname.c_str(), 64);
        s.add(y1 == (w + k.k2) * x2);

        s.add(z == z1 + y1);
    }

    // part 1: maximize digits
    s.push();
    for (int i = 0; i < ws.size(); ++i) {
        s.maximize(ws[i] * c.bv_val((uint64_t)std::pow(10, i), 64));
    }

    if (sat == s.check()) {
        auto m = s.get_model();
        std::string digits;
        for (auto& w : ws) {
            digits += std::to_string(m.eval(w).as_uint64());
        }
        sr::solution(digits);
    } else {
        return 1;
    }
    s.pop();

    // part 2: minimize digits
    s.push();
    for (int i = 0; i < ws.size(); ++i) {
        s.minimize(ws[i] * c.bv_val((uint64_t)std::pow(10, i), 64));
    }

    if (sat == s.check()) {
        auto m = s.get_model();
        std::string digits;
        for (auto& w : ws) {
            digits += std::to_string(m.eval(w).as_uint64());
        }
        sr::solution(digits);
    } else {
        return 1;
    }
    s.pop();

    return 0;
}
