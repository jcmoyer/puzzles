#include <sr/sr.hpp>

size_t move_element(sr::array2d<char>& ca, char what, const sr::vec2i& offset, sr::array2d<char>& out) {
    size_t moved = 0;
    sr::for_each_2d(ca, [&](char c, const sr::vec2i& p) {
        sr::vec2i n = p + offset;
        if (c == what && ca.at_wrap(n) == '.') {
            out[p] = '.';
            out.at_wrap(n) = what;
            ++moved;
        }
    });
    return moved;
}

size_t step_ca(sr::array2d<char>& ca, sr::array2d<char>& out) {
    size_t moved = move_element(ca, '>', {1, 0}, out);
    ca = out;
    moved += move_element(ca, 'v', {0, 1}, out);
    ca = out;
    return moved;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    auto ca = sr::read_tilemap(args.get_input_stream());
    auto out = ca;
    size_t steps = 0;
    while (size_t moved = step_ca(ca, out))
        ++steps;
    sr::solution(steps + 1);
    return 0;
}
