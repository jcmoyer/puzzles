#include <aoc/intcode.hpp>
#include <fmt/format.h>
#include <fstream>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include <sr/sr.hpp>

enum tile_type {
    empty,
    wall,
    block,
    paddle,
    ball,
};

struct tile {
    int64_t x;
    int64_t y;
    tile_type type;
};

using tilemap = std::unordered_map<sr::point, tile>;

auto count_blocks(const tilemap& tm) {
    return std::count_if(tm.begin(), tm.end(), [](const auto& kvp) {
        return kvp.second.type == block;
    });
}

class breakout {
public:
    breakout(intcode_program prog);

    void run();

    int64_t get_score() const;
    int64_t get_blocks() const;

private:
    void on_vm_output(int64_t message);
    void insert_tile(const tile& t);
    int ideal_input() const;

    enum recvstate { recv_x, recv_y, recv_tile_id, recv_score_y, recv_score };

    tilemap tm;
    intcode_vm vm;
    recvstate state = recv_x;
    // tiles are sent over multiple outputs; intermediate tiles are stored here
    tile current_tile;
    int64_t score = 0;
    int64_t blocks = 0;
    sr::point paddle_pos;
    sr::point ball_pos;
};

breakout::breakout(intcode_program prog) {
    vm.set_program(std::move(prog));
    vm.set_output_handler([this](int64_t msg) {
        on_vm_output(msg);
    });
}

void breakout::run() {
    vm.write_memory(0, 2);
    while (!vm.is_halted()) {
        vm.run();

        if (blocks == 0) {
            blocks = count_blocks(tm);
        }

        vm.push_input(ideal_input());
    }
}

inline int64_t breakout::get_score() const {
    return score;
}

inline int64_t breakout::get_blocks() const {
    return blocks;
}

void breakout::on_vm_output(int64_t message) {
    switch (state) {
    case recv_x:
        if (message == -1) {
            state = recv_score_y;
            break;
        }
        current_tile.x = message;
        state = recv_y;
        break;
    case recv_y:
        current_tile.y = message;
        state = recv_tile_id;
        break;
    case recv_tile_id:
        current_tile.type = (tile_type)message;
        insert_tile(current_tile);
        state = recv_x;
        break;
    case recv_score_y:
        // ignore this message
        state = recv_score;
        break;
    case recv_score:
        score = message;
        state = recv_x;
        break;
    default:
        throw std::runtime_error("unexpected state");
    }
}

void breakout::insert_tile(const tile& t) {
    sr::point tile_point{static_cast<int>(t.x), static_cast<int>(t.y)};
    tm.insert_or_assign(tile_point, current_tile);
    // save paddle and ball coordinates so that we can determine how to move the paddle in relation to the ball
    if (current_tile.type == paddle) {
        paddle_pos = tile_point;
    } else if (current_tile.type == ball) {
        ball_pos = tile_point;
    }
}

int breakout::ideal_input() const {
    if (paddle_pos.x < ball_pos.x) {
        return 1;
    } else if (paddle_pos.x > ball_pos.x) {
        return -1;
    } else {
        return 0;
    }
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    intcode_program prog = load_program(input);

    breakout r(prog);
    r.run();

    fmt::print("{}\n", r.get_blocks());
    fmt::print("{}\n", r.get_score());

    return 0;
}
