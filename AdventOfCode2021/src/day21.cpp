#include <fstream>

#include <sr/sr.hpp>
#include <sr/xxhash.h>

struct deterministic_die {
    uint64_t counter = 1;
    uint64_t total_rolls = 0;

    uint64_t roll() {
        ++total_rolls;
        int val = counter;
        ++counter;
        if (counter > 100)
            counter = 1;
        return val;
    }

    constexpr auto operator<=>(const deterministic_die&) const = default;
};

struct game_state {
    int positions[2]{0, 0};
    uint64_t scores[2]{0, 0};
    size_t current_player = 0;

    int& current_player_pos() {
        return positions[current_player];
    }

    uint64_t& current_player_score() {
        return scores[current_player];
    }

    void switch_player() {
        current_player = !current_player;
    }

    size_t winner(uint64_t target_score) const {
        for (size_t i = 0; i < 2; ++i)
            if (scores[i] >= target_score)
                return i;
        return -1;
    }

    constexpr auto operator<=>(const game_state&) const = default;
};

template <>
struct std::hash<game_state> {
    size_t operator()(const game_state& s) const {
        return XXH3_64bits(&s, sizeof(game_state));
    }
};

struct subwins {
    uint64_t wins[2]{0, 0};

    uint64_t max() const {
        return std::max(wins[0], wins[1]);
    }
};

size_t play_normal(game_state st) {
    deterministic_die die;
    size_t winner = -1;
    while (winner == -1) {
        int dist = 0;
        for (int i = 0; i < 3; ++i) {
            dist += die.roll();
        }
        st.current_player_pos() = (st.current_player_pos() + dist) % 10;
        st.current_player_score() += 1 + st.current_player_pos();
        st.switch_player();
        winner = st.winner(1000);
    }
    return st.scores[!winner] * die.total_rolls;
}

subwins play_dirac(game_state st, std::unordered_map<game_state, subwins>& dirac_wins) {
    size_t next_player = !st.current_player;
    size_t winner = st.winner(21);
    if (winner != -1) {
        subwins sw{};
        sw.wins[winner] = 1;
        return sw;
    }

    if (auto it = dirac_wins.find(st); it != dirac_wins.end()) {
        return it->second;
    }

    subwins total{};
    for (int i = 1; i <= 3; ++i) {
        for (int j = 1; j <= 3; ++j) {
            for (int k = 1; k <= 3; ++k) {
                auto substate = st;
                substate.current_player_pos() += i + j + k;
                substate.current_player_pos() %= 10;
                substate.current_player_score() += 1 + substate.current_player_pos();
                substate.switch_player();

                subwins sw = play_dirac(substate, dirac_wins);
                total.wins[0] += sw.wins[0];
                total.wins[1] += sw.wins[1];
            }
        }
    }

    dirac_wins[st] = total;
    return total;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    game_state init_state;

    for (const auto& line : sr::lines(args.get_input_stream())) {
        int player_index;
        int player_pos;
        sr::parse(R"(Player (\d) starting position: (\d+))", line, player_index, player_pos);
        if (player_index < 1 || player_index > 2) {
            throw std::runtime_error("bad player index");
        }
        init_state.positions[player_index - 1] = player_pos - 1;
    }

    // part 1
    sr::solution(play_normal(init_state));

    // part 2
    std::unordered_map<game_state, subwins> dirac_wins;
    auto sw = play_dirac(init_state, dirac_wins);
    sr::solution(sw.max());

    return 0;
}
