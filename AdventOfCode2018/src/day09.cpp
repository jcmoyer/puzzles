#include <algorithm>
#include <charconv>
#include <cmath>
#include <cstdint>
#include <deque>
#include <fstream>
#include <iostream>
#include <list>
#include <regex>
#include <string>
#include <vector>

template <typename T>
void circ_inc(typename std::list<T>::iterator& it, std::list<T>& cont) {
    ++it;
    if (it == cont.end())
        it = cont.begin();
}

template <typename T>
void circ_dec(typename std::list<T>::iterator& it, std::list<T>& cont) {
    if (it == cont.begin())
        it = cont.end();
    --it;
}

template <typename T>
void circ_move(typename std::list<T>::iterator& it, int d, std::list<T>& cont) {
    for (int i = 0; i < std::abs(d); ++i) {
        if (d > 0)
            circ_inc(it, cont);
        else
            circ_dec(it, cont);
    }
}

struct player {
    uint64_t score = 0;
};

struct game {
    game(int player_count) : players(player_count) {
        current_player = 0;
        next_marble = 1;
        marbles.push_back(0);
        current_marble = marbles.begin();
    }

    void do_turn() {
        player& p = players[current_player];
        current_player = (current_player + 1) % players.size();
        int marble_to_place = next_marble++;
        if (marble_to_place % 23 == 0) {
            auto where = current_marble;
            circ_move(where, -7, marbles);
            p.score += *where + marble_to_place;
            current_marble = marbles.erase(where);
        } else {
            auto where = current_marble;
            circ_move(where, +2, marbles);
            current_marble = marbles.insert(where, marble_to_place);
        }
    }

    size_t current_player;
    std::vector<player> players;
    std::list<int> marbles;
    std::list<int>::iterator current_marble;
    int next_marble;
};

int main(int argc, char* argv[]) {
    int players;
    int last_marble_val;

    std::ifstream input(argv[1]);
    std::string line;
    if (std::getline(input, line)) {
        std::regex input_regex(R"((\d+) players; last marble is worth (\d+) points)");
        std::cmatch match;
        if (std::regex_match(line.data(), match, input_regex)) {
            std::from_chars(match[1].first, match[1].second, players);
            std::from_chars(match[2].first, match[2].second, last_marble_val);
        }
    } else {
        return 1;
    }

    {
        game g(players);
        for (int i = 0; i < last_marble_val; ++i) {
            g.do_turn();
        }
        auto p = std::max_element(g.players.begin(), g.players.end(), [](auto p0, auto p1) {
            return p0.score < p1.score;
        });
        std::cout << p->score << std::endl;
    }

    {
        game g(players);
        for (int i = 0; i < last_marble_val * 100; ++i) {
            g.do_turn();
        }
        auto p = std::max_element(g.players.begin(), g.players.end(), [](auto p0, auto p1) {
            return p0.score < p1.score;
        });
        std::cout << p->score << std::endl;
    }

    return 0;
}
