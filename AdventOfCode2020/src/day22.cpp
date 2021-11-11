#include <cassert>
#include <fmt/format.h>
#include <fstream>
#include <numeric>
#include <robin_hood.hpp>

#include <sr/sr.hpp>

class arena_storage {
    uint8_t* base;
    uint8_t* cur;
    size_t size;

public:
    arena_storage(size_t bytes) {
        base = (uint8_t*)malloc(bytes);
        cur = base;
        size = bytes;
    }

    void* take(size_t bytes, size_t alignment) {
        // realign cur for alignment
        if ((size_t)cur % alignment != 0) {
            cur = cur - ((size_t)cur % alignment) + alignment;
        }
        size_t remaining = size - (cur - base);
        assert(remaining >= bytes);
        void* ret_ptr = (void*)cur;
        cur += bytes;
        return ret_ptr;
    }
};

template <typename T>
class arena_allocator {
public:
    using value_type = T;
    using pointer = value_type*;

    template <class U>
    constexpr arena_allocator(const arena_allocator<U>& a) noexcept : storage(a.storage) {}

    arena_allocator(arena_storage& store) : storage{store} {}

    pointer allocate(size_t n) {
        return (pointer)storage.take(sizeof(value_type) * n, alignof(value_type));
    }

    void deallocate(pointer, size_t) noexcept {}

private:
    template <class U>
    friend class arena_allocator;

    arena_storage& storage;
};

template <class T, class U>
bool operator==(const arena_allocator<T>& x, const arena_allocator<U>& y) {
    return true;
}
template <class T, class U>
bool operator!=(const arena_allocator<T>& x, const arena_allocator<U>& y) {
    return !(x == y);
}

using deck_int = uint8_t;
using deck = std::vector<deck_int, arena_allocator<deck_int>>;

struct deck_hash {
    XXH3_state_t* state;

    deck_hash() {
        state = XXH3_createState();
    }

    deck_hash(deck_hash&& h) noexcept {
        XXH3_freeState(state);
        state = h.state;
        h.state = nullptr;
    }

    deck_hash& operator=(deck_hash&& h) noexcept {
        XXH3_freeState(state);
        state = h.state;
        h.state = nullptr;
        return *this;
    }

    deck_hash(const deck_hash& h) {
        state = XXH3_createState();
        XXH3_copyState(state, h.state);
    }

    deck_hash& operator=(const deck_hash& h) {
        XXH3_copyState(state, h.state);
        return *this;
    }

    ~deck_hash() {
        if (state)
            XXH3_freeState(state);
    }

    size_t operator()(const deck& d) const {
        XXH3_64bits_reset(state);
        XXH3_64bits_update(state, d.data(), d.size() * sizeof(deck_int));
        return XXH3_64bits_digest(state);
    }
};

static const deck_hash global_hash;

static arena_storage global_storage(1024 * 1024 * 400);
static arena_allocator<deck_int> global_alloc(global_storage);

struct previous_states {
#if 0
    robin_hood::unordered_set<deck, deck_hash> deck1{64, global_hash};
    robin_hood::unordered_set<deck, deck_hash> deck2{64, global_hash};
#else
    std::unordered_set<deck, deck_hash, std::equal_to<deck>, arena_allocator<deck>> deck1{
        64, global_hash, global_alloc};
    std::unordered_set<deck, deck_hash, std::equal_to<deck>, arena_allocator<deck>> deck2{
        64, global_hash, global_alloc};
#endif
};

enum winner { p1_wins, p2_wins };

struct game_state {
    deck player1;
    deck player2;
    previous_states states;

    game_state() : player1(global_alloc), player2(global_alloc) {}

    winner play_normal() {
        while (player1.size() && player2.size()) {
            deck_int c0 = player1.front();
            deck_int c1 = player2.front();
            player1.erase(player1.begin());
            player2.erase(player2.begin());
            if (c0 > c1) {
                player1.emplace_back(c0);
                player1.emplace_back(c1);
            } else if (c1 > c0) {
                player2.emplace_back(c1);
                player2.emplace_back(c0);
            }
        }
        return player1.size() ? p1_wins : p2_wins;
    }

    winner play_recursive() {
        while (player1.size() && player2.size()) {
            winner w;

            if (states.deck1.find(player1) != states.deck1.end() && states.deck2.find(player2) != states.deck2.end()) {
                return p1_wins;
            } else {
                states.deck1.insert(player1);
                states.deck2.insert(player2);
            }

            deck_int c0 = player1.front();
            deck_int c1 = player2.front();
            player1.erase(player1.begin());
            player2.erase(player2.begin());

            if (player1.size() >= c0 && player2.size() >= c1) {
                game_state subgame;
                subgame.player1.insert(subgame.player1.begin(), player1.begin(), player1.begin() + c0);
                subgame.player2.insert(subgame.player2.begin(), player2.begin(), player2.begin() + c1);
                w = subgame.play_recursive();
            } else {
                if (c0 > c1) {
                    w = p1_wins;
                } else {
                    w = p2_wins;
                }
            }

            if (w == p1_wins) {
                player1.emplace_back(c0);
                player1.emplace_back(c1);
            } else if (w == p2_wins) {
                player2.emplace_back(c1);
                player2.emplace_back(c0);
            }
        }
        return player1.size() ? p1_wins : p2_wins;
    }

    size_t winner_score() const {
        winner w = player1.size() ? p1_wins : p2_wins;
        const deck& d = w == p1_wins ? player1 : player2;
        int64_t i = 1;
        int64_t sum = 0;
        for (auto it = d.rbegin(); it != d.rend(); ++it) {
            sum += *it * i;
            ++i;
        }
        return sum;
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::ifstream input(args.input_filename);

    game_state initial_state;

    size_t player = 0;
    for (const auto& line : sr::lines(input)) {
        if (line.size() == 0) {
            ++player;
        } else {
            int card = 0;
            try {
                sr::parse("(\\d+)", line, card);
                if (player == 0)
                    initial_state.player1.emplace_back(card);
                else
                    initial_state.player2.emplace_back(card);
            } catch (sr::bad_match&) {
            }
        }
    }

    {
        game_state st = initial_state;
        st.play_normal();
        sr::solution(st.winner_score());
    }
    {
        game_state st = initial_state;
        st.play_recursive();
        sr::solution(st.winner_score());
    }
    return 0;
}
