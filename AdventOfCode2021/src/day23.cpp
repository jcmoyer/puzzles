#include <bitset>
#include <fstream>

#include <sr/sr.hpp>

enum class amphipod : uint8_t {
    none = 0,
    a = 1,
    b = 2,
    c = 3,
    d = 4,
};

[[nodiscard]] amphipod to_amphipod(char ch) {
    switch (ch) {
    case 'A':
        return amphipod::a;
    case 'B':
        return amphipod::b;
    case 'C':
        return amphipod::c;
    case 'D':
        return amphipod::d;
    default:
        throw std::runtime_error("invalid ch");
    }
}

// it is intended that these functions be used in a context where a negative value is expected, so they must return a
// signed integer
[[nodiscard]] int side_x(size_t side_id) {
    assert(side_id <= 1);
    if (side_id == 0)
        return 1;
    else if (side_id == 1)
        return 11;
    // explicitly undefined if side_id not in 0..1
    return -1;
}

[[nodiscard]] int room_x(size_t room_id) {
    return 3 + (int)room_id * 2;
}

[[nodiscard]] int hall_x(size_t hall_id) {
    return 2 + (int)hall_id * 2;
}

[[nodiscard]] int hall_y() {
    return 1;
}

// distance is always positive
[[nodiscard]] size_t slot_hall_ydist(uint8_t slot) {
    return slot + 1;
}

[[nodiscard]] size_t energy_per_step(amphipod ch) {
    switch (ch) {
    case amphipod::a:
        return 1;
    case amphipod::b:
        return 10;
    case amphipod::c:
        return 100;
    case amphipod::d:
        return 1000;
    default:
        assert(false);
        throw std::runtime_error("invalid ch");
    }
}

[[nodiscard]] bool is_room_destination_for(size_t room_id, amphipod occupant) {
    if (room_id == 0 && occupant == amphipod::a)
        return true;
    if (room_id == 1 && occupant == amphipod::b)
        return true;
    if (room_id == 2 && occupant == amphipod::c)
        return true;
    if (room_id == 3 && occupant == amphipod::d)
        return true;
    return false;
}

[[nodiscard]] bool is_sideroom_left(size_t side_id) {
    return side_id == 0;
}

[[nodiscard]] size_t hall_id_left_of_room(size_t room_id) {
    return room_id;
}

[[nodiscard]] size_t hall_id_right_of_room(size_t room_id) {
    return room_id + 1;
}

[[nodiscard]] bool is_hall_left_of_room(size_t room, size_t hall) {
    return hall <= room;
}

[[nodiscard]] bool is_hall_right_of_room(size_t room, size_t hall) {
    return hall > room;
}

template <size_t RoomSlotCount>
struct world_state {
    constexpr static size_t slot_count = RoomSlotCount;

    // not yet optimized, each location can be represented with the following bit pattern:
    //
    //   0 - vacant
    //   1 - A
    //  10 - B
    //  11 - C
    // 100 - D
    //
    // alternatively, one bit for each location * 4 creature types?
    //
    // there are 15 locations in part 1 and 23 locations in part 2

    amphipod rooms[4 * RoomSlotCount]{};
    amphipod halls[5]{};
    amphipod sides[2]{};

    constexpr auto operator<=>(const world_state&) const = default;

    //=========================================================================
    // field accessors
    //=========================================================================
    //
    // callers should use these accessor functions since the underlying representation may change
    // getters do not return references here because storage optimizations may store data in non-addressable bits

    void set_room(size_t room_id, uint8_t slot, amphipod who) {
        assert(room_id <= 3);
        assert(slot < slot_count);
        rooms[room_id * slot_count + slot] = who;
    }

    [[nodiscard]] amphipod get_room(size_t room_id, uint8_t slot) const {
        assert(room_id <= 3);
        assert(slot < slot_count);
        return rooms[room_id * slot_count + slot];
    }

    void set_hall(size_t hall_id, amphipod who) {
        assert(hall_id <= 4);
        halls[hall_id] = who;
    }

    [[nodiscard]] amphipod get_hall(size_t hall_id) const {
        assert(hall_id <= 4);
        return halls[hall_id];
    }

    void set_side(size_t side_id, amphipod who) {
        assert(side_id <= 1);
        sides[side_id] = who;
    }

    [[nodiscard]] amphipod get_side(size_t side_id) const {
        assert(side_id <= 1);
        return sides[side_id];
    }
};

template <size_t RoomSlotCount>
struct small_world_state {
    constexpr static size_t slot_count = RoomSlotCount;

    template <size_t RoomSlotCount>
    struct room_int {};
    template <>
    struct room_int<2> {
        using type = uint32_t;
    };
    template <>
    struct room_int<4> {
        using type = uint64_t;
    };

    typename room_int<RoomSlotCount>::type room_bits{};
    uint32_t rest_bits{};

    constexpr auto operator<=>(const small_world_state&) const = default;

    //=========================================================================
    // field accessors
    //=========================================================================
    void set_room(size_t room_id, uint8_t slot, amphipod who) {
        const uint64_t shift = room_id * 3 * slot_count + slot * 3;
        const uint64_t mask = 0b111ull << shift;
        const uint64_t whobits = ((uint64_t)who) << shift;
        room_bits = (room_bits & (~mask)) | whobits;
    }

    [[nodiscard]] amphipod get_room(size_t room_id, uint8_t slot) const {
        const uint64_t shift = room_id * 3 * slot_count + slot * 3;
        const uint64_t mask = 0b111ull << shift;
        return static_cast<amphipod>((room_bits & mask) >> shift);
    }

    void set_hall(size_t hall_id, amphipod who) {
        const uint64_t shift = hall_id * 3;
        const uint64_t mask = 0b111ull << shift;
        const uint64_t whobits = ((uint64_t)who) << shift;
        rest_bits = (rest_bits & (~mask)) | whobits;
    }

    [[nodiscard]] amphipod get_hall(size_t hall_id) const {
        const uint64_t shift = hall_id * 3;
        const uint64_t mask = 0b111ull << shift;
        return static_cast<amphipod>((rest_bits & mask) >> shift);
    }

    void set_side(size_t side_id, amphipod who) {
        const uint64_t shift = 18 + side_id * 3;
        const uint64_t mask = 0b111ull << shift;
        const uint64_t whobits = ((uint64_t)who) << shift;
        rest_bits = (rest_bits & (~mask)) | whobits;
    }

    [[nodiscard]] amphipod get_side(size_t side_id) const {
        const uint64_t shift = 18 + side_id * 3;
        const uint64_t mask = 0b111ull << shift;
        return static_cast<amphipod>((rest_bits & mask) >> shift);
    }
};

template <size_t RoomSlotCount>
struct small_world_array_state {
    constexpr static size_t slot_count = RoomSlotCount;
    constexpr static size_t total_slot_count = 4 * RoomSlotCount;

    constexpr static size_t room_start = 0;
    constexpr static size_t room_bit_count = total_slot_count;

    constexpr static size_t hall_start = room_start + room_bit_count;
    constexpr static size_t hall_bit_count = 5;

    constexpr static size_t sideroom_start = hall_start + hall_bit_count;
    constexpr static size_t sideroom_bit_count = 2;

    constexpr static size_t total_bit_count = room_bit_count + hall_bit_count + sideroom_bit_count;

    std::bitset<total_bit_count> bits[4];

    constexpr auto operator<=>(const small_world_array_state&) const = default;

    //=========================================================================
    // field accessors
    //=========================================================================
    void set_room(size_t room_id, uint8_t slot, amphipod who) {
        const size_t pos = room_start + room_id * slot_count + slot;
        bits[0].set(pos, false);
        bits[1].set(pos, false);
        bits[2].set(pos, false);
        bits[3].set(pos, false);
        if (who != amphipod::none)
            bits[(size_t)who - 1].set(pos, true);
    }

    [[nodiscard]] amphipod get_room(size_t room_id, uint8_t slot) const {
        const size_t pos = room_start + room_id * slot_count + slot;
        for (int i = 0; i < 4; ++i) {
            if (bits[i].test(pos))
                return (amphipod)(i + 1);
        }
        return amphipod::none;
    }

    void set_hall(size_t hall_id, amphipod who) {
        const size_t pos = hall_start + hall_id;
        bits[0].set(pos, false);
        bits[1].set(pos, false);
        bits[2].set(pos, false);
        bits[3].set(pos, false);
        if (who != amphipod::none)
            bits[(size_t)who - 1].set(pos, true);
    }

    [[nodiscard]] amphipod get_hall(size_t hall_id) const {
        const size_t pos = hall_start + hall_id;
        for (int i = 0; i < 4; ++i) {
            if (bits[i].test(pos))
                return (amphipod)(i + 1);
        }
        return amphipod::none;
    }

    void set_side(size_t side_id, amphipod who) {
        const size_t pos = sideroom_start + side_id;
        bits[0].set(pos, false);
        bits[1].set(pos, false);
        bits[2].set(pos, false);
        bits[3].set(pos, false);
        if (who != amphipod::none)
            bits[(size_t)who - 1].set(pos, true);
    }

    [[nodiscard]] amphipod get_side(size_t side_id) const {
        const size_t pos = sideroom_start + side_id;
        for (int i = 0; i < 4; ++i) {
            if (bits[i].test(pos))
                return (amphipod)(i + 1);
        }
        return amphipod::none;
    }
};

template <typename State>
struct world_explorer {
    State state{};
    size_t energy_used = 0;

    void move_occupant_room_hall(size_t room_id, size_t hall_id) {
        uint8_t slot = next_room_occupant_slot(room_id);
        amphipod who = state.get_room(room_id, slot);
        assert(who != amphipod::none);
        assert(is_hallway_vacant(hall_id));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_id) - hall_x(hall_id));
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, amphipod::none);
        state.set_hall(hall_id, who);
    }

    void move_occupant_hall_room(size_t hall_id, size_t room_id) {
        amphipod who = state.get_hall(hall_id);
        assert(who != amphipod::none);
        assert(!is_hallway_vacant(hall_id));
        assert(!is_room_full(room_id));

        uint8_t slot = next_room_empty_slot(room_id);

        size_t dist = (size_t)std::abs(hall_x(hall_id) - room_x(room_id)) + slot_hall_ydist(slot);
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, who);
        state.set_hall(hall_id, amphipod::none);
    }

    void move_occupant_room_room(size_t room_src, size_t room_dst) {
        uint8_t src_slot = next_room_occupant_slot(room_src);
        amphipod who = state.get_room(room_src, src_slot);

        assert(who != amphipod::none);
        assert(!is_room_full(room_dst));
        assert(is_room_destination_for(room_dst, who));

        uint8_t dst_slot = next_room_empty_slot(room_dst);

        size_t dist = (size_t)std::abs(room_x(room_src) - room_x(room_dst)) + slot_hall_ydist(src_slot) +
                      slot_hall_ydist(dst_slot);
        energy_used += dist * energy_per_step(who);

        state.set_room(room_src, src_slot, amphipod::none);
        state.set_room(room_dst, dst_slot, who);
    }

    void move_occupant_room_sideroom(size_t room_id, size_t side_id) {
        uint8_t slot = next_room_occupant_slot(room_id);
        amphipod who = state.get_room(room_id, slot);

        assert(who != amphipod::none);
        assert(!is_sideroom_occupied(side_id));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_id) - side_x(side_id));
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, amphipod::none);
        state.set_side(side_id, who);
    }

    void move_occupant_sideroom_room(size_t sideroom_src, size_t room_dst) {
        uint8_t slot = next_room_empty_slot(room_dst);
        amphipod who = state.get_side(sideroom_src);

        assert(who != amphipod::none);
        assert(is_sideroom_occupied(sideroom_src));
        assert(!is_room_full(room_dst));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_dst) - side_x(sideroom_src));
        energy_used += dist * energy_per_step(who);

        state.set_side(sideroom_src, amphipod::none);
        state.set_room(room_dst, slot, who);
    }

    template <typename Seq>
    void get_adjacent_states(Seq& buf) {
        // moving from hallway to room, room to hallway
        for (size_t room = 0; room < 4; ++room) {
            for (size_t hall = 0; hall < 5; ++hall) {
                if (can_room_hall(room, hall)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_hall(room, hall);
                }
                if (can_hall_room(hall, room)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_hall_room(hall, room);
                }
            }
            for (size_t sideroom = 0; sideroom < 2; ++sideroom) {
                if (can_room_sideroom(room, sideroom)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_sideroom(room, sideroom);
                }
                if (can_sideroom_room(sideroom, room)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_sideroom_room(sideroom, room);
                }
            }
        }

        // moving from room to room
        for (size_t room_src = 0; room_src < 4; ++room_src) {
            for (size_t room_dst = 0; room_dst < 4; ++room_dst) {
                if (can_room_room(room_src, room_dst)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_room(room_src, room_dst);
                }
            }
        }
    }

    bool is_goal() const {
        auto r0 = room_occupants(0);
        auto r1 = room_occupants(1);
        auto r2 = room_occupants(2);
        auto r3 = room_occupants(3);
        return r0.all_occupants_are(amphipod::a) && r1.all_occupants_are(amphipod::b) &&
               r2.all_occupants_are(amphipod::c) && r3.all_occupants_are(amphipod::d) && r0.full() && r1.full() &&
               r2.full() && r3.full();
    }

    bool operator==(const world_explorer& rhs) const {
        return state == rhs.state;
    }

    //=========================================================================
    // convenience functions
    //=========================================================================

    [[nodiscard]] bool is_hallway_vacant(size_t hall_id) const {
        return state.get_hall(hall_id) == amphipod::none;
    }

    [[nodiscard]] bool is_room_full(size_t room_id) const {
        for (int i = 0; i < State::slot_count; ++i)
            if (state.get_room(room_id, i) == amphipod::none)
                return false;
        return true;
    }

    [[nodiscard]] uint8_t next_room_occupant_slot(size_t room_id) const {
        for (int slot = 0; slot < State::slot_count; ++slot) {
            if (state.get_room(room_id, slot) != amphipod::none) {
                return slot;
            }
        }
        throw std::runtime_error("no occupant in room");
    }

    [[nodiscard]] uint8_t next_room_empty_slot(size_t room_id) const {
        // very important that we take the LAST empty slot first!
        // took a lot of time to debug this...
        for (int slot = State::slot_count - 1; slot >= 0; --slot) {
            if (state.get_room(room_id, slot) == amphipod::none) {
                return slot;
            }
        }
        throw std::runtime_error("room full");
    }

    struct room_occupants_result {
        amphipod occupants[State::slot_count];

        auto operator<=>(const room_occupants_result&) const = default;

        bool full() const {
            return count() == State::slot_count;
        }

        size_t count() const {
            return std::ranges::count_if(occupants, [](amphipod who) {
                return who != amphipod::none;
            });
        }

        bool all_occupants_are(amphipod occ) const {
            for (amphipod o : occupants) {
                if (o != amphipod::none && occ != o) {
                    return false;
                }
            }
            return true;
        }
    };

    [[nodiscard]] room_occupants_result room_occupants(size_t room_id) const {
        room_occupants_result res;
        for (int i = 0; i < State::slot_count; ++i) {
            res.occupants[i] = state.get_room(room_id, i);
        }
        return res;
    }

    bool is_hallway_pathable(size_t hall_from, size_t hall_to) const {
        for (size_t i = std::min(hall_from, hall_to); i <= std::max(hall_from, hall_to); ++i) {
            if (!is_hallway_vacant(i))
                return false;
        }
        return true;
    }

    [[nodiscard]] bool is_sideroom_occupied(size_t side_id) const {
        return state.get_side(side_id) != amphipod::none;
    }

    [[nodiscard]] bool is_sideroom_vacant(size_t side_id) const {
        return !is_sideroom_occupied(side_id);
    }

    //=========================================================================
    // adjacent state queries
    //=========================================================================

    [[nodiscard]] bool can_room_hall(size_t room_id, size_t hall_id) const {
        auto rsrc = room_occupants(room_id);
        if (rsrc.count() == 0) {
            return false;
        }

        // check for hallway blockage
        if (is_hall_left_of_room(room_id, hall_id)) {
            size_t start = hall_id;
            size_t end = hall_id_left_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        } else {
            size_t start = hall_id;
            size_t end = hall_id_right_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        }

        return true;
    }

    [[nodiscard]] bool can_hall_room(size_t hall_id, size_t room_id) const {
        if (is_hallway_vacant(hall_id))
            return false;
        if (!is_room_destination_for(room_id, state.get_hall(hall_id)))
            return false;

        auto rdest = room_occupants(room_id);
        if (rdest.count() == State::slot_count) {
            return false;
        } else if (!rdest.all_occupants_are(state.get_hall(hall_id))) {
            return false;
        }

        // check for hallway blockage
        if (is_hall_left_of_room(room_id, hall_id)) {
            // edge case: can't use is_hallway_pathable because when start > end we want to skip the pathing check
            size_t start = hall_id + 1;
            size_t end = hall_id_left_of_room(room_id);
            for (size_t i = start; i <= end; ++i) {
                if (!is_hallway_vacant(i))
                    return false;
            }
        } else {
            size_t start = hall_id - 1;
            size_t end = hall_id_right_of_room(room_id);
            for (size_t i = end; i <= start; ++i) {
                if (!is_hallway_vacant(i))
                    return false;
            }
        }

        return true;
    }

    [[nodiscard]] bool can_room_room(size_t room_src, size_t room_dst) const {
        if (room_src == room_dst)
            return false;
        if (room_occupants(room_src).count() == 0)
            return false;

        uint8_t slot = next_room_occupant_slot(room_src);
        amphipod who = state.get_room(room_src, slot);

        auto rdest = room_occupants(room_dst);
        if (rdest.count() == State::slot_count) {
            return false;
        } else if (!rdest.all_occupants_are(who)) {
            return false;
        }

        // check for hallway blockage
        if (room_src < room_dst) {
            size_t start = hall_id_right_of_room(room_src);
            size_t end = hall_id_left_of_room(room_dst);
            if (!is_hallway_pathable(start, end))
                return false;
        } else {
            size_t start = hall_id_left_of_room(room_src);
            size_t end = hall_id_right_of_room(room_dst);
            if (!is_hallway_pathable(start, end))
                return false;
        }

        return is_room_destination_for(room_dst, who);
    }

    [[nodiscard]] bool can_room_sideroom(size_t room_id, size_t side_dest) const {
        if (is_sideroom_occupied(side_dest))
            return false;

        auto rsrc = room_occupants(room_id);
        if (rsrc.count() == 0) {
            return false;
        }

        // check for hallway blockage
        if (is_sideroom_left(side_dest)) {
            size_t start = 0;
            size_t end = hall_id_left_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        } else {
            size_t start = 4;
            size_t end = hall_id_right_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        }

        return true;
    }

    [[nodiscard]] bool can_sideroom_room(size_t side_id, size_t room_id) const {
        if (!is_sideroom_occupied(side_id))
            return false;
        if (!is_room_destination_for(room_id, state.get_side(side_id)))
            return false;

        auto rdest = room_occupants(room_id);
        if (rdest.count() == State::slot_count) {
            return false;
        } else if (!rdest.all_occupants_are(state.get_side(side_id))) {
            return false;
        }

        // check for hallway blockage
        if (is_sideroom_left(side_id)) {
            size_t start = 0;
            size_t end = hall_id_left_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        } else {
            size_t start = 4;
            size_t end = hall_id_right_of_room(room_id);
            if (!is_hallway_pathable(start, end))
                return false;
        }

        return true;
    }
};

template <typename State>
std::ostream& operator<<(std::ostream& stream, const world_explorer<State>& ws) {
    sr::array2d<char> space(13, 5 + State::slot_count, '#');
    for (int i = 1; i < 12; ++i)
        space.at(i, 1) = '.';
    for (int x = 3; x < 10; x += 2) {
        for (int y = 0; y < 2; ++y) {
            space.at(x, 2 + y) = '.';
        }
    }
    for (int i = 0; i < 5; ++i) {
        int x = hall_x(i);
        int y = 1;
        space.at(x, y) = ws.state.get_hall(i);
    }

    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < State::slot_count; ++j) {
            int x = room_x(i);
            int y = 2 + j;
            space.at(x, y) = ws.state.get_room(i, j);
        }
    }

    for (int i = 0; i < 2; ++i) {
        int x = side_x(i);
        int y = 1;
        space.at(x, y) = ws.state.get_side(i);
    }

    for (int y = 0; y < space.height(); ++y) {
        for (int x = 0; x < space.width(); ++x) {
            char ch = space.at(x, y);
            stream << ch;
        }
        stream << "\n";
    }
    return stream;
}

template <typename State>
struct std::hash<world_explorer<State>> {
    size_t operator()(const world_explorer<State>& ws) const {
        return XXH3_64bits(&ws.state, sizeof(ws.state));
    }
};

template <typename State>
void solve(world_explorer<State> ws0) {
    std::unordered_set<world_explorer<State>> seen;
    std::deque<world_explorer<State>> worlds;
    worlds.push_back(ws0);
    size_t best_score = -1;

    std::deque<world_explorer<State>> world_buf;
    while (worlds.size()) {
        world_explorer<State> ws = worlds.front();
        worlds.pop_front();
        seen.insert(ws);

        // std::cout << ws << "\n";
        // std::cin.get();

        if (ws.is_goal()) {
            if (ws.energy_used < best_score) {
                best_score = ws.energy_used;
                /*sr::solution(best_score);
                std::cout << ws << "\n";*/
            }
            continue;
        }

        if (ws.energy_used <= best_score) {
            ws.get_adjacent_states(world_buf);
            for (auto& w : world_buf) {
                if (!seen.contains(w)) {
                    worlds.push_back(w);
                }
            }
            world_buf.clear();
        }
    }

    sr::solution(best_score);
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    // example input
    //
    // #############
    // #...........#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########

    std::string amphipods;
    amphipods.reserve(16);
    std::string buf;
    while (std::getline(args.get_input_stream(), buf)) {
        for (char ch : buf) {
            if (ch >= 'A' && ch <= 'D') {
                amphipods.push_back(ch);
            }
        }
    }

    // part 1
    world_explorer<world_state<2>> ws1;
    for (size_t i = 0; i < 2; ++i) {
        for (size_t j = 0; j < 4; ++j) {
            ws1.state.set_room(j, i, to_amphipod(amphipods[i * 4 + j]));
        }
    }
    solve(ws1);

    // part 2 inserts the following rows between the existing ones:
    //
    //   #D#C#B#A#
    //   #D#B#A#C#

    amphipods.insert(4, "DCBADBAC");

    world_explorer<world_state<4>> ws;
    for (size_t i = 0; i < 4; ++i) {
        for (size_t j = 0; j < 4; ++j) {
            ws.state.set_room(j, i, to_amphipod(amphipods[i * 4 + j]));
        }
    }
    solve(ws);

    return 0;
}
