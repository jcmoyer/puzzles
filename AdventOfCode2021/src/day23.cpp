#include <fstream>

#include <sr/sr.hpp>

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

[[nodiscard]] size_t energy_per_step(uint8_t ch) {
    switch (ch) {
    case 'A':
        return 1;
    case 'B':
        return 10;
    case 'C':
        return 100;
    case 'D':
        return 1000;
    default:
        assert(false);
        throw std::runtime_error("invalid ch");
    }
}

[[nodiscard]] bool is_room_destination_for(size_t room_id, uint8_t occupant) {
    if (room_id == 0 && occupant == 'A')
        return true;
    if (room_id == 1 && occupant == 'B')
        return true;
    if (room_id == 2 && occupant == 'C')
        return true;
    if (room_id == 3 && occupant == 'D')
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

    uint8_t rooms[4 * RoomSlotCount]{};
    uint8_t halls[5]{};
    uint8_t sides[2]{};

    constexpr auto operator<=>(const world_state&) const = default;

    //=========================================================================
    // field accessors
    //=========================================================================
    //
    // callers should use these accessor functions since the underlying representation may change
    // getters do not return references here because storage optimizations may store data in non-addressable bits

    void set_room(size_t room_id, uint8_t slot, uint8_t who) {
        assert(room_id <= 3);
        assert(slot < slot_count);
        rooms[room_id * slot_count + slot] = who;
    }

    [[nodiscard]] uint8_t get_room(size_t room_id, uint8_t slot) const {
        assert(room_id <= 3);
        assert(slot < slot_count);
        return rooms[room_id * slot_count + slot];
    }

    void set_hall(size_t hall_id, uint8_t who) {
        assert(hall_id <= 4);
        halls[hall_id] = who;
    }

    [[nodiscard]] uint8_t get_hall(size_t hall_id) const {
        assert(hall_id <= 4);
        return halls[hall_id];
    }

    void set_side(size_t side_id, uint8_t who) {
        assert(side_id <= 1);
        sides[side_id] = who;
    }

    [[nodiscard]] uint8_t get_side(size_t side_id) const {
        assert(side_id <= 1);
        return sides[side_id];
    }

    //=========================================================================
    // convenience functions
    //=========================================================================

    [[nodiscard]] uint8_t hallway_occupant(size_t hall_id) const {
        assert(hall_id < sizeof(halls));
        return halls[hall_id];
    }

    [[nodiscard]] bool is_hallway_vacant(size_t hall_id) const {
        assert(hall_id <= sizeof(halls));
        return hallway_occupant(hall_id) == 0;
    }

    [[nodiscard]] bool is_room_full(size_t room_id) const {
        for (int i = 0; i < slot_count; ++i)
            if (get_room(room_id, i) == 0)
                return false;
        return true;
    }

    [[nodiscard]] uint8_t next_room_occupant_slot(size_t room_id) const {
        for (int slot = 0; slot < slot_count; ++slot) {
            if (get_room(room_id, slot) != 0) {
                return slot;
            }
        }
        throw std::runtime_error("no occupant in room");
    }

    [[nodiscard]] uint8_t next_room_empty_slot(size_t room_id) const {
        // very important that we take the LAST empty slot first!
        // took a lot of time to debug this...
        for (int slot = slot_count - 1; slot >= 0; --slot) {
            if (get_room(room_id, slot) == 0) {
                return slot;
            }
        }
        throw std::runtime_error("room full");
    }

    struct room_occupants_result {
        uint8_t occupants[slot_count];

        auto operator<=>(const room_occupants_result&) const = default;

        bool full() const {
            return count() == slot_count;
        }

        size_t count() const {
            return std::ranges::count_if(occupants, [](uint8_t who) {
                return who != 0;
            });
        }

        bool all_occupants_are(uint8_t occ) const {
            for (uint8_t o : occupants) {
                if (o != 0 && occ != o) {
                    return false;
                }
            }
            return true;
        }
    };

    [[nodiscard]] room_occupants_result room_occupants(size_t room_id) const {
        room_occupants_result res;
        for (int i = 0; i < slot_count; ++i) {
            res.occupants[i] = get_room(room_id, i);
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
        return get_side(side_id) != 0;
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
        if (!is_room_destination_for(room_id, hallway_occupant(hall_id)))
            return false;

        auto rdest = room_occupants(room_id);
        if (rdest.count() == slot_count) {
            return false;
        } else if (!rdest.all_occupants_are(hallway_occupant(hall_id))) {
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
        uint8_t who = get_room(room_src, slot);

        auto rdest = room_occupants(room_dst);
        if (rdest.count() == slot_count) {
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
        if (!is_room_destination_for(room_id, get_side(side_id)))
            return false;

        auto rdest = room_occupants(room_id);
        if (rdest.count() == slot_count) {
            return false;
        } else if (!rdest.all_occupants_are(get_side(side_id))) {
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
struct world_explorer {
    State state{};
    size_t energy_used = 0;

    void move_occupant_room_hall(size_t room_id, size_t hall_id) {
        uint8_t slot = state.next_room_occupant_slot(room_id);
        uint8_t who = state.get_room(room_id, slot);
        assert(who != 0);
        assert(state.is_hallway_vacant(hall_id));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_id) - hall_x(hall_id));
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, 0);
        state.set_hall(hall_id, who);
    }

    void move_occupant_hall_room(size_t hall_id, size_t room_id) {
        uint8_t who = state.get_hall(hall_id);
        assert(who != 0);
        assert(!state.is_hallway_vacant(hall_id));
        assert(!state.is_room_full(room_id));

        uint8_t slot = state.next_room_empty_slot(room_id);

        size_t dist = (size_t)std::abs(hall_x(hall_id) - room_x(room_id)) + slot_hall_ydist(slot);
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, who);
        state.set_hall(hall_id, 0);
    }

    void move_occupant_room_room(size_t room_src, size_t room_dst) {
        uint8_t src_slot = state.next_room_occupant_slot(room_src);
        uint8_t who = state.get_room(room_src, src_slot);

        assert(who != 0);
        assert(!state.is_room_full(room_dst));
        assert(is_room_destination_for(room_dst, who));

        uint8_t dst_slot = state.next_room_empty_slot(room_dst);

        size_t dist = (size_t)std::abs(room_x(room_src) - room_x(room_dst)) + slot_hall_ydist(src_slot) +
                      slot_hall_ydist(dst_slot);
        energy_used += dist * energy_per_step(who);

        state.set_room(room_src, src_slot, 0);
        state.set_room(room_dst, dst_slot, who);
    }

    void move_occupant_room_sideroom(size_t room_id, size_t side_id) {
        uint8_t slot = state.next_room_occupant_slot(room_id);
        uint8_t who = state.get_room(room_id, slot);

        assert(who != 0);
        assert(!state.is_sideroom_occupied(side_id));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_id) - side_x(side_id));
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, 0);
        state.set_side(side_id, who);
    }

    void move_occupant_sideroom_room(size_t sideroom_src, size_t room_dst) {
        uint8_t slot = state.next_room_empty_slot(room_dst);
        uint8_t who = state.get_side(sideroom_src);

        assert(who != 0);
        assert(state.is_sideroom_occupied(sideroom_src));
        assert(!state.is_room_full(room_dst));

        size_t dist = slot_hall_ydist(slot) + (size_t)std::abs(room_x(room_dst) - side_x(sideroom_src));
        energy_used += dist * energy_per_step(who);

        state.set_side(sideroom_src, 0);
        state.set_room(room_dst, slot, who);
    }

    template <typename Seq>
    void get_adjacent_states(Seq& buf) {
        // moving from hallway to room, room to hallway
        for (size_t room = 0; room < 4; ++room) {
            for (size_t hall = 0; hall < 5; ++hall) {
                if (state.can_room_hall(room, hall)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_hall(room, hall);
                }
                if (state.can_hall_room(hall, room)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_hall_room(hall, room);
                }
            }
            for (size_t sideroom = 0; sideroom < 2; ++sideroom) {
                if (state.can_room_sideroom(room, sideroom)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_sideroom(room, sideroom);
                }
                if (state.can_sideroom_room(sideroom, room)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_sideroom_room(sideroom, room);
                }
            }
        }

        // moving from room to room
        for (size_t room_src = 0; room_src < 4; ++room_src) {
            for (size_t room_dst = 0; room_dst < 4; ++room_dst) {
                if (state.can_room_room(room_src, room_dst)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_room(room_src, room_dst);
                }
            }
        }
    }

    bool is_goal() const {
        auto r0 = state.room_occupants(0);
        auto r1 = state.room_occupants(1);
        auto r2 = state.room_occupants(2);
        auto r3 = state.room_occupants(3);
        return r0.all_occupants_are('A') && r1.all_occupants_are('B') && r2.all_occupants_are('C') &&
               r3.all_occupants_are('D') && r0.full() && r1.full() && r2.full() && r3.full();
    }

    bool operator==(const world_explorer& rhs) const {
        return state == rhs.state;
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
            ws1.state.set_room(j, i, amphipods[i * 4 + j]);
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
            ws.state.set_room(j, i, amphipods[i * 4 + j]);
        }
    }
    solve(ws);

    return 0;
}
