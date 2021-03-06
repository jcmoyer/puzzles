#include <memory_resource>

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

[[nodiscard]] amphipod amphipod_for_room(size_t room_id) {
    switch (room_id) {
    case 0:
        return amphipod::a;
    case 1:
        return amphipod::b;
    case 2:
        return amphipod::c;
    case 3:
        return amphipod::d;
    }
    return amphipod::none;
}

[[nodiscard]] size_t hall_id_left_of_room(size_t room_id) {
    return room_id + 1;
}

[[nodiscard]] size_t hall_id_right_of_room(size_t room_id) {
    return room_id + 2;
}

// map of distance from first room slot to hallway
constexpr size_t dist_map_rh[4][7] = {
    {3, 2, 2, 4, 6, 8, 9},
    {5, 4, 2, 2, 4, 6, 7},
    {7, 6, 4, 2, 2, 4, 5},
    {9, 8, 6, 4, 2, 2, 3},
};

// map of room to hallway leftness
constexpr bool left_map_rh[4][7] = {
    // clang-format off
    {true, true, false, false, false, false, false},
    {true, true, true,  false, false, false, false},
    {true, true, true,  true,  false, false, false},
    {true, true, true,  true,  true,  false, false},
    // clang-format on
};

[[nodiscard]] bool is_hall_left_of_room(size_t room, size_t hall) {
    return left_map_rh[room][hall];
}

[[nodiscard]] bool is_hall_right_of_room(size_t room, size_t hall) {
    return !is_hall_left_of_room(room, hall);
}

template <size_t RoomSlotCount>
struct world_state {
    constexpr static size_t slot_count = RoomSlotCount;

    amphipod rooms[4 * RoomSlotCount]{};
    amphipod halls[7]{};

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
        assert(hall_id < 7);
        halls[hall_id] = who;
    }

    [[nodiscard]] amphipod get_hall(size_t hall_id) const {
        assert(hall_id < 7);
        return halls[hall_id];
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

        size_t dist = dist_map_rh[room_id][hall_id] + slot;
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

        size_t dist = dist_map_rh[room_id][hall_id] + slot;
        energy_used += dist * energy_per_step(who);

        state.set_room(room_id, slot, who);
        state.set_hall(hall_id, amphipod::none);
    }

    template <typename Seq>
    void get_adjacent_states(Seq& buf) {
        for (size_t room = 0; room < 4; ++room) {
            for (size_t hall = 0; hall < 7; ++hall) {
                if (can_room_hall(room, hall)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_room_hall(room, hall);
                }
                if (can_hall_room(hall, room)) {
                    world_explorer& ws = buf.emplace_back(*this);
                    ws.move_occupant_hall_room(hall, room);
                }
            }
        }
    }

    [[nodiscard]] bool is_goal() const {
        for (size_t i = 0; i < 4; ++i)
            if (!is_room_full(i) || !all_occupants_are(i, amphipod_for_room(i)))
                return false;
        return true;
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

    [[nodiscard]] uint8_t next_room_occupant_slot(size_t room_id) const {
        for (uint8_t slot = 0; slot < State::slot_count; ++slot) {
            if (state.get_room(room_id, slot) != amphipod::none) {
                return slot;
            }
        }
        throw std::runtime_error("no occupant in room");
    }

    [[nodiscard]] bool is_bottommost_resident(size_t room_id) const {
        amphipod want = amphipod_for_room(room_id);
        uint8_t slot = next_room_occupant_slot(room_id);
        for (; slot < State::slot_count; ++slot) {
            if (state.get_room(room_id, slot) != want) {
                return false;
            }
        }
        return true;
    }

    [[nodiscard]] uint8_t next_room_empty_slot(size_t room_id) const {
        // very important that we take the LAST empty slot first!
        // took a lot of time to debug this...
        for (uint8_t slot = State::slot_count - 1; slot < State::slot_count; --slot) {
            if (state.get_room(room_id, slot) == amphipod::none) {
                return slot;
            }
        }
        throw std::runtime_error("room full");
    }

    [[nodiscard]] bool all_occupants_are(size_t room_id, amphipod occ) const {
        for (size_t i = 0; i < State::slot_count; ++i) {
            amphipod a = state.get_room(room_id, i);
            if (a != amphipod::none && occ != a) {
                return false;
            }
        }
        return true;
    }

    [[nodiscard]] bool is_room_empty(size_t room_id) const {
        return state.get_room(room_id, State::slot_count - 1) == amphipod::none;
    }

    [[nodiscard]] bool is_room_full(size_t room_id) const {
        return state.get_room(room_id, 0) != amphipod::none;
    }

    //=========================================================================
    // adjacent state queries
    //=========================================================================

    [[nodiscard]] bool can_room_hall(size_t room_id, size_t hall_id) const {
        if (is_room_empty(room_id)) {
            return false;
        }

        if (is_bottommost_resident(room_id)) {
            return false;
        }

        // check for hallway blockage
        if (is_hall_left_of_room(room_id, hall_id)) {
            size_t start = hall_id;
            size_t end = hall_id_left_of_room(room_id);
            for (size_t i = start; i <= end; ++i) {
                if (!is_hallway_vacant(i))
                    return false;
            }
        } else {
            size_t start = hall_id;
            size_t end = hall_id_right_of_room(room_id);
            for (size_t i = end; i <= start; ++i) {
                if (!is_hallway_vacant(i))
                    return false;
            }
        }

        return true;
    }

    [[nodiscard]] bool can_hall_room(size_t hall_id, size_t room_id) const {
        if (is_hallway_vacant(hall_id))
            return false;
        if (!is_room_destination_for(room_id, state.get_hall(hall_id)))
            return false;

        if (is_room_full(room_id)) {
            return false;
        } else if (!all_occupants_are(room_id, state.get_hall(hall_id))) {
            return false;
        }

        // check for hallway blockage
        if (is_hall_left_of_room(room_id, hall_id)) {
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
};

template <typename State>
struct std::hash<world_explorer<State>> {
    size_t operator()(const world_explorer<State>& ws) const {
        return XXH3_64bits(&ws.state, sizeof(ws.state));
    }
};

template <typename State>
size_t solve(world_explorer<State> ws0, std::pmr::monotonic_buffer_resource* mr) {
    std::pmr::unordered_map<world_explorer<State>, size_t> seen(mr);
    seen.reserve(150000);
    std::pmr::vector<world_explorer<State>> worlds(mr);
    worlds.reserve(50000);
    worlds.emplace_back(std::move(ws0));
    size_t best_score = -1;

    constexpr auto energy_heap = [](const world_explorer<State>& w0, const world_explorer<State>& w1) {
        return w0.energy_used > w1.energy_used;
    };

    while (worlds.size()) {
        world_explorer<State> ws = worlds.front();
        std::ranges::pop_heap(worlds, energy_heap);
        worlds.pop_back();

        if (auto it = seen.find(ws); it != seen.end()) {
            if (ws.energy_used < it->second) {
                it->second = ws.energy_used;
            } else {
                // prune this world
                continue;
            }
        } else {
            seen.emplace(std::make_pair(ws, ws.energy_used));
        }

        if (ws.energy_used <= best_score) {
            if (ws.is_goal()) {
                best_score = ws.energy_used;
                continue;
            } else {
                size_t heap_size = worlds.size() + 1;
                ws.get_adjacent_states(worlds);
                for (; heap_size <= worlds.size(); ++heap_size) {
                    std::push_heap(worlds.begin(), worlds.begin() + heap_size, energy_heap);
                }
            }
        }
    }

    return best_score;
}

int main(int argc, char* argv[]) {
    std::pmr::monotonic_buffer_resource arena;
    auto args = sr::parse_command_line(argc, argv);

    // example input
    //
    // #############
    // #...........#
    // ###B#C#B#D###
    //   #A#D#C#A#
    //   #########

    std::pmr::string amphipods(&arena);
    amphipods.reserve(16);
    std::pmr::string buf(&arena);
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
    sr::solution(solve(std::move(ws1), &arena));

    // part 2 inserts the following rows between the existing ones:
    //
    //   #D#C#B#A#
    //   #D#B#A#C#

    amphipods.insert(4, "DCBADBAC");

    world_explorer<world_state<4>> ws2;
    for (size_t i = 0; i < 4; ++i) {
        for (size_t j = 0; j < 4; ++j) {
            ws2.state.set_room(j, i, to_amphipod(amphipods[i * 4 + j]));
        }
    }
    sr::solution(solve(std::move(ws2), &arena));

    return 0;
}
