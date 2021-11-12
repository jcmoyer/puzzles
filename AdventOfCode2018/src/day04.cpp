#include <array>
#include <charconv>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <stdexcept>
#include <string>

struct datetime {
    int year, month, day; // warning: no validation!!
    int hour, minute, second;
};

bool operator<(const datetime& x, const datetime& y) {
    return std::tie(x.year, x.month, x.day, x.hour, x.minute, x.second) <
           std::tie(y.year, y.month, y.day, y.hour, y.minute, y.second);
}

struct guard_event {
    enum type { shift, wake, sleep };
    static constexpr int no_id = -1;
    datetime timestamp;
    type ty;
    int id;
};

class guard_state {
public:
    guard_state(int guard_id) : id{guard_id} {}

    void shift_start() {
        // guards start their shift awake
        status = awake;
    }

    void nap_start(datetime when) {
        if (status == asleep) {
            throw std::runtime_error("already asleep");
        }
        status = asleep;
        nap_start_time = when;
    }

    void nap_end(datetime when) {
        if (status == awake) {
            throw std::runtime_error("already awake");
        }
        status = awake;
        // according to spec, guards only sleep during hour 00 and
        // assuming chronologically sorted events the other fields
        // can be ignored
        total_minutes_asleep += when.minute - nap_start_time.minute;
        for (int i = nap_start_time.minute; i < when.minute; ++i) {
            ++sleep_timeline[i];
        }
    }

    int most_frequent_minute_asleep(int* frequency = nullptr) const {
        auto it = std::max_element(sleep_timeline.begin(), sleep_timeline.end());
        if (frequency) {
            *frequency = *it;
        }
        // must fall within the range 0..59 so this cast is safe
        return static_cast<int>(std::distance(sleep_timeline.begin(), it));
    }

private:
    enum sleepstate { awake, asleep };
    sleepstate status;
    datetime nap_start_time;

public:
    int total_minutes_asleep = 0;
    int id;

private:
    // index = minute; value = total times slept on that minute
    std::array<int, 60> sleep_timeline{0};
};

struct out_of_order : std::exception {};

// replays events to compute guard statistics
struct timesheet {
    std::map<int, guard_state> guards;
    int current_guard_id;

    void process_event(const guard_event& e) {
        switch (e.ty) {
        case guard_event::shift:
            process_shift_event(e);
            break;
        case guard_event::sleep:
            process_sleep_event(e);
            break;
        case guard_event::wake:
            process_wake_event(e);
            break;
        }
    }

    const guard_state& sleepiest_guard() const {
        auto it = std::max_element(guards.begin(), guards.end(), [](const auto& x, const auto& y) {
            return x.second.total_minutes_asleep < y.second.total_minutes_asleep;
        });
        if (it == guards.end()) {
            throw std::runtime_error("no guards present");
        }
        return it->second;
    }

private:
    void process_shift_event(const guard_event& e) {
        if (auto it = guards.find(e.id); it != guards.end()) {
            it->second.shift_start();
        } else {
            // first appearance of this guard; add an entry to the map
            guards.emplace(std::make_pair(e.id, guard_state{e.id}));
        }
        current_guard_id = e.id;
    }

    void process_sleep_event(const guard_event& e) {
        if (e.id != current_guard_id) {
            throw out_of_order{};
        }
        if (auto it = guards.find(e.id); it != guards.end()) {
            it->second.nap_start(e.timestamp);
        } else {
            throw out_of_order{};
        }
    }

    void process_wake_event(const guard_event& e) {
        if (e.id != current_guard_id) {
            throw out_of_order{};
        }
        if (auto it = guards.find(e.id); it != guards.end()) {
            it->second.nap_end(e.timestamp);
        } else {
            throw out_of_order{};
        }
    }
};

template <typename Match>
bool sv_regex_match(std::string_view sv, Match& m, const std::regex& r) {
    return std::regex_match(sv.data(), sv.data() + sv.size(), m, r);
}

std::vector<guard_event> load_events(std::istream& input) {
    static std::regex timestamp(R"(^\[(\d+)\-(\d+)\-(\d+) (\d+):(\d+)\] )");
    static std::regex shift_event(R"(Guard #(\d+) begins shift$)");
    static std::regex wake_event("wakes up$");
    static std::regex sleep_event("falls asleep$");

    std::vector<guard_event> events;
    std::string line;

    while (std::getline(input, line)) {
        std::cmatch m;
        if (std::regex_search(line.data(), m, timestamp)) {
            datetime ts;
            std::from_chars(m[1].first, m[1].second, ts.year);
            std::from_chars(m[2].first, m[2].second, ts.month);
            std::from_chars(m[3].first, m[3].second, ts.day);
            std::from_chars(m[4].first, m[4].second, ts.hour);
            std::from_chars(m[5].first, m[5].second, ts.minute);
            ts.second = 0;

            guard_event ev;
            ev.id = guard_event::no_id;
            ev.timestamp = ts;

            std::string_view event_string = line.data() + m.length();
            if (sv_regex_match(event_string, m, shift_event)) {
                ev.ty = guard_event::shift;
                std::from_chars(m[1].first, m[1].second, ev.id);
            } else if (sv_regex_match(event_string, m, wake_event)) {
                ev.ty = guard_event::wake;
            } else if (sv_regex_match(event_string, m, sleep_event)) {
                ev.ty = guard_event::sleep;
            } else {
                throw std::runtime_error("unhandled event");
            }
            events.push_back(ev);
        }
    }
    return events;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);

    std::vector<guard_event> events = load_events(input);

    // sort events chronologically
    std::sort(events.begin(), events.end(), [](const guard_event& x, const guard_event& y) {
        return x.timestamp < y.timestamp;
    });

    // associate guard IDs and process events
    timesheet ts;
    int last_guard_id = guard_event::no_id;
    for (auto& ev : events) {
        if (ev.ty == guard_event::shift) {
            last_guard_id = ev.id;
        } else {
            ev.id = last_guard_id;
        }
        ts.process_event(ev);
    }

    // part 1: guard who sleeps the most minutes
    const auto& sleepiest_guard = ts.sleepiest_guard();
    std::cout << sleepiest_guard.id * sleepiest_guard.most_frequent_minute_asleep() << std::endl;

    // part 2: guard who sleeps on a single minute more than any other guard
    // map guard IDs to their most frequently slept minute
    std::map<int, int> guards_to_frequent_minutes;
    for (const auto& [k, v] : ts.guards) {
        guards_to_frequent_minutes[k] = v.most_frequent_minute_asleep();
    }

    // find the guard with the highest frequency
    auto max_guard_it = std::max_element(
        guards_to_frequent_minutes.begin(), guards_to_frequent_minutes.end(), [&ts](const auto& x, const auto& y) {
            int freq1, freq2;
            ts.guards.at(x.first).most_frequent_minute_asleep(&freq1);
            ts.guards.at(y.first).most_frequent_minute_asleep(&freq2);
            return freq1 < freq2;
        });

    std::cout << max_guard_it->first * max_guard_it->second << std::endl;

    return 0;
}
