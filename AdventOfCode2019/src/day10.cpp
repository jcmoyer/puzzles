#include <algorithm>
#include <fmt/format.h>
#include <fstream>
#include <unordered_map>
#include <unordered_set>

#include <sr/sr.hpp>

using tile = char;
using asteroid = sr::point;
using asteroid_field = std::unordered_set<asteroid>;

const double PI = 3.14159265359;

struct visresult {
    sr::point pos;
    size_t visible;
};

double dist(const sr::point& p0, const sr::point& p1) {
    const int dx = p1.x - p0.x;
    const int dy = p1.y - p0.y;
    return std::sqrt(dx * dx + dy * dy);
}

size_t fuzzy_find_index_of(const std::vector<double>& numbers, double target) {
    auto it = std::find_if(numbers.begin(), numbers.end(), [target](double val) {
        return std::abs(val - target) < 0.001;
    });
    return std::distance(numbers.begin(), it);
}

size_t num_visible_from(const asteroid_field& field, const asteroid& view) {
    // Yes, I know this is an awful idea.
    // Yes, it produces the correct result. Sometimes.
    std::unordered_set<double> seen_angles;
    for (const auto& asteroid : field) {
        double a = std::atan2(view.y - asteroid.y, view.x - asteroid.x);
        seen_angles.insert(a);
    }
    return seen_angles.size();
}

int destroy_asteroids(const asteroid_field& field, const asteroid& view) {
    std::unordered_map<double, std::vector<sr::point>> asteroids;
    std::unordered_set<double> seen_angles;

    // here we do the same thing as part 1 except we also note the angle to every single asteroid
    for (const auto& asteroid : field) {
        double a = std::atan2(view.y - asteroid.y, view.x - asteroid.x);
        seen_angles.insert(a);
        asteroids[a].push_back(asteroid);
    }

    // then we take every angle we observed to produce an array of only correct angles...
    std::vector<double> ordered_angles(seen_angles.begin(), seen_angles.end());
    // ...and sort them so that iterating through the array advances the laser to the next valid angle automatically
    std::sort(ordered_angles.begin(), ordered_angles.end());

    // sort asteroids by distance to observation point so that we can easily destroy the closest asteroid at a given
    // angle
    for (auto& [k, v] : asteroids) {
        std::sort(v.begin(), v.end(), [&](const auto& p0, const auto& p1) {
            return dist(view, p0) > dist(view, p1);
        });
    }

    // find the angle that points up and start there
    // it's theoretically possible there isn't an asteroid directly above the observation point so this will fail...
    size_t i = fuzzy_find_index_of(ordered_angles, PI / 2.0);

    // then we simply walk through the ordered angles and destroy asteroids we find once per angle, returning to the
    // start of the array when we reach the end of it - this is conceptually similar to how you would move around a
    // unit circle with cos/sin and an ever decreasing angle
    int destroyed = 0;
    sr::point last_destroyed;
    while (destroyed != 200) {
        const double a = ordered_angles[i];
        if (auto it = asteroids.find(a); it != asteroids.end() && it->second.size()) {
            last_destroyed = it->second.back();
            it->second.pop_back();
            ++destroyed;
        } else {
            throw std::runtime_error("found angle not in asteroids");
        }
        i = (i + 1) % ordered_angles.size();
    }

    return last_destroyed.x * 100 + last_destroyed.y;
}

std::vector<visresult> compute_visibility(const asteroid_field& field) {
    std::vector<visresult> vis;
    for (const auto& asteroid : field) {
        vis.emplace_back(visresult{asteroid, num_visible_from(field, asteroid)});
    }
    return vis;
}

template <typename Stream>
asteroid_field load_asteroid_field(Stream&& stream) {
    asteroid_field field;
    int y = 0;
    for (const auto& line : sr::lines(stream)) {
        for (int x = 0; x < line.size(); ++x) {
            if (line[x] == '#') {
                field.emplace(asteroid{x, y});
            }
        }
        ++y;
    }
    return field;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    const asteroid_field& field = load_asteroid_field(std::ifstream(filename));

    // part 1
    auto vis = compute_visibility(field);
    auto it = std::max_element(vis.begin(), vis.end(), [](const auto& st1, const auto& st2) {
        return st1.visible < st2.visible;
    });
    fmt::print("{}\n", it->visible);

    // part 2
    int val = destroy_asteroids(field, it->pos);
    fmt::print("{}\n", val);

    return 0;
}
