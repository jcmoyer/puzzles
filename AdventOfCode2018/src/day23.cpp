#include <algorithm>
#include <charconv>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>
#include <vector>

#include <sr/sr.hpp>

using vec3i64 = sr::vec3<int64_t>;

struct nanobot {
    vec3i64 pos;
    int64_t radius;

    std::vector<size_t> in_range;

    bool is_in_radius(const vec3i64& p, int64_t* distout) {
        int64_t dist = sr::manhattan(pos, p);
        if (distout)
            *distout = dist;
        return dist <= radius;
    }
};

std::vector<nanobot> load_nanobots(std::istream& input) {
    std::regex nanobot_regex(R"(pos=<(\-?\d+),(\-?\d+),(\-?\d+)>, r=(\d+))");

    std::vector<nanobot> nanobots;

    std::string line;
    while (std::getline(input, line)) {
        std::cmatch match;
        if (std::regex_match(line.data(), match, nanobot_regex)) {
            nanobot bot;
            std::from_chars(match[1].first, match[1].second, bot.pos.x());
            std::from_chars(match[2].first, match[2].second, bot.pos.y());
            std::from_chars(match[3].first, match[3].second, bot.pos.z());
            std::from_chars(match[4].first, match[4].second, bot.radius);
            nanobots.push_back(bot);
        }
    }

    return nanobots;
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    auto n = load_nanobots(input);

    const nanobot& strongest = *std::max_element(n.begin(), n.end(), [](const auto& a, const auto& b) {
        return a.radius < b.radius;
    });

    int64_t sum = 0;

    int64_t min_x = std::numeric_limits<int64_t>::max();
    int64_t min_y = std::numeric_limits<int64_t>::max();
    int64_t min_z = std::numeric_limits<int64_t>::max();
    int64_t max_x = std::numeric_limits<int64_t>::min();
    int64_t max_y = std::numeric_limits<int64_t>::min();
    int64_t max_z = std::numeric_limits<int64_t>::min();
    int64_t total_x = 0;
    int64_t total_y = 0;
    int64_t total_z = 0;

    for (auto& b : n) {
        if (manhattan(strongest.pos, b.pos) <= strongest.radius)
            ++sum;

        min_x = std::min(min_x, b.pos.x());
        max_x = std::max(max_x, b.pos.x());
        min_y = std::min(min_y, b.pos.y());
        max_y = std::max(max_y, b.pos.y());
        min_z = std::min(min_z, b.pos.z());
        max_z = std::max(max_z, b.pos.z());
        total_x += b.pos.x();
        total_y += b.pos.y();
        total_z += b.pos.z();

        for (size_t i = 0; i < n.size(); ++i) {
            if (manhattan(b.pos, n[i].pos) <= b.radius) {
                b.in_range.push_back(i);
            }
        }
    }
    std::cout << sum << std::endl;

    int64_t avg_x = total_x / n.size();
    int64_t avg_y = total_y / n.size();
    int64_t avg_z = total_z / n.size();

    int64_t w = max_x - min_x;
    int64_t h = max_y - min_y;
    int64_t d = max_z - min_z;

    std::sort(n.begin(), n.end(), [](auto& a, auto& b) {
        return a.in_range.size() > b.in_range.size();
    });

    int64_t max_close = std::numeric_limits<int64_t>::min();
    int64_t best_dist = std::numeric_limits<int64_t>::max();

    int64_t xstart = -1069;
    for (int64_t xo = xstart; xo <= 1500; xo++) {
        std::cout << xo << std::endl;
        for (int64_t yo = -1500; yo <= 1500; yo++) {
            for (int64_t zo = -1500; zo <= 1500; zo++) {
                int64_t close_here = 0;
                int64_t dist_here;
                for (auto& b : n) {
                    if (b.is_in_radius(vec3i64{xo, yo, zo}, &dist_here)) {
                        ++close_here;
                    }
                }
                if (close_here > max_close) {
                    max_close = close_here;
                    best_dist = sr::manhattan(vec3i64{xo, yo, zo}, vec3i64{0, 0, 0});

                    std::cout << max_close << ": " << best_dist << std::endl;

                } else if (close_here == max_close) {
                    // distance to origin breaks tie
                    if (sr::manhattan(vec3i64{xo, yo, zo}, vec3i64{0, 0, 0}) < best_dist) {
                        max_close = close_here;
                        best_dist = sr::manhattan(vec3i64{xo, yo, zo}, vec3i64{0, 0, 0});

                        std::cout << max_close << ": " << best_dist << std::endl;
                    }
                }
            }
        }
    }

    std::cout << best_dist << std::endl;
    std::cin.get();

    return 0;
}
