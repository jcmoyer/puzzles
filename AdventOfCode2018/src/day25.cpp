#include <algorithm>
#include <fstream>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_set>
#include <vector>

#include "common/array2d.hpp"
#include "common/point.hpp"

using star_point = point4<int>;

struct constellation {
    std::vector<star_point> points;

    constellation(const star_point& init_point) {
        points.push_back(init_point);
    }

    bool in_range(const star_point& p) const {
        return std::any_of(points.begin(), points.end(), [&](const star_point& x) {
            return manhattan(p, x) <= 3;
        });
    }
};

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);

    std::deque<star_point> points;
    star_point p;
    while (input) {
        input >> p.x;
        input.ignore(1, ',');
        input >> p.y;
        input.ignore(1, ',');
        input >> p.z;
        input.ignore(1, ',');
        input >> p.w;
        points.push_back(p);
    }

    std::vector<constellation> constellations;
    constellations.emplace_back(points.front());
    points.pop_front();

    int last_constellations_grown = 0;
    int constellations_grown = 0;
    for (;;) {

        last_constellations_grown = constellations_grown;
        constellations_grown = 0;

        for (int i = 0; i < points.size();) {
            star_point p = points[i];

            bool added_to_one = false;
            for (constellation& c : constellations) {
                if (c.in_range(p)) {
                    c.points.push_back(p);
                    added_to_one = true;
                    ++constellations_grown;
                }
            }

            // add to back of queue to process later once more constellations are found
            if (!added_to_one) {
                ++i;
            } else {
                points.erase(points.begin() + i);
            }
        }

        if (points.size() == 0) {
            break;
        } else if (constellations_grown == 0) {
            constellations.emplace_back(points.front());
            points.erase(points.begin());
        }
    }

    // 581 high
    // 388 high
    // 367
    std::cout << constellations.size() << std::endl;

    return 0;
}
