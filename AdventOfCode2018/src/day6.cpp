#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>

struct point {
    int x, y;
};
bool point_sort_x(const point& p0, const point& p1) {
    return p0.x < p1.x;
}
bool point_sort_y(const point& p0, const point& p1) {
    return p0.y < p1.y;
}
int manhattan(const point& p0, const point& p1) {
    return abs(p1.x - p0.x) + abs(p1.y - p0.y);
}

std::istream& operator>>(std::istream& input, point& p) {
    input >> p.x;
    input.ignore(1, ',');
    input >> p.y;
    return input;
}

struct grid {
    struct dpoint {
        int point_id;
        int distance;
    };

    std::vector<point> points;
    int b_top, b_right, b_bottom, b_left, b_width, b_height;

    void add_point(const point& p) {
        points.push_back(p);
    }

    void compute_bounds() {
        b_left = std::min_element(points.begin(), points.end(), point_sort_x)->x;
        b_right = std::max_element(points.begin(), points.end(), point_sort_x)->x;
        b_top = std::min_element(points.begin(), points.end(), point_sort_y)->y;
        b_bottom = std::max_element(points.begin(), points.end(), point_sort_y)->y;
        b_width = b_right - b_left;
        b_height = b_bottom - b_top;
    }

    int largest_finite_area() const {
        std::vector<dpoint> distance_map(points.size());
        std::vector<int> frequency(points.size());
        for (int x = 0; x < b_width; ++x) {
            for (int y = 0; y < b_height; ++y) {
                for (int z = 0; z < points.size(); ++z) {
                    distance_map[z] = {z, manhattan({b_left + x, b_top + y}, points[z])};
                }
                std::sort(distance_map.begin(), distance_map.end(), [](const auto& p0, const auto& p1) {
                    return p0.distance < p1.distance;
                });
                if (distance_map[0].distance < distance_map[1].distance) {
                    ++frequency[distance_map[0].point_id];
                }
            }
        }
        return *std::max_element(frequency.begin(), frequency.end());
    }

    int common_area() const {
        // TODO: tune this
        int search_dist = 0;
        int left = b_left - search_dist;
        int right = b_right + search_dist;
        int top = b_top - search_dist;
        int bottom = b_bottom + search_dist;
        int width = right - left;
        int height = bottom - top;
        int cells = 0;
        for (int x = 0; x <= width; ++x) {
            for (int y = 0; y <= height; ++y) {
                int sum = 0;
                for (int z = 0; z < points.size(); ++z) {
                    sum += manhattan({left + x, top + y}, points[z]);
                }
                if (sum < 10000) {
                    ++cells;
                }
            }
        }
        return cells;
    }
};

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;
    grid g;

    while (input) {
        point p;
        if (input >> p) {
            g.add_point(p);
        }
    }

    g.compute_bounds();
    std::cout << g.largest_finite_area() << std::endl;
    std::cout << g.common_area() << std::endl;

    return 0;
}
