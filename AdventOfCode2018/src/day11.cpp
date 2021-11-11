// g++ -std=c++17 -O3 -march=native this.cpp
// https://en.wikipedia.org/wiki/Summed-area_table

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <thread>
#include <tuple>
#include <vector>

constexpr int INPUT = 4172;

struct square_find_results {
    int x, y;
    int power;
    int size;
};
bool operator<(const square_find_results& x, const square_find_results& y) {
    return x.power < y.power;
}

struct threading_plan {
    struct irange {
        int mini, maxi;
    };
    std::vector<irange> ranges;
    int min_size;
    int max_size;
};

class fuel_matrix {
public:
    fuel_matrix(int w, int h, int serial) : _cellsums((1 + w) * (1 + h)), _width{w}, _height{h}, _serial{serial} {
        generate_sums();
    }

    const threading_plan get_plan(int max_size) {
        int64_t total_checks = 0;
        for (int i = 1; i <= max_size; ++i) {
            total_checks += get_check_count(i);
        }
        unsigned int n_threads = std::thread::hardware_concurrency();
        int64_t checks_per_thread = total_checks / n_threads;
        threading_plan plan;
        plan.min_size = 1;
        plan.max_size = max_size;
        int next = plan.min_size;
        int64_t remaining = checks_per_thread;
        int size = plan.min_size;
        for (;;) {
            threading_plan::irange r{next, next};
            for (;;) {
                remaining -= get_check_count(size);
                ++size;
                ++r.maxi;
                if (remaining <= 0) {
                    plan.ranges.push_back(r);
                    next = r.maxi + 1;
                    remaining = checks_per_thread;
                    break;
                }
            }
            // TODO: this probably breaks on systems with one core.
            if (plan.ranges.size() == n_threads - 1)
                break;
        }
        plan.ranges.push_back({next, max_size});
        return plan;
    }

    square_find_results find_square(int size) const {
        square_find_results results;
        int max_p = std::numeric_limits<int>::min();
        int max_x = 0;
        int max_y = 0;
        for (int y = 0; y <= _height - size; ++y) {
            for (int x = 0; x <= _width - size; ++x) {
                int a_x = x;
                int a_y = y;

                int b_x = x + size;
                int b_y = y;

                int c_x = x;
                int c_y = y + size;

                int d_x = x + size;
                int d_y = y + size;

                int a = get_sum(a_x, a_y);
                int b = get_sum(b_x, b_y);
                int c = get_sum(c_x, c_y);
                int d = get_sum(d_x, d_y);

                int r = d + a - b - c;
                if (r > max_p) {
                    max_p = r;
                    max_x = x;
                    max_y = y;
                }
            }
        }
        results.x = 1 + max_x;
        results.y = 1 + max_y;
        results.power = max_p;
        results.size = size;
        return results;
    }

private:
    constexpr int64_t get_check_count(int size) const {
        return (1 + _width - size) * (1 + _height - size);
    }

    constexpr size_t sumindex(int x, int y) const {
        return (1 + y) * (1 + _width) + (1 + x);
    }

    int get_sum(int x, int y) const {
        return _cellsums[y * (1 + _width) + x];
    }

    constexpr int power(int x, int y) const {
        int rack = 10 + (1 + x);
        int power = rack * (1 + y);
        power += _serial;
        power *= rack;
        power = (power / 100) % 10;
        power -= 5;
        return power;
    }

    void generate_sums() {
        for (int y = 0; y < _height; ++y) {
            for (int x = 0; x < _width; ++x) {
                int xsum = _cellsums[sumindex(x - 1, y)];
                int ysum = _cellsums[sumindex(x, y - 1)];
                int xyd = _cellsums[sumindex(x - 1, y - 1)];
                _cellsums[sumindex(x, y)] = power(x, y) + ysum + xsum - xyd;
            }
        }
    }

    std::vector<int_fast32_t> _cellsums;
    int _width;
    int _height;
    int _serial;
};

int main(int argc, char* argv[]) {
    fuel_matrix m(300, 300, INPUT);

    // Part 1
    {
        auto results = m.find_square(3);
        std::cout << results.x << "," << results.y << std::endl;
    }

    // Part 2
    {
        auto p = m.get_plan(300);
        std::vector<square_find_results> results(p.max_size);
        std::vector<std::thread> threads;
        for (unsigned i = 0; i < std::thread::hardware_concurrency(); ++i) {
            threads.emplace_back([&results, &m, min = p.ranges[i].mini, max = p.ranges[i].maxi]() {
                for (int j = min; j <= max; ++j)
                    results[j - 1] = m.find_square(j);
            });
        }
        for (auto& t : threads)
            t.join();
        const auto& result = *std::max_element(results.begin(), results.end());
        std::cout << result.x << "," << result.y << "," << result.size << std::endl;
    }

    return 0;
}
