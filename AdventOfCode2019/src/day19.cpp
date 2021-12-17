#include <atomic>
#include <fmt/format.h>
#include <fstream>
#include <iostream>
#include <numeric>
#include <thread>

#include <sr/sr.hpp>

#include "aoc/intcode.hpp"

double dist(const sr::vec2i& p0, const sr::vec2i& p1) {
    const int dx = p1.x() - p0.x();
    const int dy = p1.y() - p0.y();
    return std::sqrt(dx * dx + dy * dy);
}

bool search_xy(const intcode_program& prog, int64_t x, int64_t y, int64_t w, int64_t h) {
    int64_t last_val = 0;
    int64_t sum = 0;
    intcode_vm vm;
    vm.set_output_handler([&](int64_t val) {
        last_val = val;
    });

    for (int64_t y2 = y; y2 < y + h; ++y2) {
        for (int64_t x2 = x; x2 < x + w; ++x2) {
            vm.set_program(prog);
            vm.push_input(x2);
            vm.push_input(y2);
            vm.run();
            if (last_val == 0) {
                return false;
            }
        }
    }

    return true;
}

int64_t part1(const intcode_program& prog) {
    intcode_vm vm;
    int64_t sum = 0;
    vm.set_output_handler([&](int64_t val) {
        sum += val;
    });
    for (int y = 0; y < 50; ++y) {
        for (int x = 0; x < 50; ++x) {
            vm.set_program(prog);
            vm.push_input(x);
            vm.push_input(y);
            vm.run();
        }
    }
    return sum;
}

int64_t part2(const intcode_program& prog) {
    const unsigned int N_THREADS = std::thread::hardware_concurrency();

    std::atomic<int64_t> shared_y = 960;
    std::vector<std::thread> threads;
    std::vector<sr::vec2i> results(N_THREADS);

    for (unsigned int i = 0; i < N_THREADS; ++i) {
        threads.emplace_back([&, tid = i]() {
            intcode_vm vm;

            int64_t last_val = 0;
            vm.set_output_handler([&](int64_t val) {
                last_val = val;
            });

            int64_t last_start_x = 0;

            for (;;) {
                int64_t y = shared_y.fetch_add(1);

                fmt::print("y={}\n", y);

                int64_t start_x = -1;
                int64_t end_x = -1;

                // horizontal scan for start and end
                for (int64_t x = last_start_x;; ++x) {
                    vm.set_program(prog);
                    vm.push_input(x);
                    vm.push_input(y);
                    vm.run();

                    if (last_val == 1 && start_x == -1) {
                        start_x = x;
                        last_start_x = x;
                    }
                    if (last_val == 0 && end_x == -1 && start_x >= 0) {
                        end_x = x;
                        break;
                    }

                    // broken beam, near start of input
                    if (start_x == -1 && end_x == -1 && x >= 10000) {
                        break;
                    }
                }

                // only check if there are >=100 cells horizontally in this scanline
                for (int64_t x2 = start_x; x2 <= end_x - 100; ++x2) {
                    if (search_xy(prog, x2, y, 100, 100)) {
                        results[tid] = sr::vec2i{(int)x2, (int)y};
                        return;
                    }
                }
            }
        });
    }

    for (auto& t : threads) {
        t.join();
    }

    auto nearest = std::min_element(results.begin(), results.end(), [](const sr::vec2i& x, const sr::vec2i& y) {
        constexpr auto origin = sr::vec2i{};
        return dist(x, origin) < dist(y, origin);
    });

    return nearest->x() * 10000 + nearest->y();
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    const auto& prog = load_program(input);

    fmt::print("{}\n", part1(prog));
    fmt::print("{}\n", part2(prog));

    return 0;
}
