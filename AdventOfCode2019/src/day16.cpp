#include <array>
#include <cstdint>
#include <fmt/format.h>
#include <fstream>
#include <thread>
#include <vector>

using signal_type = std::vector<int>;

inline constexpr int fast_pattern(int scale, size_t n) {
    constexpr std::array<int, 4> basepat{0, 1, 0, -1};
    return basepat[((n + 1) / scale) % 4];
}

auto phase(signal_type signal) {
    signal_type new_signal(signal.size());
    for (int i = 0; i < signal.size(); ++i) {
        int sum = 0;
        int scale = i + 1;
        for (int j = 0; j < signal.size(); ++j) {
            int mul = fast_pattern(scale, j);
            sum += signal[j] * mul;
        }
        new_signal[i] = std::abs(sum) % 10;
    }
    return new_signal;
}

// Optimized phase function that encodes the pattern directly into the algorithm
// - Skips 0*N operations in the pattern, since they do nothing
// - Only does one addition operation per signal element
signal_type phase_v2(signal_type signal) {
    signal_type new_signal(signal.size());

    std::vector<std::thread> threads;

    const int THREADS = std::thread::hardware_concurrency();

    for (int tid = 0; tid < THREADS; ++tid) {
        size_t first = tid * (signal.size() / THREADS);
        size_t last = first + (signal.size()) / THREADS - 1;
        if (tid == THREADS - 1)
            last = signal.size() - 1;
        threads.emplace_back([&signal, &new_signal, first, last, tid]() {
            for (size_t i = first; i <= last; ++i) {
                int sum = 0;
                int scale = static_cast<int>(i) + 1;

                const int* signal_ptr = signal.data() + scale - 1;
                const int* signal_end = signal.data() + signal.size();

                while (signal_ptr < signal_end) {
                    const int* end;
                    end = std::min(signal_end, signal_ptr + scale);
                    for (; signal_ptr < end; ++signal_ptr) {
                        sum += *signal_ptr;
                    }
                    signal_ptr += scale;
                    end = std::min(signal_end, signal_ptr + scale);
                    for (; signal_ptr < end; ++signal_ptr) {
                        sum -= *signal_ptr;
                    }
                    signal_ptr += scale;
                }

                new_signal[i] = std::abs(sum) % 10;
            }
        });
    }
    for (auto& t : threads)
        t.join();
    return new_signal;
}

signal_type phase_right_half(signal_type signal) {
    signal_type new_signal(signal.size());
    std::vector<std::thread> threads;
    const int THREADS = std::thread::hardware_concurrency();
    for (int tid = 0; tid < THREADS; ++tid) {
        size_t first = tid * (signal.size() / THREADS);
        size_t last = first + (signal.size() / THREADS) - 1;
        if (tid == THREADS - 1)
            last = signal.size() - 1;
        threads.emplace_back([&, signal, first, last, tid]() {
            for (size_t i = first; i <= last; ++i) {
                int sum = 0;
                for (size_t j = i; j < signal.size(); ++j) {
                    sum += signal[j];
                }
                new_signal[i] = std::abs(sum) % 10;
            }
        });
    }
    for (auto& t : threads)
        t.join();
    return new_signal;
}

size_t read_embedded_offset(signal_type signal) {
    std::string s;
    for (int i = 0; i < 7; ++i) {
        s.push_back('0' + signal[i]);
    }
    return std::stoull(s);
}

void print_signal_offset(signal_type signal, size_t offset) {
    for (size_t i = offset; i < offset + 8; ++i) {
        fmt::print("{}", signal[i]);
    }
}

signal_type string_to_signal(const std::string& s) {
    signal_type signal;
    for (char ch : s) {
        signal.push_back(ch - '0');
    }
    return signal;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    std::ifstream input(filename);
    std::string line;
    std::getline(input, line);

    // part 1
    auto sig1 = string_to_signal(line);
    for (int i = 0; i < 100; ++i) {
        sig1 = phase_v2(std::move(sig1));
    }
    print_signal_offset(sig1, 0);

    // part 2
    std::string input2;
    input2.reserve(10000 * line.size());
    for (int i = 0; i < 10000; ++i) {
        input2.append(line);
    }
    signal_type sig2 = string_to_signal(input2);
    size_t offs = read_embedded_offset(sig2);
    sig2 = std::vector(sig2.begin() + offs, sig2.end());
    for (int i = 0; i < 100; ++i) {
        sig2 = phase_right_half(std::move(sig2));
    }
    print_signal_offset(sig2, 0);

    return 0;
}
