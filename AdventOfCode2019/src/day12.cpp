#include <algorithm>
#include <fmt/format.h>
#include <fstream>
#include <iterator>
#include <numeric>

#include <sr/sr.hpp>

struct vec3 {
    int32_t vals[3];

    int32_t& operator[](size_t index) {
        return vals[index];
    }

    const int32_t& operator[](size_t index) const {
        return vals[index];
    }
};

bool operator==(const vec3& lhs, const vec3& rhs) {
    return lhs[0] == rhs[0] && lhs[1] == rhs[1] && lhs[2] == rhs[2];
}

vec3 operator+(const vec3& lhs, const vec3& rhs) {
    return {lhs[0] + rhs[0], lhs[1] + rhs[1], lhs[2] + rhs[2]};
}

// 1D gravity, p0 and p1 are positions, v0 and v1 are their corresponding velocities
template <typename Integer>
void gravity(const Integer& p0, const Integer& p1, Integer& v0, Integer& v1) {
    if (p0 > p1) {
        --v0;
        ++v1;
    } else if (p1 > p0) {
        --v1;
        ++v0;
    }
}

template <typename Iterator, typename F>
void for_pairs(Iterator first, Iterator last, F callback) {
    for (; first != last; ++first) {
        for (Iterator pairing = first + 1; pairing != last; ++pairing) {
            callback(*first, *pairing);
        }
    }
}

struct moon {
    vec3 pos{};
    vec3 vel{};

    moon(vec3 p) : pos{p} {}

    void gravity(moon& other, int axis) {
        ::gravity(pos[axis], other.pos[axis], vel[axis], other.vel[axis]);
    }

    void gravity(moon& other) {
        for (int axis = 0; axis < 3; ++axis) {
            gravity(other, axis);
        }
    }

    void update(int axis) {
        pos[axis] += vel[axis];
    }

    void update() {
        pos = pos + vel;
    }

    int64_t energy() const {
        return (std::abs(pos[0]) + std::abs(pos[1]) + std::abs(pos[2])) *
               (std::abs(vel[0]) + std::abs(vel[1]) + std::abs(vel[2]));
    }

    bool equal_pv(const moon& rhs, int axis) const {
        return pos[axis] == rhs.pos[axis] && vel[axis] == rhs.vel[axis];
    }

    bool operator==(const moon& rhs) const {
        return pos == rhs.pos && vel == rhs.vel;
    }
};

int64_t simulate(std::vector<moon> moons, int64_t iterations) {
    for (int64_t i = 0; i < iterations; ++i) {
        for_pairs(moons.begin(), moons.end(), [](auto& left, auto& right) {
            left.gravity(right);
        });

        std::for_each(moons.begin(), moons.end(), [](moon& m) {
            m.update();
        });
    }

    int64_t en = 0;
    for (size_t i = 0; i < moons.size(); ++i) {
        en += moons[i].energy();
    }
    return en;
}

int64_t iterations_to_loop(const std::vector<moon>& initial_state, std::vector<moon>& moons, int axis) {
    int64_t iterations = 0;
    for (;;) {
        for_pairs(moons.begin(), moons.end(), [axis](auto& left, auto& right) {
            left.gravity(right, axis);
        });

        std::for_each(moons.begin(), moons.end(), [axis](moon& m) {
            m.update(axis);
        });

        ++iterations;

        bool done = std::equal(moons.begin(),
            moons.end(),
            initial_state.begin(),
            initial_state.end(),
            [axis](const moon& m0, const moon& m1) {
                return m0.equal_pv(m1, axis);
            });

        if (done) {
            return iterations;
        }
    }
}

int64_t solve_sim_loop(std::vector<moon> moons) {
    std::vector<int64_t> periods(3);
    std::vector<moon> initial_state = moons;

    for (int axis = 0; axis < 3; ++axis) {
        periods[axis] = iterations_to_loop(initial_state, moons, axis);
    }

    return std::accumulate(periods.begin(), periods.end(), periods.front(), std::lcm<int64_t, int64_t>);
}

template <typename Stream>
std::vector<moon> load_moons(Stream&& stream) {
    std::vector<moon> moons;
    for (const std::string& line : sr::lines(stream)) {
        vec3 pos;
        sr::parse(R"(<x=(\-?\d+), y=(\-?\d+), z=(\-?\d+)>)", line, pos[0], pos[1], pos[2]);
        moons.emplace_back(pos);
    }
    return moons;
}

int main(int argc, char* argv[]) {
    const char* filename = argv[1];
    auto moons = load_moons(std::ifstream(filename));

    // part 1
    fmt::print("{}\n", simulate(moons, 1000));

    // part 2
    fmt::print("{}\n", solve_sim_loop(moons));

    return 0;
}
