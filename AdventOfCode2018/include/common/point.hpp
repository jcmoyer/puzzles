#ifndef COMMON_POINT_HPP
#define COMMON_POINT_HPP

#include <cmath>
#include <ostream>
#include <tuple>

struct point {
    int x, y;
};

bool operator==(const point& a, const point& b) {
    return std::tie(a.y, a.x) == std::tie(b.y, b.x);
}

bool operator!=(const point& a, const point& b) {
    return !(a == b);
}

bool operator<(const point& a, const point& b) {
    return std::tie(a.y, a.x) < std::tie(b.y, b.x);
}

bool operator>(const point& a, const point& b) {
    return std::tie(a.y, a.x) > std::tie(b.y, b.x);
}

point operator+(const point& a, const point& b) {
    return {a.x + b.x, a.y + b.y};
}

point operator-(const point& a, const point& b) {
    return {a.x - b.x, a.y - b.y};
}

point& operator+=(point& a, const point& b) {
    a.x += b.x;
    a.y += b.y;
    return a;
}

std::ostream& operator<<(std::ostream& s, const point& a) {
    return s << a.x << ',' << a.y;
}

int manhattan(const point& p0, const point& p1) {
    return std::abs(p1.x - p0.x) + std::abs(p1.y - p0.y);
}

namespace std {
template <>
struct hash<point> {
    std::size_t operator()(const point& p) const {
        if constexpr (sizeof(p) == sizeof(std::size_t)) {
            std::size_t h = p.x;
            h <<= 32;
            return h | p.y;
        } else {
            throw "not impl";
        }
    }
};
}

template <typename T>
struct point3 {
    T x, y, z;
};

template <typename T>
T manhattan(const point3<T>& p0, const point3<T>& p1) {
    return std::abs(p1.x - p0.x) + std::abs(p1.y - p0.y) + std::abs(p1.z - p0.z);
}

template <typename T>
struct point4 {
    T x, y, z, w;
};

template <typename T>
T manhattan(const point4<T>& p0, const point4<T>& p1) {
    return std::abs(p1.x - p0.x) + std::abs(p1.y - p0.y) + std::abs(p1.z - p0.z) + std::abs(p1.w - p0.w);
}

namespace std {
template <typename T>
struct hash<point4<T>> {
    std::size_t operator()(const point4<T>& p) const {
        std::size_t h = p.x;
        h <<= 32;
        h |= p.y;
        std::size_t u = p.z;
        u <<= 32;
        u |= p.w;
        return h ^ u;
    }
};
}

template <typename T>
bool operator==(const point4<T>& p0, const point4<T>& p1) {
    return std::tie(p0.x, p0.y, p0.z, p0.w) == std::tie(p1.x, p1.y, p1.z, p1.w);
}

#endif
