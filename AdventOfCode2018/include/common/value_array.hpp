#ifndef COMMON_VALUE_ARRAY_HPP
#define COMMON_VALUE_ARRAY_HPP

#include <array>

template <typename T, std::size_t N>
struct value_array {
    static constexpr std::size_t SIZE = N;
    std::array<T, SIZE> values;

    // initialized
    explicit value_array(const T& val = T()) {
        values.fill(val);
    }

    // indexing
    T& operator[](std::size_t index) {
        return values[index];
    }
    const T& operator[](std::size_t index) const {
        return values[index];
    }

    // comparison
    bool operator==(const value_array& rhs) const {
        return std::memcmp(values.data(), rhs.values.data(), sizeof(T) * SIZE) == 0;
    }
    bool operator!=(const value_array& rhs) const {
        return !(*this == rhs);
    }

    // assignment
    value_array& operator=(const value_array& rhs) {
        std::memcpy(values.data(), rhs.values.data(), sizeof(T) * SIZE);
        return *this;
    }
};

#endif
