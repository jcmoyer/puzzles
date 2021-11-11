#ifndef COMMON_ARRAY2D_HPP
#define COMMON_ARRAY2D_HPP

#include <vector>

#include "common/point.hpp"

template <typename T>
class array2d {
public:
    using value_type = T;
    using size_type = typename std::vector<T>::size_type;
    using difference_type = typename std::vector<T>::difference_type;
    using reference = value_type&;
    using const_reference = const value_type&;

    array2d(size_type width, size_type height, const T& val = T())
        : _width{width}, _height{height}, _data(width * height, val) {}

    array2d(const array2d& x) : _width{x._width}, _height{x._height}, _data(x._data) {}

    array2d(array2d&& x) : _width{x._width}, _height{x._height}, _data{std::move(x._data)} {}

    array2d& operator=(const array2d& x) {
        _width = x._width;
        _height = x._height;
        _data = x._data;
        return *this;
    }

    array2d& operator=(array2d&& x) {
        _width = x._width;
        _height = x._height;
        _data = std::move(x._data);
        return *this;
    }

    reference at(size_type x, size_type y) {
        return _data.at(y * _width + x);
    }

    reference operator[](size_type i) {
        return _data[i];
    }

    const_reference operator[](size_type i) const {
        return _data[i];
    }

    reference operator[](const point& p) {
        return _data[p.y * _width + p.x];
    }

    const_reference operator[](const point& p) const {
        return _data[p.y * _width + p.x];
    }

    T* data() {
        return _data.data();
    }
    const T* data() const {
        return _data.data();
    }

    size_type width() const {
        return _width;
    }
    size_type height() const {
        return _height;
    }
    size_type size() const {
        return _width * _height;
    }

    auto begin() {
        return _data.begin();
    }
    auto begin() const {
        return _data.begin();
    }
    auto end() {
        return _data.end();
    }
    auto end() const {
        return _data.end();
    }

    bool operator==(const array2d& rhs) const {
        return _width == rhs._width && _height == rhs._height && _data == rhs._data;
    }

private:
    std::vector<T> _data;
    size_type _width, _height;
};

#endif
