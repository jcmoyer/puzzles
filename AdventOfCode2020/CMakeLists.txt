cmake_minimum_required(VERSION 3.15)
project(aoc2020)

set(PYTHON_BIN python CACHE STRING "name of python binary to use for the test runner")

find_package(sr CONFIG REQUIRED)
find_package(fmt CONFIG REQUIRED)

enable_testing()

include_directories("include")
add_subdirectory("src")
