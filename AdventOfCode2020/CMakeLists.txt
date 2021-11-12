cmake_minimum_required(VERSION 3.21)
project(aoc2020)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake")
include(aoc_common)

include_directories("include")
add_subdirectory("src")
