cmake_minimum_required(VERSION 3.11)

set(PYTHON_BIN python CACHE STRING "name of python binary to use for the test runner")

enable_testing()

if(MSVC)
  if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
    string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  endif()
endif()

include_directories("include")
add_subdirectory("src")
