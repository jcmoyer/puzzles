set(PYTHON_BIN python CACHE STRING "name of python binary to use for the test runner")

find_package(sr CONFIG REQUIRED)
find_package(fmt CONFIG REQUIRED)

enable_testing()

set(AOC_TARGETS "")

function(aoc_add_day n)
    if(NOT "${n}" MATCHES "[0-2][0-9]")
        message(FATAL_ERROR "invalid day: \"${n}\"; must be two-digit string from 01..25")
    endif()

    add_executable("day${n}" "day${n}.cpp")
    target_link_libraries("day${n}" sr::sr fmt::fmt)
    set_property(TARGET "day${n}" PROPERTY CXX_STANDARD 20)
    install(TARGETS "day${n}" RUNTIME DESTINATION .)

    if(NOT "${ARGV}" MATCHES "NO_TEST")
        add_test(
        NAME "day${n}-test"
        COMMAND ${PYTHON_BIN}
            "${CMAKE_SOURCE_DIR}/../scripts/test_runner.py"
            $<TARGET_FILE:day${n}>
            "${CMAKE_SOURCE_DIR}/test/day${n}-input.txt"
            "${CMAKE_SOURCE_DIR}/test/day${n}-output.txt"
        )
    endif()
  
    if(MSVC)
        target_compile_options("day${n}" PRIVATE /W3 /constexpr:steps2048576)
    else()
        target_compile_options("day${n}" PRIVATE -Wall -Wextra -pedantic -march=native)
    endif()

    list(APPEND AOC_TARGETS "day${n}")
    set(AOC_TARGETS ${AOC_TARGETS} PARENT_SCOPE)
endfunction()
