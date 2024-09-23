This is an Ada/SPARK solution for [Advent of Code 2015 day
4](https://adventofcode.com/2015/day/4).

Features a SPARK Silver level MD5 implementation roughly based on the reference
C implementation in [RFC 1321](https://www.ietf.org/rfc/rfc1321.txt).

## Building, Running, and Verification

Requires [Alire](https://alire.ada.dev/) and GNAT.

```
# run tests
$ alr build --validation
$ alr run md5_test[.exe]

# run solver (accepts puzzle input as command line parameter)
$ alr build --release
$ alr run day04[.exe]

# verify (--level=3 should be sufficient)
$ alr gnatprove --level=3
```

## Optimizations

### Short Strings

Short strings are strings with a length <= 55. These are special because they
can be hashed with a single block transform. All strings in this puzzle meet
this criteria. The MD5 implementation provides a fast path for short strings
that bypasses the general block transform, constructing the block in-place.
This eliminates some branching since all of the required information is known
at the point the hash is performed.

This could theoretically be done with Ada's standard bounded strings, but in
practice there is a significant performance penalty because the bounded string
buffer is inaccessible. Even providing an inlined getter to access the buffer
is not zero-cost - measured, there is a 15% difference in total runtime.

### Digest Testing

A somewhat obvious optimization is to not create a hex string digest for each
hash. It's enough to do a bitwise-and and compare with zero to test for 5 or 6
leading zeroes on the first 32-bit word (`Md5.Hashing.State.A` in this
implementation).

### Tasking

The solver uses Ada tasking to find eligible hashes using multiple threads.
Each task starts from a unique integer position modulo the number of tasks and
steps the position by the number of tasks so that the tasks never duplicate
work. As tasks find solutions they submit answers to a shared, protected
object. When a solution with 6 leading zeroes is found, the number that
produced that solution becomes the upper search bound. Tasks will
self-terminate after reaching that number themselves. This performs very well
and eliminates the need to explicitly use locks or atomics.

### Benchmarks

I looked for fast solutions to compare against, and the fastest I've found so
far is [maneatingape/advent-of-code-rust](https://github.com/maneatingape/advent-of-code-rust).
If you're reading this and have a faster implementation, please open an issue
so I can benchmark it!

Some benchmarks done with `hyperfine --warmup 10` on an i5-2500K:

```
jcmoyer/puzzles (this)
Configuration: LTO, -march=native, -gnatp

  Time (mean ± σ):     305.3 ms ±   2.3 ms    [User: 1188.4 ms, System: 8.5 ms]
  Range (min … max):   303.3 ms … 308.6 ms    10 runs

maneatingape/advent-of-code-rust
Configuration: LTO, -march=native

  Time (mean ± σ):     324.5 ms ±   1.1 ms    [User: 1273.4 ms, System: 2.8 ms]
  Range (min … max):   323.4 ms … 326.5 ms    10 runs

maneatingape/advent-of-code-rust
Configuration: LTO, -F SIMD, -C target-cpu=native

  Time (mean ± σ):     198.1 ms ±   3.9 ms    [User: 774.8 ms, System: 4.3 ms]
  Range (min … max):   191.5 ms … 204.3 ms    15 runs
```

SIMD provides a pretty massive speed up, but it's not trivial to use in Ada. I
have experimented with this a bit but it involves wrapping gcc intrinsics,
which is a lot of work and not very well-documented.

## SPARK status

Each package under `Md5.*` has full SPARK annotations. There is no use of
`pragma Assume`. The `day04` and `md5_test` executables are not written in
SPARK.
