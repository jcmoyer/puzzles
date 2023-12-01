#!/usr/bin/env python

import sys
import subprocess
import difflib
import textwrap


def main():
    if len(sys.argv) != 4:
        sys.exit(1)

    binary = sys.argv[1]
    input_filename = sys.argv[2]
    output_filename = sys.argv[3]

    try:
        with open(output_filename, "r") as f:
            expected = f.read().strip()
    except FileNotFoundError:
        print("output file does not exist")
        sys.exit(1)

    proc = subprocess.run(
        [binary, input_filename],
        # We don't use stdin, but this function randomly fails when test_runner
        # is called from a zig build script if we don't pipe it. This may be
        # because zig passes "\Device\Null" when spawning a child process with
        # ignored stdin, but need to investigate more.
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        encoding="utf-8",
    )
    results = proc.stdout.strip()

    if proc.returncode != 0:
        print("process exited with non-zero return code")
        sys.exit(1)
    if results != expected:
        print("result differs from expected; context diff:\n")
        diff = difflib.context_diff(
            expected.splitlines(),
            results.splitlines(),
            n=2,
            lineterm="",
            fromfile="expected",
            tofile="actual",
        )
        print(textwrap.indent("\n".join(diff), "  "))
        sys.exit(1)

    sys.exit(0)


if __name__ == "__main__":
    main()
