#!/usr/bin/env python

import argparse
import subprocess
import glob
import re
from pathlib import Path


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--mode",
        choices=["Debug", "Release"],
        default="Debug",
        help="build configuration to test",
    )

    args = parser.parse_args()

    mode = args.mode
    proc = subprocess.run(["gprbuild", "-j0", f"-XBuild={mode}"])
    if proc.returncode != 0:
        print("test-all: gprbuild failed; aborting test")
        exit(1)

    inputs = glob.glob("test/*-input.txt")
    for input in inputs:
        output = input.replace("input", "output")
        year, day = re.match(".*?(\d{4})\-(\d{2})\-input.txt$", input).groups()
        exe = f"bin/{mode}/day{day}"

        if not Path(output).exists():
            print(f"day{day}: SKIP (no corresponding output at '{output}')")
            continue

        try:
            proc = subprocess.run([exe], capture_output=True)
        except FileNotFoundError:
            print(f"day{day}: SKIP (no executable at '{exe}')")
            continue

        cmd = ["python", "../scripts/test_runner.py", exe, input, output]
        proc = subprocess.run(cmd)

        if proc.returncode == 0:
            print(f"day{day}: PASS")
        else:
            print(f"day{day}: FAIL")


if __name__ == "__main__":
    main()
