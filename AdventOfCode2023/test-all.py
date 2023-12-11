#!/usr/bin/env python

import argparse
import subprocess
import glob
import re
from pathlib import Path
import multiprocessing
import time
from dataclasses import dataclass


@dataclass
class AsyncTestResult:
    test_name: str
    elapsed_ns: int
    stdout: str
    stderr: str
    returncode: int


def format_ns(t):
    if t < 1000:
        return f"{t}ns"
    t //= 1000
    if t < 1000:
        return f"{t}us"
    t //= 1000
    if t < 1000:
        return f"{t}ms"
    t //= 1000
    if t < 1000:
        return f"{t}s"


def async_run_test(test_name, cmd):
    t0 = time.time_ns()
    proc = subprocess.run(cmd, capture_output=True)
    t1 = time.time_ns()

    return AsyncTestResult(
        test_name=test_name,
        elapsed_ns=t1 - t0,
        stdout=proc.stdout.decode(),
        stderr=proc.stderr.decode(),
        returncode=proc.returncode,
    )


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

    with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
        tasks = []

        for input in inputs:
            output = input.replace("input", "output")
            year, day = re.match(".*?(\d{4})\-(\d{2})\-input.txt$", input).groups()
            exe = f"bin/{mode}/day{day}"

            test_name = f"day{day}"

            if not Path(output).exists():
                print(f"{test_name}: SKIP (no corresponding output at '{output}')")
                continue

            try:
                proc = subprocess.run([exe], capture_output=True)
            except FileNotFoundError:
                print(f"{test_name}: SKIP (no executable at '{exe}')")
                continue

            cmd = ["python", "../scripts/test_runner.py", exe, input, output]
            tasks.append(pool.apply_async(async_run_test, (test_name, cmd)))

        for async_result in tasks:
            test_result = async_result.get()
            if test_result.returncode == 0:
                print(
                    f"{test_result.test_name}: PASS........elapsed: {format_ns(test_result.elapsed_ns)}"
                )
            else:
                print(
                    f"{test_result.test_name}: FAIL........elapsed: {format_ns(test_result.elapsed_ns)}"
                )
                # TODO: test runner should probably write to stderr
                print(f"{test_result.stdout}")


if __name__ == "__main__":
    main()
