#!/usr/bin/env python

import glob
import subprocess
from pathlib import Path
from dataclasses import dataclass
import textwrap
import re


@dataclass
class FailedTest:
    source_path: Path
    stderr: str


def get_failed_assert(stacktrace, source_filename):
    m = re.search(r"Test_.*? at (test_.*?\.adb):(\d+)", stacktrace, re.MULTILINE)
    if m:
        line_number = int(m.group(2))
        with open(source_filename, "r") as f:
            lines = f.readlines()
            return lines[line_number - 1].strip()


def main():
    # TODO: Release mode testing currently not implemented
    mode = "Debug"
    test_sources = glob.glob("src/test_*.adb")

    subprocess.run(["gprbuild", "-j0", f"-XBuild={mode}"], check=True)

    test_count = len(test_sources)
    test_pass = 0
    test_fail = 0
    fail_list = []

    print(f"Discovered {test_count} tests")

    for source in test_sources:
        source_path = Path(source)
        binary_name = source_path.stem
        binary_path = f"bin/{mode}/{binary_name}"

        print(f"Run {binary_path}...")
        proc = subprocess.run([binary_path], capture_output=True)
        if proc.returncode == 0:
            test_pass += 1
            print("Pass")
        else:
            test_fail += 1
            fail_list.append(
                FailedTest(source_path=source_path, stderr=proc.stderr.decode().strip())
            )
            print("Fail")

    print(f"{test_pass}/{test_count} tests passed; {test_fail} tests failed")

    if len(fail_list) > 0:
        print("\nFailed tests:\n")
        for failed in fail_list:
            print(f"   - {failed.source_path}")
            if len(failed.stderr) > 0:
                print("\n     stderr:")
                print(textwrap.indent(failed.stderr, prefix="        "))
                print("\n     most recent line in test file:")
                print(
                    textwrap.indent(
                        get_failed_assert(failed.stderr, failed.source_path),
                        prefix="        ",
                    )
                )


if __name__ == "__main__":
    main()
