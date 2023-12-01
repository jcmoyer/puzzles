#!/usr/bin/env python

import sys
import requests
import os.path
import pathlib


def get_session_token():
    p = pathlib.Path.cwd()
    p.resolve()
    while True:
        want = p / "session.txt"
        if want.exists():
            return want.read_text()
        if p == p.parent:
            raise RuntimeError()
        p = p.parent


def get_input(year, day, local_path):
    # don't hit the website if we already have the input
    if os.path.exists(local_path):
        return None
    # e.g.: https://adventofcode.com/2022/day/1/input
    input_url = f"https://adventofcode.com/{year}/day/{day}/input"
    # https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/
    headers = {"User-Agent": "https://github.com/jcmoyer/puzzles"}
    cookies = {"session": get_session_token()}
    print(f"GET {input_url}")
    req = requests.get(input_url, cookies=cookies, headers=headers)
    if req.status_code != 200:
        print(f"Server responded with status {req.status_code}")
        sys.exit(1)
    with open(local_path, "w") as f:
        f.write(req.text)
    return req.text


def get_input_text(year, day, local_path=None):
    """Programmatic interface to get_input."""
    if local_path is None:
        local_path = f"{year}-{day:>02}-input.txt"
    maybe_io_text = get_input(year, day, local_path)
    if maybe_io_text:
        return maybe_io_text
    else:
        with open(local_path) as f:
            return f.read()


def main():
    if len(sys.argv) < 2:
        print("Not enough arguments. Usage: aoctool <command> [<command args>]")
        sys.exit(1)

    if sys.argv[1] == "get-input":
        if len(sys.argv) != 5:
            print(
                "Not enough arguments. Usage: aoctool get-input <year> <day> <localpath>"
            )
            sys.exit(1)
        year = int(sys.argv[2])
        day = int(sys.argv[3])
        local_path = sys.argv[4]
        get_input(year, day, local_path)


if __name__ == "__main__":
    main()
