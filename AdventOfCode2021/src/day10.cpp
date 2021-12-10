#include <deque>
#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

int score(char c) {
    switch (c) {
    case ')':
        return 3;
    case ']':
        return 57;
    case '}':
        return 1197;
    case '>':
        return 25137;
    default:
        return 0;
    }
}

int64_t score2(char c) {
    switch (c) {
    case ')':
        return 1;
    case ']':
        return 2;
    case '}':
        return 3;
    case '>':
        return 4;
    default:
        return 0;
    }
}

int64_t count(std::string_view s) {
    std::deque<char> match;

    for (char c : s) {
        switch (c) {
        case '(':
            match.push_back(')');
            break;
        case '[':
            match.push_back(']');
            break;
        case '{':
            match.push_back('}');
            break;
        case '<':
            match.push_back('>');
            break;
        default:
            if (c != match.back()) {
                // syntax err
                return score(c);
            } else {
                match.pop_back();
            }
        }
    }
    return 0;
}

int64_t complete(std::string_view s) {
    std::deque<char> match;

    for (char c : s) {
        switch (c) {
        case '(':
            match.push_back(')');
            break;
        case '[':
            match.push_back(']');
            break;
        case '{':
            match.push_back('}');
            break;
        case '<':
            match.push_back('>');
            break;
        default:
            if (c != match.back()) {
                throw std::runtime_error("expected valid string");
            } else {
                match.pop_back();
            }
            break;
        }
    }

    int64_t sc = 0;
    while (match.size()) {
        char c = match.back();
        match.pop_back();
        sc = 5 * sc + score2(c);
    }

    return sc;
}

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    std::ifstream input(args.input_filename);

    std::vector<std::string> entries;

    for (const auto& line : sr::lines(input)) {
        entries.push_back(line);
    }

    std::vector<int64_t> complete_scores;

    int64_t sum = 0;
    for (auto& e : entries) {
        int64_t sc = count(e);
        if (sc != 0) {
            sum += sc;
        } else {
            complete_scores.push_back(complete(e));
        }
    }

    std::ranges::sort(complete_scores);
    
    sr::solution(sum);
    sr::solution(complete_scores[complete_scores.size() / 2]);

    return 0;
}
