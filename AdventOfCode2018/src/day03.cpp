#include <charconv>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <string>
#include <vector>

struct rectangle {
    int x, y, w, h;
    int right() const {
        return x + w;
    }
    int bottom() const {
        return y + h;
    }
};

bool intersects(const rectangle& a, const rectangle& b) {
    return !(a.bottom() < b.y || a.y > b.bottom() || a.x > b.right() || a.right() < b.x);
}

struct claim {
    int id;
    rectangle area;
};

bool parse_claim(std::string_view s, claim& c) {
    static std::regex claim_regex(R"(#(\d+) @ (\d+),(\d+): (\d+)x(\d+))");
    std::cmatch m;
    if (std::regex_match(s.data(), m, claim_regex)) {
        std::from_chars(m[1].first, m[1].second, c.id);
        std::from_chars(m[2].first, m[2].second, c.area.x);
        std::from_chars(m[3].first, m[3].second, c.area.y);
        std::from_chars(m[4].first, m[4].second, c.area.w);
        std::from_chars(m[5].first, m[5].second, c.area.h);
        return true;
    } else {
        return false;
    }
}

struct fabric_cell {
    std::vector<int> claimers;
    size_t num_claims() const {
        return claimers.size();
    }
};

struct canvas {
    static constexpr size_t SIZE = 1000;
    int width = SIZE, height = SIZE;

    std::vector<fabric_cell> fabric;

    canvas() {
        fabric.resize(width * height);
    }

    void draw_claim(const claim& cl) {
        for (int y = 0; y < cl.area.h; ++y) {
            for (int x = 0; x < cl.area.w; ++x) {
                int fabric_x = cl.area.x + x;
                int fabric_y = cl.area.y + y;
                xy(fabric_x, fabric_y).claimers.push_back(cl.id);
            }
        }
    }

    fabric_cell& xy(int x, int y) {
        if (x >= width)
            throw "out of range";
        if (y >= height)
            throw "out of range";
        return fabric[y * width + x];
    }
};

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);

    std::string line;
    std::vector<claim> claims;
    std::map<int, int> claims_id_to_index;

    canvas cv;

    while (std::getline(input, line)) {
        claim c;
        if (parse_claim(line, c)) {
            claims_id_to_index[c.id] = claims.size();
            claims.push_back(c);
            cv.draw_claim(c);
        }
    }

    std::set<int> candidate_ids;
    int total_twos = 0;
    for (int y = 0; y < cv.height; ++y) {
        for (int x = 0; x < cv.width; ++x) {
            if (cv.xy(x, y).num_claims() >= 2) {
                ++total_twos;
            } else if (cv.xy(x, y).num_claims() == 1) {
                candidate_ids.emplace(cv.xy(x, y).claimers[0]);
            }
        }
    }

    std::cout << total_twos << std::endl;

    int one;
    // refine search results for the one that does not overlap
    for (int cid : candidate_ids) {
        const claim& self = claims[claims_id_to_index[cid]];
        bool is_lonely = true;
        for (const claim& other : claims) {
            if (&self == &other)
                continue;
            if (intersects(self.area, other.area)) {
                is_lonely = false;
                one = cid;
                break;
            }
        }
        if (is_lonely) {
            one = cid;
            break;
        }
    }

    std::cout << one << std::endl;

    return 0;
}
