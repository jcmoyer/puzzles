#include <chrono>
#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>
#include <sr/xxhash.h>

// standard form:
//
// y
// ^ z
// |/
// +-->x
//
// r = right
// l = left
// u = up
// d = down
// f = forward (away from viewer)
// b = backward (towards viewer)
enum rotation {
    // Y-up
    xr_yu_zf, // standard form, assume scanner 0 has this orientation
    xb_yu_zr,
    xl_yu_zb,
    xf_yu_zl,
    // Y-down
    xr_yd_zb,
    xf_yd_zr,
    xb_yd_zl,
    xl_yd_zf,
    // X-up
    xu_yl_zf,
    xu_yf_zr,
    xu_yr_zb,
    xu_yb_zl,
    // X-down
    xd_yf_zl,
    xd_yr_zf,
    xd_yb_zr,
    xd_yl_zb,
    // Z-up
    xf_yr_zu,
    xr_yb_zu,
    xb_yl_zu,
    xl_yf_zu,
    // Z-down
    xr_yf_zd,
    xb_yr_zd,
    xl_yb_zd,
    xf_yl_zd,
    rotation_count,
};
static_assert(rotation_count == 24);

[[nodiscard]] sr::vec3i rotate(const sr::vec3i& v, rotation r) {
    switch (r) {
        // Y-up
    case xr_yu_zf: // standard form; assume vector starts in this form
        return v;
    case xb_yu_zr:
        return {v.z(), v.y(), -v.x()};
    case xl_yu_zb:
        return {-v.x(), v.y(), -v.z()};
    case xf_yu_zl:
        return {-v.z(), v.y(), v.x()};
        // Y-down
    case xr_yd_zb:
        return {v.x(), -v.y(), -v.z()};
    case xf_yd_zr:
        return {v.z(), -v.y(), v.x()};
    case xb_yd_zl:
        return {-v.z(), -v.y(), -v.x()};
    case xl_yd_zf:
        return {-v.x(), -v.y(), v.z()};
        // X-up
    case xu_yl_zf:
        return {-v.y(), v.x(), v.z()};
    case xu_yf_zr:
        return {v.z(), v.x(), v.y()};
    case xu_yr_zb:
        return {v.y(), v.x(), -v.z()};
    case xu_yb_zl:
        return {-v.z(), v.x(), -v.y()};
        // X-down
    case xd_yf_zl:
        return {-v.z(), -v.x(), v.y()};
    case xd_yr_zf:
        return {v.y(), -v.x(), v.z()};
    case xd_yb_zr:
        return {v.z(), -v.x(), -v.y()};
    case xd_yl_zb:
        return {-v.y(), -v.x(), -v.z()};
        // Z-up
    case xf_yr_zu:
        return {v.y(), v.z(), v.x()};
    case xr_yb_zu:
        return {v.x(), v.z(), -v.y()};
    case xb_yl_zu:
        return {-v.y(), v.z(), -v.x()};
    case xl_yf_zu:
        return {-v.x(), v.z(), v.y()};
        // Z-down
    case xr_yf_zd:
        return {v.x(), -v.z(), v.y()};
    case xb_yr_zd:
        return {v.y(), -v.z(), -v.x()};
    case xl_yb_zd:
        return {-v.x(), -v.z(), -v.y()};
    case xf_yl_zd:
        return {-v.y(), -v.z(), v.x()};
    }
    throw std::runtime_error("bad rotation");
}

std::ostream& operator<<(std::ostream& stream, const sr::vec3i& v) {
    return stream << "<" << v.x() << "," << v.y() << "," << v.z() << ">";
}

struct scanner {
    size_t id;
    std::vector<sr::vec3i> beacons;
    rotation solved_rotation;
    sr::vec3i solved_position{};
    sr::array2d<sr::vec3i> distance_maps[rotation_count];

    void reorient(rotation r) {
        for (auto& vec : beacons)
            vec = rotate(vec, r);
        solved_rotation = r;
        // IMPORTANT: solved orientation needs cache invalidated!
        // otherwise the solver will try to match unsolved scanners with the old orientation!
        distance_maps[xr_yu_zf].resize(0, 0);
    }

    const sr::array2d<sr::vec3i>& compute_distances(rotation r) {
        if (distance_maps[r].size() == 0) {
            distance_maps[r].resize(beacons.size(), beacons.size());
            for (size_t i = 0; i < beacons.size(); ++i)
                for (size_t j = 0; j < beacons.size(); ++j)
                    distance_maps[r].at(i, j) = rotate(beacons[i], r) - rotate(beacons[j], r);
        }
        return distance_maps[r];
    }
};

struct solve_key {
    size_t solved_id;
    size_t unsolved_id;
    rotation rot;
    constexpr auto operator<=>(const solve_key&) const = default;
};

template <>
struct std::hash<solve_key> {
    size_t operator()(const solve_key& k) const {
        return XXH3_64bits(&k, sizeof(k));
    }
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);
    std::vector<scanner> scanners;

    for (const auto& line : sr::lines(args.get_input_stream())) {
        if (line.size() == 0)
            continue;
        try {
            size_t scanner_id;
            sr::parse(R"(--- scanner (\d+) ---)", line, scanner_id);
            scanner& sc = scanners.emplace_back();
            sc.id = scanner_id;
        } catch (const sr::bad_match&) {
            sr::vec3i pos;
            sr::parse(R"((\-?\d+),(\-?\d+),(\-?\d+))", line, pos.x(), pos.y(), pos.z());
            scanners.back().beacons.push_back(pos);
        }
    }

    std::vector<size_t> solved;
    solved.push_back(0);

    std::vector<size_t> unsolved;
    for (size_t i = 1; i < scanners.size(); ++i)
        unsolved.push_back(i);

    std::unordered_set<solve_key> tried;

    while (unsolved.size()) {
    solve_next:
        auto t0 = std::chrono::high_resolution_clock::now();
        for (size_t unsolved_ix = 0; unsolved_ix < unsolved.size(); ++unsolved_ix) {
            for (size_t solved_ix = 0; solved_ix < solved.size(); ++solved_ix) {
                scanner& unsolved_scanner = scanners[unsolved[unsolved_ix]];
                scanner& solved_scanner = scanners[solved[solved_ix]];
                const auto& ds = solved_scanner.compute_distances(xr_yu_zf);
                for (int rot = 0; rot < rotation_count; ++rot) {
                    solve_key key{solved_scanner.id, unsolved_scanner.id, (rotation)rot};
                    if (tried.contains(key))
                        continue;
                    tried.insert(key);
                    const auto& du = unsolved_scanner.compute_distances((rotation)rot);
                    for (int dsy = 0; dsy < ds.height(); ++dsy) {
                        int common_dist = 0;
                        for (int dsx = 0; dsx < ds.width(); ++dsx) {
                            if (dsx == dsy)
                                continue;
                            const sr::vec3i& dsd = ds.at(dsx, dsy);
                            for (int duy = 0; duy < du.height(); ++duy) {
                                for (int dux = 0; dux < du.width(); ++dux) {
                                    if (dux == duy)
                                        continue;
                                    const sr::vec3i& dud = du.at(dux, duy);
                                    if (dsd == dud) {
                                        ++common_dist;
                                        // 11 because the point being examined has distance 0,0,0
                                        if (common_dist >= 11) {
                                            auto oriented_solved_beacon =
                                                solved_scanner.solved_position + solved_scanner.beacons[dsx];
                                            auto oriented_unsolved_beacon =
                                                rotate(unsolved_scanner.beacons[dux], (rotation)rot);
                                            unsolved_scanner.solved_position =
                                                oriented_solved_beacon - oriented_unsolved_beacon;
                                            solved.push_back(unsolved[unsolved_ix]);
                                            std::iter_swap(unsolved.begin() + unsolved_ix, unsolved.end() - 1);
                                            unsolved.pop_back();
                                            unsolved_scanner.reorient((rotation)rot);
                                            auto t1 = std::chrono::high_resolution_clock::now();
                                            std::cerr << "solved " << unsolved_scanner.id << " (" << solved.size()
                                                      << "/" << unsolved.size() + solved.size() << ") in "
                                                      << std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0)
                                                             .count()
                                                      << "ms\n";
                                            goto solve_next;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    std::unordered_set<sr::vec3i> unique_points;
    for (auto& sc : scanners) {
        for (auto& vec : sc.beacons) {
            unique_points.insert(sc.solved_position + vec);
        }
    }
    sr::solution(unique_points.size());

    size_t max_distance = 0;
    for (size_t i = 0; i < scanners.size(); ++i)
        for (size_t j = i; j < scanners.size(); ++j)
            max_distance = std::max(
                max_distance, (size_t)sr::manhattan(scanners[i].solved_position, scanners[j].solved_position));
    sr::solution(max_distance);

    return 0;
}
