#include <chrono>
#include <fmt/format.h>
#include <fstream>
#include <numeric>

#include <sr/sr.hpp>

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

sr::vec3i rotate(sr::vec3i v, rotation r) {
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
    int id;
    std::vector<sr::vec3i> beacons;
    rotation solved_rotation;
    sr::vec3i solved_position{};

    void reorient(rotation r) {
        for (auto& vec : beacons)
            vec = rotate(vec, r);
        solved_rotation = r;
    }

    auto compute_distances(rotation r) const {
        sr::array2d<sr::vec3i> distances(beacons.size(), beacons.size());
        for (size_t i = 0; i < beacons.size(); ++i)
            for (size_t j = 0; j < beacons.size(); ++j)
                distances.at(i, j) = rotate(beacons[i], r) - rotate(beacons[j], r);
        return distances;
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

    std::unordered_set<size_t> solved;
    solved.insert(0);

    std::unordered_set<size_t> unsolved;
    for (size_t i = 1; i < scanners.size(); ++i)
        unsolved.insert(i);

    while (unsolved.size()) {
    solve_next:
        auto t0 = std::chrono::high_resolution_clock::now();
        for (size_t i : unsolved) {
            for (size_t j : solved) {
                auto ds = scanners[j].compute_distances(xr_yu_zf);
                for (int rot = 0; rot < rotation_count; ++rot) {
                    auto du = scanners[i].compute_distances((rotation)rot);
                    for (int dsx = 0; dsx < ds.width(); ++dsx) {
                        int common_dist = 0;
                        for (int dsy = 0; dsy < ds.height(); ++dsy) {
                            if (dsx == dsy)
                                continue;
                            sr::vec3i dsd = ds.at(dsx, dsy);
                            for (int dux = 0; dux < du.width(); ++dux) {
                                for (int duy = 0; duy < du.height(); ++duy) {
                                    if (dux == duy)
                                        continue;
                                    sr::vec3i dud = du.at(dux, duy);
                                    if (dsd == dud) {
                                        ++common_dist;
                                        if (common_dist >= 11) {
                                            auto oriented_solved_beacon =
                                                scanners[j].solved_position + scanners[j].beacons[dsx];
                                            auto nonoriented_unsolved_beacon = scanners[i].beacons[dux];
                                            auto oriented_unsolved_beacon =
                                                rotate(scanners[i].beacons[dux], (rotation)rot);
                                            scanners[i].solved_position =
                                                oriented_solved_beacon - oriented_unsolved_beacon;
                                        }
                                    }
                                }
                            }
                        }
                        // 11 because the point being examined has distance 0,0,0
                        if (common_dist >= 12 - 1) {
                            scanners[i].reorient((rotation)rot);
                            unsolved.erase(i);
                            solved.insert(i);
                            auto t1 = std::chrono::high_resolution_clock::now();
                            std::cout << "solved " << i << " in "
                                      << std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count()
                                      << "ms\n";
                            goto solve_next;
                        }
                    }
                }
            }
            std::cout << "cannot solve " << i << " yet\n";
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
