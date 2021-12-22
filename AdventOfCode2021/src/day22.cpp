#include <cassert>

#include <sr/sr.hpp>

struct aabb {
    sr::vec3i min, max;

    bool contains(const sr::vec3i& p) const {
        return p.x() >= min.x() && p.y() >= min.y() && p.z() >= min.z() && p.x() <= max.x() && p.y() <= max.y() &&
               p.z() <= max.z();
    }

    bool within(const sr::vec3i& bounds_min, const sr::vec3i& bounds_max) const {
        return min.x() >= bounds_min.x() && min.y() >= bounds_min.y() && min.z() >= bounds_min.z() &&
               max.x() <= bounds_max.x() && max.y() <= bounds_max.y() && max.z() <= bounds_max.z();
    }

    bool within(const aabb& box) const {
        return within(box.min, box.max);
    }

    uint64_t width() const {
        return 1 + max.x() - min.x();
    }

    uint64_t height() const {
        return 1 + max.y() - min.y();
    }

    uint64_t depth() const {
        return 1 + max.z() - min.z();
    }

    uint64_t volume() const {
        return width() * height() * depth();
    }

    bool intersection(const aabb& box, aabb* out) const {
        int left_x = std::max(min.x(), box.min.x());
        int right_x = std::min(max.x(), box.max.x());
        int bottom_y = std::max(min.y(), box.min.y());
        int top_y = std::min(max.y(), box.max.y());
        int back_z = std::max(min.z(), box.min.z());
        int front_z = std::min(max.z(), box.max.z());

        if (left_x <= right_x && bottom_y <= top_y && back_z <= front_z) {
            if (out != nullptr)
                *out = aabb{
                    {left_x, bottom_y, back_z},
                    {right_x, top_y, front_z},
                };
            return true;
        } else {
            return false;
        }
    }
};

void assert_valid_box(const aabb& box) {
    assert(box.min.x() <= box.max.x());
    assert(box.min.y() <= box.max.y());
    assert(box.min.z() <= box.max.z());
}

struct cuboid_instr {
    aabb box;
    bool on;
};

struct volume {
    std::vector<aabb> boxes;

    void push_aabb(const aabb& val) {
        boxes.push_back(val);
        assert_valid_box(val);
    }

    auto begin() const {
        return boxes.begin();
    }

    auto end() const {
        return boxes.end();
    }

    auto begin() {
        return boxes.begin();
    }

    auto end() {
        return boxes.end();
    }

    auto size() {
        return boxes.size();
    }

    uint64_t total_volume() {
        uint64_t sum = 0;
        for (const auto& b : *this)
            sum += b.volume();
        return sum;
    }
};

// Splits a single box into up to 6 boxes by subtracting the volume of a second box. This is done by splitting the
// first box on the planes formed by the faces of the second box. The volume of `box` to the left of `subtraction` is
// one box; the volume below the bottom of `subtraction` is another, and so on.
//
// - The volumes left and right of `subtraction` are smallest
// - The volumes on top of and below `subtraction` are larger and cover the first two volumes when viewed along the Y
//   axis
// - The volumes in front of and behind `subtraction` are the largest and cover all the other volumes when viewed along
//   the Z axis
//
// INT_MIN and INT_MAX are used to split the geometry to "infinity," but any constants larger than the puzzle bounds
// will work.
volume subtract(const aabb& box, const aabb& subtraction) {
    volume v;
    if (!box.intersection(subtraction, nullptr)) {
        v.push_aabb(box);
        return v;
    }
    // Important edge case: If `subtraction` min coordinate is <= `box` min coordinate, there is no volume on the other
    // side. Likewise for `subtraction` max coordinate >= `box` max coordinate.
    aabb left;
    if (subtraction.min.x() <= box.max.x() && subtraction.min.x() > box.min.x() &&
        box.intersection(
            {

                {INT_MIN, subtraction.min.y(), subtraction.min.z()},
                {subtraction.min.x(), subtraction.max.y(), subtraction.max.z()},
            },
            &left)) {
        --left.max.x();
        v.push_aabb(left);
    }
    aabb right;
    if (subtraction.max.x() >= box.min.x() && subtraction.max.x() < box.max.x() &&
        box.intersection(
            {
                {subtraction.max.x(), subtraction.min.y(), subtraction.min.z()},
                {INT_MAX, subtraction.max.y(), subtraction.max.z()},
            },
            &right)) {
        ++right.min.x();
        v.push_aabb(right);
    }

    aabb bottom;
    if (subtraction.min.y() <= box.max.y() && subtraction.min.y() > box.min.y() &&
        box.intersection(
            {
                {INT_MIN, INT_MIN, subtraction.min.z()},
                {INT_MAX, subtraction.min.y(), subtraction.max.z()},
            },
            &bottom)) {
        --bottom.max.y();
        v.push_aabb(bottom);
    }
    aabb top;
    if (subtraction.max.y() >= box.min.y() && subtraction.max.y() < box.max.y() &&
        box.intersection(
            {
                {INT_MIN, subtraction.max.y(), subtraction.min.z()},
                {INT_MAX, INT_MAX, subtraction.max.z()},
            },
            &top)) {
        ++top.min.y();
        v.push_aabb(top);
    }
    aabb back;
    if (subtraction.min.z() <= box.max.z() && subtraction.min.z() > box.min.z() &&
        box.intersection(
            {
                {INT_MIN, INT_MIN, INT_MIN},
                {INT_MAX, INT_MAX, subtraction.min.z()},
            },
            &back)) {
        back.max.z()--;
        v.push_aabb(back);
    }
    aabb front;
    if (subtraction.max.z() >= box.min.z() && subtraction.max.z() < box.max.z() &&
        box.intersection(
            {
                {INT_MIN, INT_MIN, subtraction.max.z()},
                {INT_MAX, INT_MAX, INT_MAX},
            },
            &front)) {
        front.min.z()++;
        v.push_aabb(front);
    }
    return v;
}

class world {
public:
    // Inserts a box, subtracting from existing boxes as needed so there is no overlap.
    void insert(const aabb& box) {
        if (boxes.size() == 0) {
            boxes.push_back(box);
            return;
        }
        // subtract from all existing boxes
        std::vector<volume> new_volumes;
        for (const aabb& existing : boxes) {
            new_volumes.push_back(subtract(existing, box));
        }
        boxes.clear();
        boxes.push_back(box);
        for (const volume& v : new_volumes) {
            for (const aabb& b : v) {
                boxes.push_back(b);
            }
        }
    }

    // Subtracts `box` from all current boxes, producing volumes whose boxes replace the current ones.
    void erase(const aabb& box) {
        std::vector<volume> new_volumes;
        for (const aabb& old : boxes) {
            new_volumes.push_back(subtract(old, box));
        }
        boxes.clear();
        for (const volume& v : new_volumes) {
            for (const aabb& b : v) {
                boxes.push_back(b);
            }
        }
    }

    uint64_t total_volume() const {
        uint64_t sum = 0;
        for (const auto& b : boxes) {
            sum += b.volume();
        }
        return sum;
    }

private:
    std::vector<aabb> boxes;
};

int main(int argc, char* argv[]) {
    auto args = sr::parse_command_line(argc, argv);

    // aabb box0{{10, 10, 10}, {12, 12, 12}};
    // aabb box1{{11, 11, 11}, {13, 13, 13}};
    // aabb box2{{9, 9, 9}, {11, 11, 11}};
    // world ww;
    // ww.insert(box0);
    // ww.insert(box1);
    // assert(ww.total_volume() == 46);
    // ww.erase(box2);
    // assert(ww.total_volume() == 46 - 8);

    std::vector<cuboid_instr> entries;
    for (const auto& line : sr::lines(args.get_input_stream())) {
        auto& c = entries.emplace_back();
        std::string onoff;
        sr::parse(R"((\w+) x=(\-?\d+)..(\-?\d+),y=(\-?\d+)..(\-?\d+),z=(\-?\d+)..(\-?\d+))",
            line,
            onoff,
            c.box.min.x(),
            c.box.max.x(),
            c.box.min.y(),
            c.box.max.y(),
            c.box.min.z(),
            c.box.max.z());
        c.on = onoff == "on";
        assert_valid_box(c.box);
    }

    // part 1
    {
        world w;
        for (const auto& e : entries) {
            if (!e.box.within({-50, -50, -50}, {50, 50, 50}))
                continue;
            if (e.on)
                w.insert(e.box);
            else
                w.erase(e.box);
        }
        sr::solution(w.total_volume());
    }

    // part 2
    {
        world w;
        for (const auto& e : entries) {
            if (e.on)
                w.insert(e.box);
            else
                w.erase(e.box);
        }
        sr::solution(w.total_volume());
    }

    return 0;
}
