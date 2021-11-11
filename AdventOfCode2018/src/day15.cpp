#include <algorithm>
#include <atomic>
#include <cassert>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <queue>
#include <set>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "common/point.hpp"

struct tile {
    enum type { none, floor, wall };
    static constexpr int no_actor = -1;
    type ty;
    int actor_id = no_actor;

    tile() : ty{none} {}
    tile(type t) : ty{t} {}

    bool walkable() const {
        return ty == floor && actor_id == no_actor;
    }
};

struct actor {
    enum type { elf, goblin, none };
    int id;
    type ty;
    point position;
    int hp = 200;
    int attack = 3;
    bool alive() const {
        return hp > 0;
    }
    type enemy_type() const {
        return ty == elf ? goblin : elf;
    }
};

void fight(actor& a, actor& b) {
    assert(a.enemy_type() != b.enemy_type());
    b.hp -= a.attack;
}

struct win_state {
    int elves_alive = 0;
    int goblins_alive = 0;
    int remaining_hp = 0;
    int turns = 0;
    int elf_attack = 0;

    bool win() const {
        return elves_alive == 0 || goblins_alive == 0;
    }

    actor::type winner() const {
        if (elves_alive == 0)
            return actor::goblin;
        else if (goblins_alive == 0)
            return actor::elf;
        else
            return actor::none;
    }

    int score() const {
        return remaining_hp * turns;
    }
};

struct world {
    world(int width, int height) : width{width}, height{height}, tiles(width * height) {}

    void add_tile(int x, int y, tile::type ty) {
        tile_at_point(x, y) = tile{ty};
    }

    void add_actor(int x, int y, actor::type ty) {
        int actor_id = next_aid++;
        actors.emplace_back(actor{actor_id, ty, point{x, y}});
        tile_at_point(x, y).actor_id = actor_id;
    }

    void tick(win_state& wstate) {
        // sort by turn order - making a copy of actors here so they have stable indices
        std::vector<std::size_t> sorted_actors(actors.size(), 0);
        for (size_t i = 0; i < actors.size(); ++i) {
            sorted_actors[i] = i;
        }
        std::sort(sorted_actors.begin(), sorted_actors.end(), [this](std::size_t i, std::size_t j) {
            return actors[i].position < actors[j].position;
        });

        for (int i = 0; i < sorted_actors.size(); ++i) {
            int actor_id = sorted_actors[i];
            actor& a = actors[actor_id];
            if (!a.alive())
                continue;

            if (check_win(wstate)) {
                return;
            }

            int target_id;
            if (get_adjacent_target(actor_id, target_id)) {
                actor& target = actors[target_id];
                fight(a, target);
                if (!target.alive()) {
                    tile_at_point(target.position).actor_id = tile::no_actor;
                }
            } else {
                make_move(actor_id);
                if (get_adjacent_target(actor_id, target_id)) {
                    actor& target = actors[target_id];
                    fight(a, target);
                    if (!target.alive()) {
                        tile_at_point(target.position).actor_id = tile::no_actor;
                    }
                }
            }
        }

        ++turns;
    }

    struct path_results2 {
        std::vector<std::vector<point>> paths;
        point destination(int i) const {
            return paths[i].back();
        }
        int size(int i) const {
            return paths[i].size();
        }
    };

    struct dist_compare {
        std::unordered_map<point, int>& dist;
        dist_compare(std::unordered_map<point, int>& dist) : dist{dist} {}
        bool operator()(const point& p1, const point& p2) {
            return dist[p1] > dist[p2];
        }
    };

    template <typename T>
    struct skip_vector {
        std::vector<T> vec;
        std::size_t skipped = 0;
        auto begin() {
            return vec.begin() + skipped;
        }
        auto begin() const {
            return vec.begin() + skipped;
        }
        auto end() {
            return vec.end();
        }
        auto end() const {
            return vec.end();
        }
        T& front() {
            return *begin();
        }
        void skip() {
            ++skipped;
        }
        void push_back(const T& val) {
            vec.push_back(val);
        }
        std::size_t size() const {
            return vec.size();
        }
        void reserve(std::size_t n) {
            vec.reserve(n);
        }
    };

    struct node_info {
        int dist;
        point prev;
    };

    path_results2 shortest_path2(point from, std::vector<point>& to_any_of) {
        // sort for binary searching
        std::sort(to_any_of.begin(), to_any_of.end());
        std::unordered_map<point, int> dist;
        std::unordered_map<point, point> prev;
        skip_vector<point> q;
        q.reserve(512);
        dist_compare heap_comp{dist};

        q.push_back(from);
        dist[from] = 0;

        path_results2 r;

        while (q.begin() != q.end()) {
            point p = q.front();
            q.skip();

            if (std::binary_search(to_any_of.begin(), to_any_of.end(), p)) {
                r.paths.emplace_back(std::vector<point>{p});
                if (r.paths.size() == to_any_of.size()) {
                    break;
                }
            }

            // examine walkable neighbors and assign a distance
            if (auto d = p - point{0, 1}; in_bounds(d) && tile_at_point(d).walkable() && !dist.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                std::push_heap(q.begin(), q.end(), heap_comp);
            }
            if (auto d = p - point{1, 0}; in_bounds(d) && tile_at_point(d).walkable() && !dist.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                std::push_heap(q.begin(), q.end(), heap_comp);
            }
            if (auto d = p + point{1, 0}; in_bounds(d) && tile_at_point(d).walkable() && !dist.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                std::push_heap(q.begin(), q.end(), heap_comp);
            }
            if (auto d = p + point{0, 1}; in_bounds(d) && tile_at_point(d).walkable() && !dist.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                std::push_heap(q.begin(), q.end(), heap_comp);
            }
        }

        for (int i = 0; i < r.paths.size(); ++i) {
            auto& path = r.paths[i];
            path.resize(1 + dist[path.front()]);
            std::swap(path.front(), path.back());
            int j = path.size() - 1;
            point init_point = prev[path[path.size() - 1]];
            for (point p = init_point; p != from; p = prev[p]) {
                path[--j] = p;
            }
            path[0] = from;
        }

        // end of pathfinding TODO refactor
        return r;
    }

    struct path_results {
        std::vector<point> points;
        bool success;
        point destination() const {
            return points.back();
        }
    };

    path_results shortest_path(point from, point to) {
        // determine which destinations are actually reachable
        std::map<point, int> dist;
        std::set<point> explored;
        std::deque<point> q;
        std::map<point, point> prev;

        const int MAX_DIST = std::numeric_limits<int>::max();

        q.push_back(from);
        dist[from] = 0;

        int cost = 0;
        bool goal_found = false;

        while (q.size() > 0) {
            std::sort(q.begin(), q.end(), [&dist](const point& a, const point& b) {
                return dist[a] < dist[b];
            });
            point p = q.front();
            q.pop_front();

            if (p == to) {
                cost = dist[p];
                goal_found = true;
                break;
            }

            // examine walkable neighbors and assign a distance
            if (auto d = p - point{0, 1}; in_bounds(d) && tile_at_point(d).walkable() && !explored.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                explored.insert(d);
            }
            if (auto d = p - point{1, 0}; in_bounds(d) && tile_at_point(d).walkable() && !explored.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                explored.insert(d);
            }
            if (auto d = p + point{1, 0}; in_bounds(d) && tile_at_point(d).walkable() && !explored.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                explored.insert(d);
            }
            if (auto d = p + point{0, 1}; in_bounds(d) && tile_at_point(d).walkable() && !explored.count(d)) {
                dist[d] = 1 + dist[p];
                prev[d] = p;
                q.push_back(d);
                explored.insert(d);
            }
        }

        path_results r;
        r.success = goal_found;
        if (goal_found) {
            for (point p = to; p != from; p = prev[p]) {
                r.points.push_back(p);
            }
            r.points.push_back(from);
            std::reverse(r.points.begin(), r.points.end());
        }
        // end of pathfinding TODO refactor
        return r;
    }

    bool check_win(win_state& wstate) {
        wstate.goblins_alive = 0;
        wstate.elves_alive = 0;
        wstate.remaining_hp = 0;
        wstate.turns = turns;
        wstate.elf_attack = elf_attack;
        for (int i = 0; i < actors.size(); ++i) {
            if (!actors[i].alive())
                continue;
            if (actors[i].ty == actor::elf)
                ++wstate.elves_alive;
            else if (actors[i].ty == actor::goblin)
                ++wstate.goblins_alive;
            wstate.remaining_hp += actors[i].hp;
        }

        return wstate.win();
    }

    void make_move(int actor_id) {
        std::vector<point> possible_destinations;
        get_possible_destinations(actor_id, possible_destinations);

        // std::cout << "Move " << actor_id << std::endl;

        path_results2 destinations = shortest_path2(actors[actor_id].position, possible_destinations);

        // no moves
        if (destinations.paths.size() == 0)
            return;

        std::sort(destinations.paths.begin(), destinations.paths.end(), [](auto&& d1, auto&& d2) {
            return d1.size() < d2.size();
        });
        int target_size = destinations.paths[0].size();
        int i;
        for (i = 0; i < destinations.paths[0].size() && destinations.paths[i].size() == target_size; ++i)
            ;

        auto reading_order_first = destinations.paths.begin();
        auto reading_order_last = destinations.paths.begin() + i;
        std::sort(reading_order_first, reading_order_last, [](auto&& d1, auto&& d2) {
            const auto d = d1[1] - d1[0];
            return std::tie(d1.back(), d) < std::tie(d2.back(), d);
        });

        tile_at_point(actors[actor_id].position).actor_id = tile::no_actor;
        actors[actor_id].position = (*reading_order_first)[1];
        tile_at_point(actors[actor_id].position).actor_id = actor_id;
    }

    bool get_adjacent_target(int actor_id, int& target_actor_id) {
        const auto& a = actors[actor_id];

        std::vector<int> eligible_targets;

        // evaluate adjacent tiles in reading order
        if (get_actor_at(a.position - point{0, 1}, target_actor_id)) {
            if (actors[target_actor_id].ty == a.enemy_type() && actors[target_actor_id].alive())
                eligible_targets.push_back(target_actor_id);
        }
        if (get_actor_at(a.position - point{1, 0}, target_actor_id)) {
            if (actors[target_actor_id].ty == a.enemy_type() && actors[target_actor_id].alive())
                eligible_targets.push_back(target_actor_id);
        }
        if (get_actor_at(a.position + point{1, 0}, target_actor_id)) {
            if (actors[target_actor_id].ty == a.enemy_type() && actors[target_actor_id].alive())
                eligible_targets.push_back(target_actor_id);
        }
        if (get_actor_at(a.position + point{0, 1}, target_actor_id)) {
            if (actors[target_actor_id].ty == a.enemy_type() && actors[target_actor_id].alive())
                eligible_targets.push_back(target_actor_id);
        }

        if (eligible_targets.size() == 0)
            return false;

        std::sort(eligible_targets.begin(), eligible_targets.end(), [this](int i, int j) {
            return std::tie(actors[i].hp, actors[i].position) < std::tie(actors[j].hp, actors[j].position);
        });

        target_actor_id = eligible_targets[0];

        return true;
    }

    void get_possible_destinations(int actor_id, std::vector<point>& out) {
        const auto& a = actors[actor_id];
        for (int j = 0; j < actors.size(); ++j) {
            if (actor_id == j)
                continue;
            actor& b = actors[j];
            if (b.ty == a.enemy_type() && b.alive()) {
                if (moveable(b.position - point{1, 0})) {
                    out.push_back(b.position - point{1, 0});
                }
                if (moveable(b.position + point{1, 0})) {
                    out.push_back(b.position + point{1, 0});
                }
                if (moveable(b.position - point{0, 1})) {
                    out.push_back(b.position - point{0, 1});
                }
                if (moveable(b.position + point{0, 1})) {
                    out.push_back(b.position + point{0, 1});
                }
            }
        }
    }

    bool moveable(const point& coord) const {
        const tile& t = tile_at_point(coord);
        return in_bounds(coord) && t.ty == tile::floor && t.actor_id == t.no_actor;
    }

    bool get_actor_at(point p, int& actor_id) {
        // TODO: multi index actors so this isn't linear
        auto it = std::find_if(actors.begin(), actors.end(), [&p](auto&& a) {
            return a.alive() && a.position == p;
        });
        if (it != actors.end()) {
            actor_id = std::distance(actors.begin(), it);
            return true;
        } else {
            return false;
        }
    }

    // std::map<point, tile> tiles;

    tile& tile_at_point(const point& p) {
        return tile_at_point(p.x, p.y);
    }

    const tile& tile_at_point(const point& p) const {
        return tile_at_point(p.x, p.y);
    }

    tile& tile_at_point(int x, int y) {
        return tiles[y * width + x];
    }

    const tile& tile_at_point(int x, int y) const {
        return tiles[y * width + x];
    }

    bool in_bounds(const point& p) const {
        return in_bounds(p.x, p.y);
    }

    bool in_bounds(int x, int y) const {
        return x >= 0 && x < width && y >= 0 && y < height;
    }

    void draw() {
        system("cls");
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                int aid;
                if (get_actor_at(point{x, y}, aid) && actors[aid].alive()) {
                    std::cout << (actors[aid].ty == actor::elf ? 'E' : 'G');
                } else {
                    std::cout << (tile_at_point(x, y).ty == tile::floor ? '.' : '#');
                }
            }
            std::cout << std::endl;
        }
    }

    void set_elf_attack(int value) {
        elf_attack = value;
        for (int i = 0; i < actors.size(); ++i) {
            if (actors[i].ty == actor::elf)
                actors[i].attack = value;
        }
    }

    int get_elf_count() {
        int sum = 0;
        for (int i = 0; i < actors.size(); ++i) {
            if (actors[i].ty == actor::elf)
                ++sum;
        }
        return sum;
    }

    static constexpr int TILEMAP_SIZE = 1024;
    std::vector<tile> tiles;
    int width = TILEMAP_SIZE;
    int height = TILEMAP_SIZE;
    int elf_attack = 3;

    // indices in this vector are stable; actors will never be re-ordered
    std::vector<actor> actors;
    int turns = 0;
    int next_aid = 0;
};

struct world_builder {
    void add_tile(int x, int y, tile::type ty) {
        sparse_tiles.insert_or_assign(point{x, y}, tile{ty});
    }

    void add_actor(int x, int y, actor::type ty) {
        sparse_actors.insert_or_assign(point{x, y}, actor{0, ty, point{x, y}});
    }

    world create_world() const {
        auto [min, max] = std::minmax_element(sparse_tiles.begin(), sparse_tiles.end(), [](auto&& a, auto&& b) {
            return a.first < b.first;
        });
        int width = 1 + max->first.x - min->first.x;
        int height = 1 + max->first.y - min->first.y;
        world w(width, height);

        for (auto& [pos, t] : sparse_tiles) {
            w.add_tile(pos.x, pos.y, t.ty);
        }

        for (auto& [pos, a] : sparse_actors) {
            w.add_actor(pos.x, pos.y, a.ty);
        }

        return w;
    }

    std::map<point, tile> sparse_tiles;
    std::map<point, actor> sparse_actors;
};

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;
    int y = 0;
    world_builder b;
    while (std::getline(input, line)) {
        for (int x = 0; x < line.size(); ++x) {
            char ch = line[x];
            switch (ch) {
            case '#':
                b.add_tile(x, y, tile::wall);
                break;
            case '.':
                b.add_tile(x, y, tile::floor);
                break;
            case 'E':
                b.add_tile(x, y, tile::floor);
                b.add_actor(x, y, actor::elf);
                break;
            case 'G':
                b.add_tile(x, y, tile::floor);
                b.add_actor(x, y, actor::goblin);
                break;
            default:
                throw std::runtime_error("invalid char");
            }
        }
        ++y;
    }

    win_state wstate;

    std::atomic<int> elf_attack = 3;
    std::vector<std::thread> threads;
    std::vector<std::unique_ptr<win_state>> results;
    results.resize(std::thread::hardware_concurrency());
    for (int i = 0; i < std::thread::hardware_concurrency(); ++i) {
        threads.emplace_back([&, id = i]() {
            for (;;) {
                ++elf_attack;
                world w = b.create_world();
                w.set_elf_attack(elf_attack);
                int expected_remaining_elves = w.get_elf_count();

                // std::cout << "A=" << elf_attack << std::endl;
                for (;;) {
                    w.tick(wstate);
                    if (wstate.win()) {
                        if (wstate.goblins_alive == 0 && wstate.elves_alive == expected_remaining_elves) {
                            results[id] = std::make_unique<win_state>(wstate);
                            // std::cout << "ATK   " << elf_attack << std::endl;
                            // std::cout << "Turns " << wstate.turns << std::endl;
                            // std::cout << "HP    " << wstate.remaining_hp << std::endl;
                            // std::cout << "Score " << wstate.score() << std::endl;
                            return;
                        } else {
                            break;
                        }
                    }
                }
            }
        });
    }
    for (auto& t : threads)
        t.join();
    auto best_result = std::min_element(results.begin(), results.end(), [](auto&& r1, auto&& r2) {
        return r1->elf_attack < r2->elf_attack;
    });

    std::cout << (*best_result)->score() << std::endl;

    return 0;
}
