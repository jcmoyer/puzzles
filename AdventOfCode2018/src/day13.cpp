#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <thread>
#include <vector>

struct point {
    int x, y;
};
bool operator==(const point& a, const point& b) {
    return std::tie(a.y, a.x) == std::tie(b.y, b.x);
}
bool operator<(const point& a, const point& b) {
    return std::tie(a.y, a.x) < std::tie(b.y, b.x);
}
point operator+(const point& a, const point& b) {
    return {a.x + b.x, a.y + b.y};
}
point& operator+=(point& a, const point& b) {
    a.x += b.x;
    a.y += b.y;
    return a;
}
std::ostream& operator<<(std::ostream& s, const point& a) {
    return s << a.x << ',' << a.y;
}

enum class facing { up, right, down, left };

point directional_point(facing f) {
    switch (f) {
    case facing::left:
        return {-1, 0};
    case facing::up:
        return {0, -1};
    case facing::right:
        return {1, 0};
    case facing::down:
        return {0, 1};
    default:
        return {0, 0};
    }
}

facing rotate_facing_ccw(facing f, int n = 1) {
    for (int i = 0; i < n; ++i) {
        switch (f) {
        case facing::up:
            f = facing::left;
            break;
        case facing::right:
            f = facing::up;
            break;
        case facing::down:
            f = facing::right;
            break;
        case facing::left:
            f = facing::down;
            break;
        }
    }
    return f;
}

struct tile {
    enum type {
        horizontal,
        vertical,
        cross,
        corner_tl,
        corner_tr,
        corner_bl,
        corner_br,
        indeterminate_ru_ld,
        indeterminate_rd_lu
    };
    type ty;

    char to_char() const {
        switch (ty) {
        case horizontal:
            return '-';
        case vertical:
            return '|';
        case cross:
            return '+';
        case corner_br:
        case corner_tl:
            return '/';
        case corner_bl:
        case corner_tr:
            return '\\';
        default:
            throw "unknown type";
        }
    }

    bool is_corner() const {
        return ty == corner_tl || ty == corner_tr || ty == corner_bl || ty == corner_br;
    }

    facing out_direction(facing in_direction) const {
        switch (ty) {
        case horizontal:
        case vertical:
            return in_direction;
        case corner_tl:
            if (in_direction == facing::up)
                return facing::right;
            else if (in_direction == facing::left)
                return facing::down;
            throw std::runtime_error("tile does not support given direction");
        case corner_tr:
            if (in_direction == facing::up)
                return facing::left;
            else if (in_direction == facing::right)
                return facing::down;
            throw std::runtime_error("tile does not support given direction");
        case corner_bl:
            if (in_direction == facing::down)
                return facing::right;
            else if (in_direction == facing::left)
                return facing::up;
            throw std::runtime_error("tile does not support given direction");
        case corner_br:
            if (in_direction == facing::down)
                return facing::left;
            else if (in_direction == facing::right)
                return facing::up;
            throw std::runtime_error("tile does not support given direction");
        default:
            throw std::runtime_error("tile type not supported");
        }
    }
};

class cart {
public:
    cart(const point& p, facing f) : position{p}, f{f}, init_position{p}, init_facing{f} {}

    point position;
    facing f;

    point init_position;
    facing init_facing;

    bool alive = true;

    void reset() {
        position = init_position;
        f = init_facing;
        turn = turn_left;
        alive = true;
    }

    void perform_next_turn() {
        switch (turn) {
        case turn_left:
            f = rotate_facing_ccw(f, 1);
            break;
        case turn_straight:
            break;
        case turn_right:
            f = rotate_facing_ccw(f, 3);
            break;
        }
        turn = next_turn_state(turn);
    }

private:
    enum turn_state { turn_left, turn_straight, turn_right };
    turn_state turn = turn_left;
    static turn_state next_turn_state(turn_state s) {
        switch (s) {
        case turn_left:
            return turn_straight;
        case turn_straight:
            return turn_right;
        case turn_right:
            return turn_left;
        }
    }
};

void apply_tile(cart& c, const tile& t) {
    if (t.ty == tile::cross) {
        c.perform_next_turn();
    } else {
        c.f = t.out_direction(c.f);
    }
}

class step_context {
public:
    step_context() {}
    void stop() {
        _stopped = true;
    }
    bool stopped() const {
        return _stopped;
    }

private:
    bool _stopped = false;
};

struct step_event_handler {
    virtual void handle_collision(step_context& ctx, const cart&, const cart&){};
    virtual void handle_last_cart(step_context& ctx, const cart&){};
};

enum class step_result { ok, stopped };

struct world {
    void add_tile(int x, int y, tile::type t) {
        tiles[point{x, y}] = tile{t};
    }

    void add_cart(int x, int y, facing f) {
        carts.emplace_back(point{x, y}, f);
    }

    // processes the tiles and turns indeterminate ones into correct ones depending on their neighboring tiles
    void postprocess() {
        for (auto& [p, t] : tiles) {
            if (t.ty == tile::indeterminate_ru_ld) {
                auto left = tiles.find(p + point{-1, 0});
                auto up = tiles.find(p + point{0, -1});
                if (left != tiles.end() && up != tiles.end() &&
                    (left->second.ty == tile::horizontal || left->second.ty == tile::cross) &&
                    (up->second.ty == tile::vertical || up->second.ty == tile::cross)) {
                    t.ty = tile::corner_br;
                    continue;
                }

                auto right = tiles.find(p + point{1, 0});
                auto down = tiles.find(p + point{0, 1});
                if (right != tiles.end() && down != tiles.end() &&
                    (right->second.ty == tile::horizontal || right->second.ty == tile::cross) &&
                    (down->second.ty == tile::vertical || down->second.ty == tile::cross)) {
                    t.ty = tile::corner_tl;
                    continue;
                }

                throw std::runtime_error("could not determine tile type from surrounding tiles");
            } else if (t.ty == tile::indeterminate_rd_lu) {
                auto right = tiles.find(p + point{1, 0});
                auto up = tiles.find(p + point{0, -1});
                if (right != tiles.end() && up != tiles.end() &&
                    (right->second.ty == tile::horizontal || right->second.ty == tile::cross) &&
                    (up->second.ty == tile::vertical || up->second.ty == tile::cross)) {
                    t.ty = tile::corner_bl;
                    continue;
                }

                auto left = tiles.find(p + point{-1, 0});
                auto down = tiles.find(p + point{0, 1});
                if (left != tiles.end() && down != tiles.end() &&
                    (left->second.ty == tile::horizontal || left->second.ty == tile::cross) &&
                    (down->second.ty == tile::vertical || down->second.ty == tile::cross)) {
                    t.ty = tile::corner_tr;
                    continue;
                }

                throw std::runtime_error("could not determine tile type from surrounding tiles");
            }
        }
    }

    step_result step(step_event_handler& handler) {
        step_result r = step_result::ok;
        step_context ctx;

        std::sort(carts.begin(), carts.end(), [](auto&& x, auto&& y) {
            return x.position < y.position;
        });

        for (int i = 0; i < carts.size(); ++i) {
            if (!carts[i].alive)
                continue;
            step_cart(carts[i]);
            if (int j; check_cart_collisions(i, j)) {
                carts[i].alive = false;
                carts[j].alive = false;
                handler.handle_collision(ctx, carts[i], carts[j]);
            }
        }

        int num_alive = std::count_if(carts.begin(), carts.end(), [](auto&& c) {
            return c.alive;
        });

        if (num_alive == 1) {
            auto it = std::find_if(carts.begin(), carts.end(), [](auto&& c) {
                return c.alive;
            });
            handler.handle_last_cart(ctx, *it);
        }

        if (ctx.stopped())
            r = step_result::stopped;

        return r;
    }

    void reset() {
        for (cart& c : carts) {
            c.reset();
        }
    }

    std::map<point, tile> tiles;
    std::vector<cart> carts;

private:
    void step_cart(cart& c) {
        c.position += directional_point(c.f);
        tile& t = tiles.at(c.position);
        apply_tile(c, t);
    }

    // true if collided
    bool check_cart_collisions(int vs, int& other) {
        if (!carts[vs].alive)
            throw std::runtime_error("first cart should be alive");
        for (other = 0; other < carts.size(); ++other) {
            if (!carts[other].alive)
                continue;
            if (other == vs)
                continue;
            if (carts[vs].position == carts[other].position) {
                return true;
            }
        }
        return false;
    }
};

struct part1_handler : step_event_handler {
    void handle_collision(step_context& ctx, const cart& x, const cart& y) override {
        std::cout << x.position << std::endl;
        ctx.stop();
    }
};

struct part2_handler : step_event_handler {
    void handle_last_cart(step_context& ctx, const cart& x) override {
        std::cout << x.position << std::endl;
        ctx.stop();
    }
};

#include <Windows.h>
void visualize(world& w) {
    HANDLE hstdout = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE hbuffer = CreateConsoleScreenBuffer(GENERIC_READ | // read/write access
                                                   GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE, // shared
        NULL,                               // default security attributes
        CONSOLE_TEXTMODE_BUFFER,            // must be TEXTMODE
        NULL);                              // reserved; must be NULL
    SetConsoleActiveScreenBuffer(hbuffer);
    CHAR_INFO cbuffer[80 * 25];
    memset(cbuffer, 0, sizeof(CHAR_INFO) * 80 * 25);
    for (int i = 0; i < 80 * 25; ++i) {
        cbuffer[i].Attributes = FOREGROUND_GREEN;
    }
    COORD cbuffer_size;
    cbuffer_size.X = 80;
    cbuffer_size.Y = 25;
    COORD cbuffer_dst;
    cbuffer_dst.X = 0;
    cbuffer_dst.Y = 0;
    SMALL_RECT writerect;
    writerect.Left = 0;
    writerect.Top = 0;
    writerect.Bottom = 24;
    writerect.Right = 79;

    //{x=133 y=102 }
    // auto cart_id = w.carts[1].init_position;
    auto cart_id = std::find_if(w.carts.begin(), w.carts.end(), [](auto&& c) {
        return c.init_position == point{133, 102};
    })->position;
    int steps = 0;
    part2_handler h;
    while (w.step(h) == step_result::ok) {
        ++steps;
        auto attached_cart = std::find_if(w.carts.begin(), w.carts.end(), [&cart_id](auto&& c) {
            return c.init_position == cart_id;
        });
        for (int y = -12; y <= +12; ++y) {
            for (int x = -39; x <= +40; ++x) {

                int dx = x + 39;
                int dy = y + 12;

                auto p = point{attached_cart->position.x + x, attached_cart->position.y + y};
                auto t = w.tiles.find(p);

                cart* cart_here = nullptr;
                if (attached_cart->position == p)
                    cart_here = &*attached_cart;
                if (!cart_here) {
                    auto it = std::find_if(w.carts.begin(), w.carts.end(), [&p, &attached_cart, x, y](auto&& c) {
                        return c.position == attached_cart->position + point{x, y};
                    });
                    if (it != w.carts.end()) {
                        cart_here = &*it;
                    }
                }

                if (t != w.tiles.end()) {
                    if (cart_here) {
                        if (cart_here->alive) {
                            cbuffer[dy * 80 + dx].Char.AsciiChar = '\xdb';
                            cbuffer[dy * 80 + dx].Attributes = FOREGROUND_GREEN | FOREGROUND_INTENSITY;
                        } else {
                            cbuffer[dy * 80 + dx].Char.AsciiChar = '\xb1';
                            cbuffer[dy * 80 + dx].Attributes = FOREGROUND_RED | FOREGROUND_INTENSITY;
                        }
                    } else {
                        cbuffer[dy * 80 + dx].Char.AsciiChar = t->second.to_char();
                        if (t->second.ty == tile::cross) {
                            cbuffer[dy * 80 + dx].Attributes = FOREGROUND_BLUE | FOREGROUND_INTENSITY;
                        } else {
                            cbuffer[dy * 80 + dx].Attributes = FOREGROUND_INTENSITY;
                        }
                    }
                } else {
                    cbuffer[dy * 80 + dx].Char.AsciiChar = ' ';
                }
            }
        }

        DWORD succ = WriteConsoleOutput(hbuffer, // screen buffer to write to
            cbuffer,                             // buffer to copy from
            cbuffer_size,                        // col-row size of chiBuffer
            cbuffer_dst,                         // top left src cell in chiBuffer
            &writerect);                         // dest. screen buffer rectangle

        std::this_thread::sleep_for(std::chrono::milliseconds(20));
    }
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;

    world w;
    int y = 0;

    while (std::getline(input, line)) {
        for (int x = 0; x < line.size(); ++x) {
            char ch = line[x];
            switch (ch) {
            case '-':
                w.add_tile(x, y, tile::horizontal);
                break;
            case '|':
                w.add_tile(x, y, tile::vertical);
                break;
            case '+':
                w.add_tile(x, y, tile::cross);
                break;
            case '/':
                w.add_tile(x, y, tile::indeterminate_ru_ld);
                break;
            case '\\':
                w.add_tile(x, y, tile::indeterminate_rd_lu);
                break;
            case '^':
                w.add_cart(x, y, facing::up);
                w.add_tile(x, y, tile::vertical);
                break;
            case '>':
                w.add_cart(x, y, facing::right);
                w.add_tile(x, y, tile::horizontal);
                break;
            case 'v':
                w.add_cart(x, y, facing::down);
                w.add_tile(x, y, tile::vertical);
                break;
            case '<':
                w.add_cart(x, y, facing::left);
                w.add_tile(x, y, tile::horizontal);
                break;
            case ' ':
                break;
            default:
                throw std::runtime_error("unhandled character");
            }
        }
        ++y;
    }
    w.postprocess();

    part1_handler h1;
    while (w.step(h1) == step_result::ok) {
    }
    w.reset();
    part2_handler h2;
    while (w.step(h2) == step_result::ok) {
    }

    return 0;
}
