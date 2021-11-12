#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <string>
#include <vector>

using task_id = char;

struct task {
    task_id id;
    std::vector<task_id> dependencies;
    std::vector<task_id> dependents;

    int total_time() const {
        return 60 + 1 + id - 'A';
    }
};

struct elf {
    static constexpr task_id no_task = -1;

    task_id task = no_task;
    int time_spent = 0;

    void work() {
        ++time_spent;
    }

    bool has_task() const {
        return task != no_task;
    }

    void clear_task() {
        task = no_task;
        time_spent = 0;
    }
};

class elf_team {
public:
    elf_team(int team_size) : elves(team_size) {}

    elf* get_available_elf() {
        auto it = std::find_if(elves.begin(), elves.end(), [](elf& e) {
            return !e.has_task();
        });
        if (it != elves.end()) {
            return &*it;
        } else {
            return nullptr;
        }
    }

    void work() {
        for (elf& e : elves) {
            if (e.has_task())
                e.work();
        }
    }

    bool has_work() const {
        return std::any_of(elves.begin(), elves.end(), [](const elf& e) {
            return e.has_task();
        });
    }

    auto begin() {
        return elves.begin();
    }
    auto end() {
        return elves.end();
    }

private:
    std::vector<elf> elves;
};

class dependency_graph {
public:
    void add_dependency(task_id dependency, task_id dependent) {
        graph[dependency].id = dependency;
        graph[dependency].dependents.push_back(dependent);
        graph[dependent].id = dependent;
        graph[dependent].dependencies.push_back(dependency);
    }

    int compute_multitask_time(int num_elves) {
        auto node_state = create_search_state();
        task_seq next_steps;
        get_zero_outdegrees(next_steps);
        int time_spent = 0;
        elf_team elves(num_elves);
        while (next_steps.size() > 0 || elves.has_work()) {
            // distribute available tasks to available elves
            elf* next_idle_elf;
            while (next_steps.size() > 0 && (next_idle_elf = elves.get_available_elf())) {
                task_id next = next_steps.back();
                next_steps.pop_back();
                next_idle_elf->task = next;
            }
            elves.work();
            ++time_spent;
            // look for finished elves
            for (elf& e : elves) {
                auto& n = graph[e.task];
                if (e.has_task() && e.time_spent >= n.total_time()) {
                    node_state[n.id].complete = true;
                    e.clear_task();
                    get_ready_dependents(n, node_state, next_steps);
                }
            }
        }
        return time_spent;
    }

    std::string compute_task_order() {
        auto node_state = create_search_state();
        task_seq ready_steps;
        get_zero_outdegrees(ready_steps);
        std::string order;
        while (ready_steps.size() > 0) {
            // tasks MUST be completed in alphabetical order no matter what the
            // dependent nodes are - here the tasks are sorted in reverse order
            // since we want to pop from the back of the sequence
            std::sort(ready_steps.rbegin(), ready_steps.rend());
            task_id next = ready_steps.back();
            auto& n = graph[next];
            node_state[next].complete = true;
            order += next;
            ready_steps.pop_back();
            get_ready_dependents(n, node_state, ready_steps);
        }
        return order;
    }

private:
    struct node_traversal_state {
        bool complete = false;
    };
    using search_state = std::map<task_id, node_traversal_state>;
    using task_seq = std::vector<task_id>;

    search_state create_search_state() const {
        search_state st;
        for (const auto& [k, v] : graph) {
            st[k] = {false};
        }
        return st;
    }

    // examines eligible dependents and places them into tasks
    void get_ready_dependents(const task& n, search_state& node_state, task_seq& tasks) {
        for (task_id dependent_id : n.dependents) {
            auto& dependent = graph[dependent_id];
            bool all_ready =
                std::all_of(dependent.dependencies.begin(), dependent.dependencies.end(), [&node_state](task_id dep) {
                    return node_state[dep].complete;
                });
            // if all dependencies have completed, this task is ready to be processed
            if (all_ready) {
                tasks.push_back(dependent.id);
            }
        }
    }

    void get_zero_outdegrees(task_seq& next_steps) const {
        for (auto& [id, n] : graph) {
            if (n.dependencies.size() == 0) {
                next_steps.push_back(id);
            }
        }
    }

    std::map<task_id, task> graph;
};

void load_dependencies(std::istream& input, dependency_graph& dg) {
    // this looks for the two standalone characters surrounded by spaces within each line
    std::string line;
    std::regex task_name(R"([Ss]tep (\w))");
    while (std::getline(input, line)) {
        std::cmatch m;
        char t0, t1;
        if (std::regex_search(line.data(), m, task_name)) {
            t0 = *m[1].first;
            if (std::regex_search(line.data() + m.position() + 1, m, task_name)) {
                t1 = *m[1].first;
            } else {
                throw "malformed input";
            }
        } else {
            throw "malformed input";
        }
        dg.add_dependency(t0, t1);
    }
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    dependency_graph dg;
    load_dependencies(input, dg);
    std::cout << dg.compute_task_order() << std::endl;
    std::cout << dg.compute_multitask_time(5) << std::endl;
    return 0;
}
