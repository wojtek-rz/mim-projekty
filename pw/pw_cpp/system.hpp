#ifndef SYSTEM_HPP
#define SYSTEM_HPP

#include <exception>
#include <vector>
#include <unordered_map>
#include <functional>
#include <future>
#include <queue>
#include <algorithm>
#include "machine.hpp"

class FulfillmentFailure : public std::exception {
};

class OrderNotReadyException : public std::exception {
};

class BadOrderException : public std::exception {
};

class BadPagerException : public std::exception {
};

class OrderExpiredException : public std::exception {
};

class RestaurantClosedException : public std::exception {
};

struct WorkerReport {
    std::vector<std::vector<std::string>> collectedOrders;
    std::vector<std::vector<std::string>> abandonedOrders;
    std::vector<std::vector<std::string>> failedOrders;
    std::vector<std::string> failedProducts;
};

class CoasterPager {
    friend class System;

private:
    size_t id;
    std::future<bool> ready_future;
    std::function<bool()> is_ready;

    CoasterPager(size_t id, std::future<bool> ready_future, std::function<bool()> is_ready);

public:
    void wait() const;

    void wait(unsigned int timeout) const;

    [[nodiscard]] unsigned int getId() const;

    [[nodiscard]] bool isReady() const;
};

class QueuedMachine {
private:
    std::string name;
    std::shared_ptr<Machine> machine;
    std::mutex mutex;
    std::condition_variable cond;
    std::queue<std::promise<std::unique_ptr<Product>>> requests;
    bool working;
    std::thread thread;
public:
    explicit QueuedMachine(std::string name, std::shared_ptr<Machine> machine);

    std::future<std::unique_ptr<Product>> getProductFuture();

    void returnProduct(std::unique_ptr<Product> &&product);

    void machineLoop();

    void start();

    void stop();
};

struct WorkerOrderDetails {
    size_t id;
    std::vector<std::string> orders;

    std::promise<bool> ready_promise;
    std::promise<std::unique_ptr<CoasterPager>> pager_promise;
    std::promise<std::vector<std::unique_ptr<Product>>> products_promise;

    WorkerOrderDetails(size_t id, std::vector<std::string> orders);
};

struct SystemOrderDetails { // znajduje się w sekcji krytycznej
    bool ready;
    bool failed;
    bool expired;
    SystemOrderDetails();
};

struct WorkerDetails {
    size_t id;
    unsigned int clientTimeout;
    std::unordered_map<std::string, std::shared_ptr<QueuedMachine>> machines;
    std::function<void(size_t order_id, bool failed, bool expired)> order_ready_callback;
    std::function<void(std::vector<std::string> names)> machine_failure_callback;
    std::function<bool(size_t worker_id)> worker_ready_callback;

    std::promise<std::shared_ptr<WorkerOrderDetails>> order_promise;
    WorkerReport report;

    WorkerDetails(size_t id,  unsigned int clientTimeout,
                  std::unordered_map<std::string,std::shared_ptr<QueuedMachine>> &machines,
                  std::function<void(size_t order_id, bool failed, bool expired)> order_ready_callback,
                  std::function<void(std::vector<std::string> names)> machine_failure_callback,
                  std::function<bool(size_t worker_id)> worker_ready_callback);
};

class System {
private:
    unsigned int numberOfWorkers;
    unsigned int numberOfFreeWorkers;
    unsigned int clientTimeout;
    bool closed;
    size_t order_count;
    std::unordered_map<std::string, std::shared_ptr<QueuedMachine>> machines;

    mutable std::mutex mutex;
    std::condition_variable free_worker_cond;
    std::vector<std::shared_ptr<WorkerDetails>> workers;
    std::vector<bool> workers_ready;
    std::vector<std::thread> worker_threads;

    std::unordered_map<size_t, std::shared_ptr<WorkerOrderDetails>> worker_orders;
    std::unordered_map<size_t, std::shared_ptr<SystemOrderDetails>> system_orders;
    std::vector<std::string> menu;

    std::function<void(size_t order_id, bool failed, bool expired)> order_ready_callback{
            [this](size_t order_id, bool failed, bool expired) {
                std::unique_lock<std::mutex> lock(mutex);
                system_orders[order_id]->ready = true;
                system_orders[order_id]->failed = failed;
                system_orders[order_id]->expired = expired;
            }
    };
    std::function<void(std::vector<std::string> names)> machine_failure_callback{
        [this](std::vector<std::string> names) {
            std::unique_lock<std::mutex> lock(mutex);
            for (auto &name: names) {
                menu.erase(std::remove(menu.begin(), menu.end(), name), menu.end());
            }
        }
    };
    std::function<bool(size_t worker_id)> worker_ready_callback{
            [this](size_t worker_id) {
                std::unique_lock<std::mutex> lock(mutex);
                workers[worker_id]->order_promise = std::promise<std::shared_ptr<WorkerOrderDetails>>();
                workers_ready[worker_id] = true;
                numberOfFreeWorkers++;
                free_worker_cond.notify_one();

                return !closed;
            }
    };
public:
    typedef std::unordered_map<std::string, std::shared_ptr<Machine>> machines_t;

    System(machines_t machines, unsigned int numberOfWorkers, unsigned int clientTimeout);

    std::vector<WorkerReport> shutdown();

    std::vector<std::string> getMenu() const;

    std::vector<unsigned int> getPendingOrders() const;

    std::unique_ptr<CoasterPager> order(std::vector<std::string> products);

    std::vector<std::unique_ptr<Product>> collectOrder(std::unique_ptr<CoasterPager> CoasterPager);

    unsigned int getClientTimeout() const;
};

#endif // SYSTEM_HPP