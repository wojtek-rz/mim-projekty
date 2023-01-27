#ifndef SYSTEM_HPP
#define SYSTEM_HPP

#include <exception>
#include <utility>
#include <vector>
#include <unordered_map>
#include <functional>
#include <future>
#include <iostream>
using namespace std;
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

    CoasterPager(size_t id, std::future<bool> ready_future, std::function<bool()> is_ready) {
        this->id = id;
        this->ready_future = std::move(ready_future);
        this->is_ready = std::move(is_ready);
    }

public:
    void wait() const {
        ready_future.wait();
    }

    void wait(unsigned int timeout) const {
        ready_future.wait_for(std::chrono::milliseconds(timeout));
    }

    [[nodiscard]] unsigned int getId() const {
        return id;
    }

    [[nodiscard]] bool isReady() const {
        return is_ready();
    }
};

#include <queue>

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
    explicit QueuedMachine(std::string name, std::shared_ptr<Machine> machine) : name(std::move(name)), machine(std::move(machine)), mutex(), cond(), requests(),
                                                               working(false) {};

    std::future<std::unique_ptr<Product>> getProductFuture() {
        std::future<std::unique_ptr<Product>> future;
        {
            std::unique_lock<std::mutex> lock(mutex);
            std::promise<std::unique_ptr<Product>> promise;
            future = promise.get_future();
            requests.push(std::move(promise));
            if (requests.size() == 1) {
                cond.notify_one();
            }
        }
        return future;
    };

    void returnProduct(std::unique_ptr<Product> &&product) {
        std::unique_lock<std::mutex> lock(mutex);
        machine->returnProduct(std::move(product));
    };

    void machineLoop() {
        machine->start();
        while (true) {
            {
                std::unique_lock<std::mutex> lock(mutex);
                cond.wait(lock, [this] { return !requests.empty() || !working; });
                if (!working) break;
            }
            std::unique_ptr<Product> product;
            try {
                product = machine->getProduct();
                cout << "Machine " << name << " produced product" << endl;
                std::unique_lock<std::mutex> lock(mutex);
                requests.front().set_value(std::move(product));
                requests.pop();
            }
            catch (MachineFailure &e) {
                std::unique_lock<std::mutex> lock(mutex);
                cout << "Machine " << name << " failed" << endl;
                requests.front().set_exception(std::make_exception_ptr(e));
                requests.pop();
            }

        }
    }

    void start() {
        std::unique_lock<std::mutex> lock(mutex);
        working = true;
        thread = std::thread([this] { this->machineLoop(); });
    };

    void stop() {
        {
            std::unique_lock<std::mutex> lock(mutex);
            working = false;
            cond.notify_one();
        }

        thread.join();
        machine->stop();
    };
};


struct WorkerOrderDetails {
    size_t id;
    std::vector<std::string> orders;

    std::promise<bool> ready_promise;
    std::promise<std::unique_ptr<CoasterPager>> pager_promise;
    std::promise<std::vector<std::unique_ptr<Product>>> products_promise;

    WorkerOrderDetails(size_t id, std::vector<std::string> orders) {
        this->id = id;
        this->orders = std::move(orders);
        this->ready_promise = std::promise<bool>();
        this->pager_promise = std::promise<std::unique_ptr<CoasterPager>>();
        this->products_promise = std::promise<std::vector<std::unique_ptr<Product>>>();
    }
};

struct SystemOrderDetails {
    bool ready; // ready musi być w sekcji krytycznej
    bool failed; // failed musi być w sekcji krytycznej
};

struct WorkerDetails {
    size_t id;
    unsigned int clientTimeout;
    std::unordered_map<std::string, std::shared_ptr<QueuedMachine>> machines;
    std::function<void(size_t order_id, bool failed)> order_ready_callback;
    std::function<bool(size_t worker_id)> worker_ready_callback;

    std::promise<std::shared_ptr<WorkerOrderDetails>> order_promise;
    WorkerReport report;

    WorkerDetails(size_t id,  unsigned int clientTimeout,
                  std::unordered_map<std::string,std::shared_ptr<QueuedMachine>> &machines,
                  std::function<void(size_t order_id, bool failed)> order_ready_callback,
                  std::function<bool(size_t worker_id)> worker_ready_callback) {
        this->id = id;
        this->clientTimeout = clientTimeout;
        this->machines = machines;
        this->order_ready_callback = std::move(order_ready_callback);
        this->worker_ready_callback = std::move(worker_ready_callback);
        report = WorkerReport();
    };
};

using namespace std::chrono_literals;

void workerLoop(std::shared_ptr<WorkerDetails> worker_details) {
    for (;;) {
        if (!worker_details->worker_ready_callback(worker_details->id)) break;
        std::shared_ptr<WorkerOrderDetails> order_details = worker_details->order_promise.get_future().get();
        if (order_details == nullptr) break;

        cout << "Worker " << worker_details->id << " got order " << order_details->id << endl;

        std::vector<std::future<std::unique_ptr<Product>>> futures;
        std::vector<std::unique_ptr<Product>> products;

        bool failed = false;
        for (auto & order : order_details->orders) {
            futures.push_back(worker_details->machines[order]->getProductFuture());
        }

        for (auto &future : futures) {
            try {
                products.push_back(future.get());
            }
            catch (MachineFailure &e) {
                failed = true;
                break;
            }
        }


        worker_details->order_ready_callback(order_details->id, failed);
        order_details->ready_promise.set_value(!failed);
        if (failed){
            worker_details->report.failedOrders.push_back(order_details->orders);
            order_details->products_promise.set_exception(std::make_exception_ptr(FulfillmentFailure()));
            continue;
        }

        auto future_status = order_details->pager_promise.get_future().wait_for(std::chrono::seconds(worker_details->clientTimeout));
        if (future_status == std::future_status::ready) {
            worker_details->report.collectedOrders.push_back(order_details->orders);
        } else {
            worker_details->report.abandonedOrders.push_back(order_details->orders);
        }

        order_details->products_promise.set_value(std::move(products));
    }
}


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

    std::function<void(size_t order_id, bool failed)> order_ready_callback{
            [this](size_t order_id, bool failed) {
                std::unique_lock<std::mutex> lock(mutex);
                system_orders[order_id]->ready = true;
                system_orders[order_id]->failed = failed;
            }
    };

    std::function<bool(size_t worker_id)> worker_ready_callback{
            [this](size_t worker_id) {
                std::unique_lock<std::mutex> lock(mutex);
                workers[worker_id]->order_promise = std::promise<std::shared_ptr<WorkerOrderDetails>>();
                workers_ready[worker_id] = true;
                numberOfFreeWorkers++;
                free_worker_cond.notify_one();
                cout << "Worker " << worker_id << " is ready" << endl;

                return !closed;
            }
    };
public:
    typedef std::unordered_map<std::string, std::shared_ptr<Machine>> machines_t;

    System(machines_t machines, unsigned int numberOfWorkers, unsigned int clientTimeout) {
        this->closed = false;
        this->numberOfWorkers = numberOfWorkers;
        this->numberOfFreeWorkers = 0;
        this->clientTimeout = clientTimeout;
        this->order_count = 0;

        for (auto &machine: machines) {
            this->machines[machine.first] = std::make_shared<QueuedMachine>(machine.first, machine.second);
            this->machines[machine.first]->start();
        }
        for (auto &machine: this->machines) {
            this->menu.push_back(machine.first);
        }

        for (size_t i = 0; i < numberOfWorkers; i++) {
            auto worker_details = std::make_shared<WorkerDetails>(i, clientTimeout, this->machines,  order_ready_callback,
                                                                  worker_ready_callback);
            workers.push_back(worker_details);
            workers_ready.push_back(true);
            std::thread thread(workerLoop, worker_details);
            worker_threads.push_back(std::move(thread));
        }
    }

    std::vector<WorkerReport> shutdown() {
        {
            std::unique_lock<std::mutex> lock(mutex);
            cout << "system shutdown" << endl;
            closed = true;
        }
        for (auto &worker: workers) {
            if (workers_ready[worker->id])
                worker->order_promise.set_value(nullptr);
        }

        for (auto &thread: worker_threads) {
            thread.join();
        }
        for (auto &machine: machines) {
            machine.second->stop();
        }
        std::vector<WorkerReport> reports;
        for (auto &worker: workers) {
            reports.push_back(worker->report);
        }
        return reports;
    }

    std::vector<std::string> getMenu() const {
        return this->menu;
    }

    std::vector<unsigned int> getPendingOrders() const {
        std::unique_lock<std::mutex> lock(mutex);
        std::vector<unsigned int> pending_orders;
        for (auto &order: system_orders) {
            pending_orders.push_back(order.first);
        }
        return pending_orders;
    }

    std::unique_ptr<CoasterPager> order(std::vector<std::string> products) {
        std::unique_lock<std::mutex> lock(mutex);
        if (closed){
            throw RestaurantClosedException();
        }

        const size_t id = this->order_count++;
        free_worker_cond.wait(lock, [this] { return numberOfFreeWorkers > 0; });
        cout << "Order " << this->order_count << " received, free workers: " << numberOfFreeWorkers << endl;
        numberOfFreeWorkers--;

        auto system_order = std::make_shared<SystemOrderDetails>();
        system_orders[id] = system_order;

        auto worker_order = std::make_shared<WorkerOrderDetails>(id, products);
        worker_orders[id] = worker_order;

        for (unsigned int i = 0; i < numberOfWorkers; i++) {
            if (workers_ready[i]) {
                workers_ready[i] = false;
                workers[i]->order_promise.set_value(worker_order);
                break;
            }
        }
        auto ready_future = worker_order->ready_promise.get_future();
        auto *raw_pager_ptr = new CoasterPager(worker_order->id, std::move(ready_future),
                                               [this, system_order] {
                                                   std::unique_lock<std::mutex> lock(mutex);
                                                   return system_order->ready;
                                               });
        std::unique_ptr<CoasterPager> pager_ptr(raw_pager_ptr);
        cout << "Order " << id << " given pager" << endl;
        return pager_ptr;
    }

    std::vector<std::unique_ptr<Product>> collectOrder(std::unique_ptr<CoasterPager> pager) {
        cout << "collected order: " << pager->getId() << endl;
        std::unique_lock<std::mutex> lock(mutex);
        if (system_orders.find(pager->getId()) == system_orders.end()) {
            throw BadPagerException();
        }
        if (!system_orders[pager->getId()]->ready) {
            throw OrderNotReadyException();
        }
        if (system_orders[pager->getId()]->failed) {
            throw FulfillmentFailure();
        }

        auto worker_order = worker_orders[pager->getId()];
        worker_orders.erase(pager->getId());
        worker_order->pager_promise.set_value(std::move(pager));

        auto products = worker_order->products_promise.get_future().get();
        return products;
    }

    unsigned int getClientTimeout() const {
        return clientTimeout;
    }
};

#endif // SYSTEM_HPP