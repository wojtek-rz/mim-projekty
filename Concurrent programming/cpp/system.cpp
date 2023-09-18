#include <set>
#include <iterator>
#include "system.hpp"

CoasterPager::CoasterPager(size_t id, std::future<bool> ready_future, std::function<bool()> is_ready) {
    this->id = id;
    this->ready_future = std::move(ready_future);
    this->is_ready = std::move(is_ready);
}

void CoasterPager::wait() const {
    ready_future.wait();
}

void CoasterPager::wait(unsigned int timeout) const {
    ready_future.wait_for(std::chrono::milliseconds(timeout));
}

[[nodiscard]] unsigned int CoasterPager::getId() const {
    return id;
}

[[nodiscard]] bool CoasterPager::isReady() const {
    return is_ready();
}

QueuedMachine::QueuedMachine(std::string name, std::shared_ptr<Machine> machine) : name(std::move(name)),
                                                                                   machine(std::move(machine)), mutex(),
                                                                                   cond(), requests(),
                                                                                   working(false) {}

std::future<std::unique_ptr<Product>> QueuedMachine::getProductFuture() {
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
}

void QueuedMachine::returnProduct(std::unique_ptr<Product> &&product) {
    std::unique_lock<std::mutex> lock(mutex);
    machine->returnProduct(std::move(product));
}

void QueuedMachine::machineLoop() {
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
            std::unique_lock<std::mutex> lock(mutex);
            requests.front().set_value(std::move(product));
            requests.pop();
        }
        catch (MachineFailure &e) {
            std::unique_lock<std::mutex> lock(mutex);
            requests.front().set_exception(std::make_exception_ptr(e));
            requests.pop();
        }

    }
}

void QueuedMachine::start() {
    std::unique_lock<std::mutex> lock(mutex);
    working = true;
    thread = std::thread([this] { this->machineLoop(); });
}

void QueuedMachine::stop() {
    {
        std::unique_lock<std::mutex> lock(mutex);
        working = false;
        cond.notify_one();
    }

    thread.join();
    machine->stop();
}


WorkerOrderDetails::WorkerOrderDetails(size_t id, std::vector<std::string> orders) {
    this->id = id;
    this->orders = std::move(orders);
    this->ready_promise = std::promise<bool>();
    this->pager_promise = std::promise<std::unique_ptr<CoasterPager>>();
    this->products_promise = std::promise<std::vector<std::unique_ptr<Product>>>();
}

SystemOrderDetails::SystemOrderDetails() {
    ready = false;
    failed = false;
    expired = false;
}

WorkerDetails::WorkerDetails(size_t id, unsigned int clientTimeout,
                             std::unordered_map<std::string, std::shared_ptr<QueuedMachine>> &machines,
                             std::function<void(size_t order_id, bool failed, bool expired)> order_ready_callback,
                             std::function<void(std::vector<std::string> names)> machine_failure_callback,
                             std::function<bool(size_t worker_id)> worker_ready_callback) {
    this->id = id;
    this->clientTimeout = clientTimeout;
    this->machines = machines;
    this->order_ready_callback = std::move(order_ready_callback);
    this->machine_failure_callback = std::move(machine_failure_callback);
    this->worker_ready_callback = std::move(worker_ready_callback);
    report = WorkerReport();
}

std::vector<std::string> sumVectorsWithoutRepetitions(std::vector<std::string> &a, std::vector<std::string> &b) {
    std::vector<std::string> result;
    std::set_union(a.begin(), a.end(), b.begin(), b.end(), std::back_inserter(result));
    return result;
}

void workerLoop(std::shared_ptr<WorkerDetails> worker_details) {
    for (;;) {
        if (!worker_details->worker_ready_callback(worker_details->id)) break;
        std::shared_ptr<WorkerOrderDetails> order_details = worker_details->order_promise.get_future().get();
        if (order_details == nullptr) break;

//        std::cout << "Worker " << worker_details->id << " got order " << order_details->id << std::endl;

        std::vector<std::future<std::unique_ptr<Product>>> futures;
        std::vector<std::unique_ptr<Product>> products;

        for (auto &order: order_details->orders) {
            futures.push_back(worker_details->machines[order]->getProductFuture());
        }

        std::vector<std::string> failed_machines;
        for (size_t i = 0; i < order_details->orders.size(); i++) {
            try {
                products.push_back(futures[i].get());
            } catch (MachineFailure &e) {
                failed_machines.push_back(order_details->orders[i]);
            }
        }
        bool failed = !failed_machines.empty();

        worker_details->machine_failure_callback(failed_machines);
        worker_details->order_ready_callback(order_details->id, failed, false);
        order_details->ready_promise.set_value(!failed);

        if (failed) {
            worker_details->report.failedOrders.push_back(order_details->orders);
            worker_details->report.failedProducts = sumVectorsWithoutRepetitions(
                    worker_details->report.failedProducts, failed_machines);
            order_details->products_promise.set_exception(std::make_exception_ptr(FulfillmentFailure()));
            continue;
        }

        auto future_status = order_details->pager_promise.get_future().wait_for(
                std::chrono::seconds(worker_details->clientTimeout));
        if (future_status == std::future_status::ready) {
            worker_details->report.collectedOrders.push_back(order_details->orders);
        } else {
            for (size_t i = 0; i < order_details->orders.size(); i++) {
                worker_details->machines[order_details->orders[i]]->returnProduct(std::move(products[i]));
            }

            worker_details->report.abandonedOrders.push_back(order_details->orders);
            worker_details->order_ready_callback(order_details->id, failed, true);
        }

        order_details->products_promise.set_value(std::move(products));
    }
}

System::System(machines_t machines, unsigned int numberOfWorkers, unsigned int clientTimeout) {
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
        auto worker_details = std::make_shared<WorkerDetails>(i, clientTimeout, this->machines,
                                                              order_ready_callback,
                                                              machine_failure_callback, worker_ready_callback);
        workers.push_back(worker_details);
        workers_ready.push_back(true);
        std::thread thread(workerLoop, worker_details);
        worker_threads.push_back(std::move(thread));
    }
}

std::vector<WorkerReport> System::shutdown() {
    {
        std::unique_lock<std::mutex> lock(mutex);
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

std::vector<std::string> System::getMenu() const {
    std::unique_lock<std::mutex> lock(mutex);
    return this->menu;
}

std::vector<unsigned int> System::getPendingOrders() const {
    std::unique_lock<std::mutex> lock(mutex);
    std::vector<unsigned int> pending_orders;
    for (auto &order: system_orders) {
        if (!order.second->expired && !order.second->failed) {
            pending_orders.push_back(order.first);
        }
    }
    return pending_orders;
}

std::unique_ptr<CoasterPager> System::order(std::vector<std::string> products) {
    std::unique_lock<std::mutex> lock(mutex);
    if (closed) {
        throw RestaurantClosedException();
    }

    const size_t id = this->order_count++;
    free_worker_cond.wait(lock, [this] { return numberOfFreeWorkers > 0; });
//        std::cout << "Order " << this->order_count << " received, free workers: " << numberOfFreeWorkers << std::endl;
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
    return pager_ptr;
}

std::vector<std::unique_ptr<Product>> System::collectOrder(std::unique_ptr<CoasterPager> pager) {
//        std::cout << "collected order: " << pager->getId() << std::endl;
    std::unique_lock<std::mutex> lock(mutex);
    if (!pager || system_orders.find(pager->getId()) == system_orders.end()) {
        throw BadPagerException();
    }
    if (!system_orders[pager->getId()]->ready) {
        throw OrderNotReadyException();
    }
    if (system_orders[pager->getId()]->failed) {
        throw FulfillmentFailure();
    }
    if (system_orders[pager->getId()]->expired) {
        throw OrderExpiredException();
    }

    auto worker_order = worker_orders[pager->getId()];
    worker_orders.erase(pager->getId());
    worker_order->pager_promise.set_value(std::move(pager));

    auto products = worker_order->products_promise.get_future().get();
    return products;
}

unsigned int System::getClientTimeout() const {
    return clientTimeout;
}