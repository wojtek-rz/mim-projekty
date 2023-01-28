#include <barrier>
#include "my_tests.h"

bool productsMatchOrder(std::vector<std::unique_ptr<Product>> products, std::vector<std::string> order){
    std::vector<std::string> products_names;
    for (auto &product : products) {
        products_names.push_back(dynamic_cast<CustomProduct *>(product.get())->getName());
    }
    std::sort(products_names.begin(), products_names.end());
    std::sort(order.begin(), order.end());

    return products_names == order;
}

void printReports(const std::vector<WorkerReport> &reports){
    for (auto &report : reports) {
        std::cout << std::endl << "WORKER REPORT: " << std::endl;
        std::cout << report.collectedOrders.size()  << " collected orders: ";
        for (auto &report_orders : report.collectedOrders) {
            std::cout << "[";
            for (auto &order : report_orders) {
                std::cout << order << " ";
            }
            std::cout << "], ";
        }
        std::cout << std::endl << report.failedOrders.size() <<  " failed orders: ";
        for (auto &report_orders : report.failedOrders) {
            std::cout << "[";
            for (auto &order : report_orders) {
                std::cout << order << " ";
            }
            std::cout << "], ";
        }
        std::cout << std::endl << report.abandonedOrders.size() << " abandoned orders: ";
        for (auto &report_orders : report.abandonedOrders) {
            std::cout << "[";
            for (auto &order : report_orders) {
                std::cout << order << " ";
            }
            std::cout << "], ";
        }
        std::cout << std::endl << report.failedProducts.size() << " failed products: ";
        for (auto &product : report.failedProducts) {
            std::cout << product <<  ", ";
        }
        std::cout << std::endl;
    }
}

void checkReports(const std::vector<WorkerReport> &reports, const std::vector<size_t>& reportSizes, size_t workers, size_t workingWorkers){
    printReports(reports);
    assert(reports.size() == workers);
    std::vector<size_t> counts(4, 0);
    size_t workingWorkersCount = 0;
    for (auto &report : reports) {
        counts[0] += report.collectedOrders.size();
        counts[1] += report.abandonedOrders.size();
        counts[2] += report.failedOrders.size();
        counts[3] += report.failedProducts.size();
        bool empty = report.collectedOrders.empty() && report.failedOrders.empty() && report.abandonedOrders.empty() && report.failedProducts.empty();
        if (!empty) workingWorkersCount++;
    }
    if (workingWorkersCount != workingWorkers) {
        std::cerr << "NON-EMPTY REPORTS NUMBER IS INCORRECT, SYSTEM MAY NOT BY CONCURRENT" << std::endl;
    }

    assert(counts[0] == reportSizes[0]);
    assert(counts[1] == reportSizes[1]);
    assert(counts[2] == reportSizes[2]);
    assert(counts[3] == reportSizes[3]);
}

bool test1() {
    std::cout << std::endl << "MANY CLIENTS, FEW WORKERS TEST" << std::endl;
    const unsigned int WORKERS_N = 2;
    const unsigned int CLIENTS_N = 8;
    const unsigned int ORDERS_N = 8;
    const unsigned int TIMEOUT_S = 1;
    std::vector<std::thread> clients;
    std::vector<std::string> order = {"burger", "burger", "burger", "cheeseburger"};

    std::shared_ptr<CustomMachine<Burger>> burgerMachine(new CustomMachine<Burger>(500));
    std::shared_ptr<CustomMachine<CheeseBurger>> cheeseburgerMachine(new CustomMachine<CheeseBurger>(500));
    System system{
            {
                    {"burger", burgerMachine},
                    {"cheeseburger", cheeseburgerMachine},
            },
            WORKERS_N,
            TIMEOUT_S
    };

    for (size_t i = 0; i < CLIENTS_N; i++) {
        std::thread client([&system, &order, i]() {
            std::this_thread::sleep_for(std::chrono::milliseconds(200 * (i + 1)));
            auto p = system.order(order);
            p->wait();
            auto products = system.collectOrder(std::move(p));
            assert(productsMatchOrder(std::move(products), order));
        });
        clients.push_back(std::move(client));
    }

    for (auto &client : clients) {
        client.join();
    }

    auto reports =  system.shutdown();
    checkReports(reports, {ORDERS_N, 0, 0, 0}, WORKERS_N, WORKERS_N);

    assert(burgerMachine->productsMade == 3 * ORDERS_N);
    assert(cheeseburgerMachine->productsMade == 1 * ORDERS_N);
    assert(burgerMachine->productsReturned == 0);
    assert(cheeseburgerMachine->productsReturned == 0);

    return true;
}

bool test2() {
    std::cout << std::endl << "ABANDONED ORDERS TEST" << std::endl;
    const unsigned int WORKERS_N = 8;
    const unsigned int CLIENTS_N = 8;
    const unsigned int ORDERS_N = 8;
    const unsigned int TIMEOUT_S = 3;
    std::vector<std::thread> clients;
    std::vector<std::string> order = {"burger", "cheeseburger"};

    std::shared_ptr<CustomMachine<Burger>> burgerMachine(new CustomMachine<Burger>(500));
    std::shared_ptr<CustomMachine<CheeseBurger>> cheeseburgerMachine(new CustomMachine<CheeseBurger>(500));
    System system{
            {
                    {"burger", burgerMachine},
                    {"cheeseburger", cheeseburgerMachine},
            },
            WORKERS_N,
            TIMEOUT_S
    };

    for (size_t i = 0; i < CLIENTS_N; i++) {
        std::thread client([&system, &order, i]() {
            auto p = system.order(order);
            p->wait();
            if (i % 2 == 0) {
                std::this_thread::sleep_for(std::chrono::seconds(TIMEOUT_S + 1));
                try {
                    auto products = system.collectOrder(std::move(p));
                    assert(false);
                } catch (const OrderExpiredException &e) {
                    assert(true);
                }

            } else {
                std::this_thread::sleep_for(std::chrono::seconds(TIMEOUT_S - 1));
                try {
                    auto products = system.collectOrder(std::move(p));
                    assert(productsMatchOrder(std::move(products), order));
                    assert(true);
                } catch (const OrderExpiredException &e) {
                    assert(false);
                }
            }
        });
        clients.push_back(std::move(client));
    }

    for (auto &client : clients) {
        client.join();
    }

    auto reports =  system.shutdown();
    checkReports(reports, {CLIENTS_N/2, CLIENTS_N/2, 0, 0}, WORKERS_N, WORKERS_N);
    assert(burgerMachine->productsMade == 1 * ORDERS_N);
    assert(cheeseburgerMachine->productsMade == 1 * ORDERS_N);
    assert(burgerMachine->productsReturned == ORDERS_N / 2);
    assert(cheeseburgerMachine->productsReturned == ORDERS_N / 2);

    return true;
}

bool test3() {
    std::cout << std::endl << "MENU TESTS" << std::endl;
    const unsigned int WORKERS_N = 3;
    const unsigned int CLIENTS_N = 3;
    const unsigned int ORDERS_N = 12;
    const unsigned int TIMEOUT_S = 3;
    std::vector<std::thread> clients;

    std::shared_ptr<CustomMachine<Burger>> burgerMachine(new CustomMachine<Burger>(500));
    std::shared_ptr<CustomMachine<CheeseBurger>> cheeseburgerMachine(new CustomMachine<CheeseBurger>(500));
    std::shared_ptr<WillFailMachine<McRoyal>> mcRoyalMachine(new WillFailMachine<McRoyal>(500, CLIENTS_N));
    std::shared_ptr<WillFailMachine<IceCream>> iceCreamMachine(new WillFailMachine<IceCream>(500, CLIENTS_N * 3)); // !!!
    // jeśli ktoś cofa zamówienie w przypadku awarii innej maszyny (co jest trudniejsze do napisania),
    // to trzeba zmienić "CLIENTS_N * 3" ==> "CLIENTS_N * 2 + 1"

    System system{
            {
                    {"burger", burgerMachine},
                    {"cheeseburger", cheeseburgerMachine},
                    {"mcRoyal", mcRoyalMachine},
                    {"iceCream", iceCreamMachine},
            },
            WORKERS_N,
            TIMEOUT_S
    };

    assert(system.getMenu() == std::vector<std::string>({"burger", "cheeseburger", "mcRoyal", "iceCream"}));

    std::barrier barrier(CLIENTS_N);
    for (size_t i = 0; i < CLIENTS_N; i++) {
        std::thread client([&system, &barrier] () {
            // Maszyny produkują rzeczy 0,5 sekundy!
            std::unique_ptr<CoasterPager> p;
            p = system.order({"burger", "cheeseburger", "mcRoyal", "iceCream"}); p->wait();
            assert(productsMatchOrder(system.collectOrder(std::move(p)), {"burger", "cheeseburger", "mcRoyal", "iceCream"}));
            assert(system.getMenu() == std::vector<std::string>({"burger", "cheeseburger", "mcRoyal", "iceCream"}));

            barrier.arrive_and_wait();

            p = system.order({"burger", "cheeseburger", "mcRoyal", "iceCream"}); p->wait();
            try {
                system.collectOrder(std::move(p)); assert(false);
            } catch (const FulfillmentFailure &e) {
                assert(true);
            }
            assert(system.getMenu() == std::vector<std::string>({"burger", "cheeseburger", "iceCream"}));

            barrier.arrive_and_wait();

            p = system.order({"burger", "cheeseburger", "iceCream"}); p->wait();
            assert(productsMatchOrder(system.collectOrder(std::move(p)), {"burger", "cheeseburger", "iceCream"}));
            assert(system.getMenu() == std::vector<std::string>({"burger", "cheeseburger", "iceCream"}));

            barrier.arrive_and_wait();

            p = system.order({"burger", "cheeseburger", "iceCream"}); p->wait();
            try {
                system.collectOrder(std::move(p)); assert(false);
            } catch (const FulfillmentFailure &e) {
                assert(true);
            }
        });
        clients.push_back(std::move(client));
    }
    for (auto &client : clients) {
        client.join();
    }

    auto reports =  system.shutdown();
    checkReports(reports, {CLIENTS_N * 2, CLIENTS_N * 2, 0, 0}, WORKERS_N, WORKERS_N);
    assert(burgerMachine->productsMade == ORDERS_N);
    assert(cheeseburgerMachine->productsMade == ORDERS_N);
    assert(mcRoyalMachine->productsMade == CLIENTS_N);
    assert(iceCreamMachine->productsMade == CLIENTS_N * 2);
}

int main(){
    test1();
    test2();
    test3();

    std::cout << "ALL TESTS PASSED" << std::endl;
    return 0;
}