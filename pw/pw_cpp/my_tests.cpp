#include <atomic>
#include <chrono>
#include <deque>
#include <thread>
#include <iostream>
#include <latch>
#include <algorithm>
#include <utility>
#include <vector>
#include <cassert>
#include "system.hpp"


template<typename T, typename V>
bool checkType(const V *v) {
    return dynamic_cast<const T *>(v) != nullptr;
}

class ProductEnhanced : public Product {
public:
    virtual std::string getName() = 0;
    ~ProductEnhanced() override = default;
};

class Burger : public ProductEnhanced {
public:
    std::string getName() override {
        return "burger";
    }
};

class CheeseBurger : public ProductEnhanced {
public:
    std::string getName() override {
        return "cheeseburger";
    }
};

class BurgerMachine : public Machine {
protected:
    size_t productsMade;
    bool turned_on;
    std::chrono::milliseconds time = std::chrono::milliseconds (500);
public:
    BurgerMachine() : productsMade(0), turned_on(false) {}

    std::unique_ptr<Product> getProduct() {
        if (!turned_on) throw MachineNotWorking();

        productsMade += 1;
        std::this_thread::sleep_for(time);
        return std::unique_ptr<Product>(new Burger());
    }

    void returnProduct(std::unique_ptr<Product> product) {
        if (!checkType<Burger>(product.get())) throw BadProductException();
    }

    void start() {
        turned_on = true;
    }

    void stop() {
        turned_on = false;
    }

    size_t getProductsMade() {
        return productsMade;
    }
};

class CheeseBurgerMachine : public BurgerMachine {
public:
    CheeseBurgerMachine() : BurgerMachine() {}

    std::unique_ptr<Product> getProduct() override {
        if (!BurgerMachine::turned_on) throw MachineNotWorking();

        productsMade += 1;
        std::this_thread::sleep_for(time);
        return std::unique_ptr<Product>(new CheeseBurger());
    }

    void returnProduct(std::unique_ptr<Product> product) override {
        if (!checkType<CheeseBurger>(product.get())) throw BadProductException();
    }
};

bool productsMatchOrder(std::vector<std::unique_ptr<Product>> products, std::vector<std::string> order){
    std::vector<std::string> products_names;
    for (auto &product : products) {
        products_names.push_back(dynamic_cast<ProductEnhanced *>(product.get())->getName());
    }
    std::sort(products_names.begin(), products_names.end());
    std::sort(order.begin(), order.end());

    return products_names == order;
}


bool test1() {
    std::vector<std::thread> clients;
    std::vector<std::vector<std::string>> orders = {
            {"burger", "burger", "burger", "cheeseburger"},
            {"burger", "burger", "burger", "cheeseburger"},
            {"burger", "burger", "burger", "cheeseburger"},
            {"burger", "burger", "burger", "cheeseburger"},
            {"burger", "burger", "burger", "cheeseburger"},
            {"burger", "burger", "burger", "cheeseburger"},
    };
    std::shared_ptr<BurgerMachine> burgerMachine(new BurgerMachine());
    std::shared_ptr<BurgerMachine> cheeseBurgerMachine(new CheeseBurgerMachine());
    System system{
            {
                    {"burger", burgerMachine},
                    {"cheeseburger", cheeseBurgerMachine},
            },
            2,
            1
    };

    // shutdown latch
    std::latch shutdown(orders.size());

    for (size_t i = 0; i < orders.size(); i++) {
        std::thread([&system, &orders, i, &shutdown]() {
            std::this_thread::sleep_for(std::chrono::milliseconds(200 * (i + 1)));
            auto p = system.order(orders[i]);
            p->wait();
            auto products = system.collectOrder(std::move(p));
            assert(productsMatchOrder(std::move(products), orders[i]));
            shutdown.count_down();
        }).detach();
    }

    shutdown.wait();
    auto reports =  system.shutdown();

    assert(reports.size() == 2);

    // print reports
    for (auto &report : reports) {
        cout << "report: " << endl;
        assert(report.collectedOrders.size() == 3);
        for (auto &report_orders : report.collectedOrders) {
            assert(report_orders == orders[0]);
            for (auto &order : report_orders) {
                cout << order << " ";
            }
            cout << endl;
        }
    }
    // assert that all products were made



    assert(burgerMachine->getProductsMade() == 18);
    assert(cheeseBurgerMachine->getProductsMade() == 6);

    return true;
}

int main(){
    test1();
    return 0;
}