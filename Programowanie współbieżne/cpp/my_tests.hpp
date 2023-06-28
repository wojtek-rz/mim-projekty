#ifndef PW_CPP_MY_TESTS_HPP
#define PW_CPP_MY_TESTS_HPP

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
#include "system.cpp"


template<typename T, typename V>
bool checkType(const V *v) {
    return dynamic_cast<const T *>(v) != nullptr;
}

class CustomProduct : public Product {
public:
    virtual std::string getName() = 0;
    ~CustomProduct() override = default;
};

class Burger : public CustomProduct {
public:
    std::string getName() override {
        return "burger";
    }
};

class McRoyal : public CustomProduct {
public:
    std::string getName() override {
        return "mcRoyal";
    }
};

class CheeseBurger : public CustomProduct {
public:
    std::string getName() override {
        return "cheeseburger";
    }
};

class IceCream : public CustomProduct {
public:
    std::string getName() override {
        return "iceCream";
    }
};

template <typename T>
class CustomMachine : public Machine {
protected:
    bool turned_on;
    size_t time;
public:
    size_t productsMade;
    size_t productsReturned;

    explicit CustomMachine(size_t time) : Machine(), turned_on(false), time(time), productsMade(0), productsReturned(0) {}

    std::unique_ptr<Product> getProduct() override {
        if (!turned_on) throw MachineNotWorking();

        productsMade += 1;
        std::this_thread::sleep_for(std::chrono::milliseconds(time));
        return std::unique_ptr<T>(new T());
    }

    void returnProduct(std::unique_ptr<Product> product) override {
        productsReturned += 1;
        if (!checkType<T>(product.get())) throw BadProductException();
    }

    void start() override {
        turned_on = true;
    }

    void stop() override {
        turned_on = false;
    }
};

template <typename T>
class WillFailMachine : public CustomMachine<T> {
private:
    size_t notFailedProducts;
public:
    explicit WillFailMachine(size_t time, size_t notFailedProducts) : CustomMachine<T>(time),
            notFailedProducts(notFailedProducts){}

    std::unique_ptr<Product> getProduct() override {
        if (!CustomMachine<T>::turned_on) throw MachineNotWorking();

        std::this_thread::sleep_for(std::chrono::milliseconds(CustomMachine<T>::time));
        if (CustomMachine<T>::productsMade >= notFailedProducts){
            throw MachineFailure();
        }
        CustomMachine<T>::productsMade += 1;
        return std::unique_ptr<T>(new T());
    }
};

#endif //PW_CPP_MY_TESTS_HPP
