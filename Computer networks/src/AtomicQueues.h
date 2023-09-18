#ifndef ZADANIE1_ATOMICQUEUES_H
#define ZADANIE1_ATOMICQUEUES_H

#include<vector>
#include <cstdlib>
#include <mutex>
#include <condition_variable>
#include <iostream>
#include <set>
#include <vector>
#include <algorithm>

/* Thread safe buffer for audio data. */
class AtomicRecvQueue {
private:
    std::vector<std::vector<uint8_t>> queue;
    size_t byte0, bsize, psize, write_num, read_num;
    bool debug = false;
    bool closed = false;
    bool reading_blocked = true;

    std::mutex mutex;
    std::condition_variable cv;
    std::set<size_t> missing_packages;

    size_t index(size_t byte_num) {
        return ((byte_num - byte0) / psize) % queue.size();
    }

    void update_missing_packages(size_t first_byte_num) {
//--------------------------------- | read_num -------------------------------------- | write_num --------------------------------------------- | first_byte_num
// packages lost but set not updated          packages that are missing and are in set             packages that are missing and are not in set
        std::set<size_t> missing_packages_copy(missing_packages);
        for (auto &i: missing_packages_copy) {
            if (i < read_num) missing_packages.erase(i);
            else break;
        }

        for (size_t i = write_num; i < first_byte_num; i += psize) {
            missing_packages.insert(i);
        }
        if (missing_packages.find(first_byte_num) != missing_packages.end()) {
            missing_packages.erase(first_byte_num);
        }
    }

    void reset_vectors_queue(size_t new_size) {
        queue = std::vector<std::vector<uint8_t>>(bsize / new_size);
        for (auto &i: queue) {
            i = std::vector<uint8_t>(new_size, 0);
        }
    }
public:
    AtomicRecvQueue(size_t bsize, size_t psize, size_t byte0 = 0) :
            byte0(byte0), bsize(bsize), psize(psize), write_num(byte0), read_num(byte0) {
        queue = std::vector<std::vector<uint8_t>>(bsize / psize);
        for (auto &i: queue) {
            i = std::vector<uint8_t>(psize, 0);
        }
    }

    bool write_data(size_t first_byte_num, std::vector<uint8_t> &data) {
        std::unique_lock<std::mutex> lock(mutex);
        if (data.size() != psize || first_byte_num < byte0 || first_byte_num < read_num) {
            if (first_byte_num < read_num){
                std::cerr << "Dropping package " << first_byte_num << " because it is too old (read_num = " << read_num
                << ", write_num = " << write_num << ")" << std::endl;
            }
            return false;
        }

        for (size_t i = write_num; i < first_byte_num; i += psize) {
            std::fill(queue[index(i)].begin(), queue[index(i)].end(), 0);
        }

        std::copy(data.begin(), data.end(), queue[index(first_byte_num)].begin());

        if (read_num + psize * queue.size() < first_byte_num) { // move read_num if necessary
            read_num = first_byte_num - psize * queue.size();
        }
        update_missing_packages(first_byte_num);
        if (first_byte_num >= write_num){
            write_num = first_byte_num + psize;
        }

        if (debug){
            std::cerr << "FIRST_BYTE: " << first_byte_num << " WRITE: " << write_num << " READ: " << read_num << std::endl;
        }

        cv.notify_all();
        return true;
    }


    bool read_data(std::vector<uint8_t> &buffer) {
        std::unique_lock<std::mutex> lock(mutex);
        while ((!closed && (read_num == write_num || reading_blocked))) {
            cv.wait(lock);
        }
        if (closed) return false;
        if (debug) {
            std::cerr << "READ: " << read_num << " WRITE: " << write_num << std::endl;
        }

        buffer.resize(psize);
        std::copy(queue[index(read_num)].begin(), queue[index(read_num)].end(), buffer.begin());
        read_num += psize;

        return true;
    }

    void close() {
        std::unique_lock<std::mutex> lock(mutex);
        closed = true;
        cv.notify_all();
    }

    void block_reading() {
        std::unique_lock<std::mutex> lock(mutex);
        reading_blocked = true;
    }

    void allow_reading() {
        std::unique_lock<std::mutex> lock(mutex);
        reading_blocked = false;
        cv.notify_all();
    }

    void reset_queue(size_t new_byte0, size_t new_psize) {
        block_reading();
        std::unique_lock<std::mutex> lock(mutex);
        read_num = new_byte0;
        write_num = new_byte0;
        byte0 = new_byte0;
        psize = new_psize;
        missing_packages.clear();
        reset_vectors_queue(new_psize);

        cv.notify_all();
    }

    std::set<uint64_t> get_missing_packages() {
        std::unique_lock<std::mutex> lock(mutex);
        if (!missing_packages.empty() && debug){
            std::cerr << "MISSING: " << missing_packages.size() << std::endl;
        }
        return missing_packages;
    }

    /*
     * Returns vector of missing packages between write_num and new_package_num
     */
    std::set<uint64_t> get_gap_packages(uint64_t new_package_num){
        std::set<uint64_t> gap_packages;
        std::unique_lock<std::mutex> lock(mutex);
        for (uint64_t i = write_num; i < new_package_num; i += psize) {
            gap_packages.insert(i);
        }
        if (!gap_packages.empty() && debug){
            std::cerr << "write num = " << write_num << " new package num = " << new_package_num << std::endl;
            std::cerr << "GAP: " << gap_packages.size() << std::endl;
        }
        return gap_packages;
    }

    [[nodiscard]] size_t get_byte0() const {
        return byte0;
    }

    // copy constructor
    AtomicRecvQueue(const AtomicRecvQueue &other) {
        std::unique_lock<std::mutex> lock(mutex);
        queue = other.queue;
        psize = other.psize;
        bsize = other.bsize;
        byte0 = other.byte0;
        write_num = other.write_num;
        read_num = other.read_num;
        debug = other.debug;
        std::copy(other.queue.begin(), other.queue.end(), queue.begin());
    }
};

class AtomicSendQueue {
private:
    size_t fsize, psize, read_num, write_num;
    std::vector<uint64_t> first_bytes;
    std::vector<std::vector<uint8_t>> queue;

    std::mutex mutex;

    size_t index(size_t first_byte_num) {
        return (first_byte_num / psize) % queue.size();
    }
public:
    AtomicSendQueue(size_t fsize, size_t psize)
    : fsize(fsize), psize(psize), read_num(0), write_num(0) {
        queue = std::vector<std::vector<uint8_t>>(fsize / psize);
        first_bytes = std::vector<uint64_t>(fsize / psize, 0);
        for (auto &i: queue) {
            i = std::vector<uint8_t>(psize, 0);
        }
    }

    void push_data(uint64_t first_byte_num, std::vector<uint8_t> &data) {
        std::unique_lock<std::mutex> lock(mutex);
        if (data.size() != psize || queue.empty()) {
            return;
        }
        queue[index(write_num)] = data;
        first_bytes[index(write_num)] = first_byte_num;
        write_num += psize;
    }

    std::vector<std::vector<uint8_t >> get_data(std::vector<uint64_t> &first_byte_nums){
        std::vector<std::vector<uint8_t >> res;
        std::unique_lock<std::mutex> lock(mutex);
        if (queue.empty()){
            for (size_t i = 0; i < first_byte_nums.size(); i++){
                res.push_back(std::vector<uint8_t>(0, 0));
            }
        } else {
            for (size_t first_byte_num : first_byte_nums) {
                auto ind = std::find(first_bytes.begin(), first_bytes.end(), first_byte_num);
                if (ind != first_bytes.end() && first_byte_num < write_num) {
                    res.push_back(queue[ind - first_bytes.begin()]);
                }
                else {
                    res.push_back(std::vector<uint8_t>(0, 0));
                }
            }
        }
        return res;
    }
};


#endif //ZADANIE1_ATOMICQUEUES_H
