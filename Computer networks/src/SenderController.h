#ifndef ZADANIE2_CONTROLLER_H
#define ZADANIE2_CONTROLLER_H

#include <chrono>
#include <thread>
#include <utility>
#include <vector>
#include <atomic>
#include "UdpServer.h"
#include "Options.h"
#include "AtomicQueues.h"
#include "DatagramConverter.h"
#include "Protocols.h"

class Controller {
private:
    using clock_type = std::chrono::high_resolution_clock;
    std::atomic<bool> quit{false};

    UdpServer server;
    std::shared_ptr<SenderOptions> options;
    std::shared_ptr<AtomicSendQueue> send_queue;
    std::thread controller_thread;

    std::thread remix_thread;
    std::mutex rexmit_mutex;
    std::set<uint64_t> remix_first_bytes_nums;

    const long CTRL_TIMEOUT = 250;
public:

    Controller(const std::shared_ptr<SenderOptions> &options, const std::shared_ptr<AtomicSendQueue> &send_queue) :
        server((int)options->ctrl_port), options(options), send_queue(send_queue) {
        server.set_send_address(options->mcast_addr, options->data_port);
        server.set_receive_timeout(CTRL_TIMEOUT);
        controller_thread = std::thread([this] () {controller_thread_loop();});
        remix_thread = std::thread([this] () {rexmit_reply_thread_loop();});
    };
    ~Controller() {
        quit.store(true);
        controller_thread.join();
        remix_thread.join();
    }

    void handle_lookup_request(struct sockaddr_in *addr){
        const std::string response_str = Ctrl::REPLY_PREFIX + " " + options->mcast_addr + " "
                + std::to_string(options->data_port) + " " + options->name + "\n";

        server.send_message(response_str, addr);
    }

    void handle_rexmit_request(const std::string &request, struct sockaddr_in *addr){
        std::string tmp_number;
        std::vector<uint64_t> requested_nums;

        for (size_t i = Ctrl::REXMIT_PREFIX.size(); i < request.size(); i++){
            unsigned char c = request[i];
            if (c == ',' || c == '\n' || c == ' '){
                if (!tmp_number.empty()) {
                    requested_nums.emplace_back(std::stoi(tmp_number));
                    tmp_number.clear();
                }
            } else if (std::isdigit(c)) {
                tmp_number += (char)c;
            } else {
                return;
            }
        }

        std::unique_lock<std::mutex> lock(rexmit_mutex);
        remix_first_bytes_nums.insert(requested_nums.begin(), requested_nums.end());
        std::cerr << requested_nums.size() << " inserted" << std::endl;
    }

    void controller_thread_loop(){
        struct sockaddr_in addr{};
        std::vector<uint8_t> buffer(128);

        while(!quit.load()){
            ssize_t len = server.read_message(buffer, &addr);
            if (len == 0) {
                continue;
            }
            std::string request_str(buffer.begin(), buffer.begin() + len);

            std::cerr << "[control thread] received request: " << request_str << std::endl;

            if (request_str.find(Ctrl::REXMIT_PREFIX) == 0){
                handle_rexmit_request(request_str, &addr);
            } else if (request_str.find(Ctrl::LOOKUP_PREFIX) == 0){
                handle_lookup_request(&addr);
            }
        }
    }

    void rexmit_reply_thread_loop(){

        auto when_started = clock_type::now();
        auto target_time = when_started + std::chrono::milliseconds(options->rtime);
        Audio::datagram_t datagram{static_cast<uint64_t>(options->session_id), 0, std::vector<uint8_t>()};
        std::vector<uint8_t> buffer(options->psize + 16);

        while(!quit.load()){
            std::this_thread::sleep_until(target_time);
            target_time += std::chrono::milliseconds(options->rtime);

            std::vector<uint64_t> rexmit_requests_vec;
            {
                std::lock_guard<std::mutex> lock(rexmit_mutex);
                rexmit_requests_vec.insert(rexmit_requests_vec.end(), remix_first_bytes_nums.begin(), remix_first_bytes_nums.end());
                remix_first_bytes_nums.clear();
            }

            std::cerr << "[rexmit thread] rexmitting " << rexmit_requests_vec.size() << " packets" << std::endl;
            std::vector<std::vector<uint8_t>> data = send_queue->get_data(rexmit_requests_vec);

            for (size_t i = 0; i < rexmit_requests_vec.size(); i++){
               if (!data[i].empty()) {
                    datagram.first_byte_num = rexmit_requests_vec[i];
                    datagram.payload = data[i];
                    Audio::serialize_package(&datagram, buffer);
                    server.send_message(buffer);
                    std::cerr << "[rexmit thread] rexmitting packet " << datagram.first_byte_num << std::endl;
               }
            }
        }
    }
};

#endif //ZADANIE2_CONTROLLER_H