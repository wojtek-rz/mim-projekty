#ifndef ZADANIE2_RECEIVERSTREAM_H
#define ZADANIE2_RECEIVERSTREAM_H

#include <mutex>
#include <thread>
#include <utility>
#include "AtomicQueues.h"
#include "DatagramConverter.h"
#include "UdpServer.h"
#include "Stations.h"
#include "Options.h"
#include "Protocols.h"

class ReceiverStream {
private:
    using clock_type = std::chrono::high_resolution_clock;

    ReceiverOptions options;
    AtomicRecvQueue recv_queue;
    std::shared_ptr<UdpServer> ctrl_server;

    std::mutex mutex;
    Stations::station_t station;
    struct sockaddr_in station_ctrl_addr{};
    bool station_changed;

    std::atomic<bool> quit;
    std::thread reader_thread;
    std::thread rexmit_thread;
    std::thread writer_thread;

    static void write_n(int fd, uint8_t *buf, size_t n) {
        size_t len, tlen;
        len = 0;
        do {
            tlen = write(fd, buf + len, n - len);
            len += tlen;
        } while (len < n && tlen > 0);
    }

    bool read_new_package(UdpServer &receiver, Audio::datagram_t &audio_package, std::vector<uint8_t> buffer) {
        ssize_t len = receiver.read_message(buffer);
        while (len < 17){
            len = receiver.read_message(buffer);
            if (get_stations_changed() || quit.load()){
                return false;
            }
        }
        buffer.resize(len);
        Audio::deserialize_package(&audio_package, buffer);
        return true;
    }

    // Unused
    void writer_thread_loop() {
        std::vector<uint8_t> msg;
        while (!quit.load() && recv_queue.read_data(msg)) {
            write_n(STDOUT_FILENO, msg.data(), msg.size());
        }
    };

    void send_rexmit_request(const std::set<uint64_t> &requested_nums){
        std::vector<uint64_t> nums(requested_nums.begin(), requested_nums.end());
        std::string request = Ctrl::REXMIT_PREFIX + " ";
        for (int64_t i = 0; i < (int64_t)nums.size() - 1; ++i){
            request += std::to_string(nums[i]) + ",";
        }
        if (!nums.empty()){
            request += std::to_string(nums[nums.size() - 1]) + "\n";
            ctrl_server->send_message(request, &station_ctrl_addr);
            std:: cerr << "[Receiver] Sent rexmit request for " << nums.size() << " packages" << std::endl;
        }
    }

    void rexmit_thread_loop() {
        auto when_started = clock_type::now();
        auto target = when_started + std::chrono::milliseconds(options.rtime);
        while (!quit.load()){
            std::this_thread::sleep_until(target);
            target += std::chrono::milliseconds(options.rtime);

            if (get_stations_changed() || station.addr.empty()){
                continue;
            }

            auto missing_packages = recv_queue.get_missing_packages();
            if (!missing_packages.empty()){
                send_rexmit_request(missing_packages);
            }
        }
    }

    bool get_stations_changed(){
        std::lock_guard<std::mutex> lock(mutex);
        return station_changed;
    }

    void read_stream(UdpServer &receiver, Audio::datagram_t &audio_package) {
        int psize = (int)audio_package.payload.size();
        std::vector<uint8_t> msg(psize + 16);

        size_t current_session_id = audio_package.session_id;
        bool reading_from_queue_allowed = false;

        while (!get_stations_changed() && !quit.load()) {
            msg.resize(psize + 16);
            if (!read_new_package(receiver,audio_package, msg)){
                continue;
            }

            if (audio_package.session_id < current_session_id) {
                continue;
            } else if (audio_package.session_id > current_session_id) {
                current_session_id = audio_package.session_id;
                recv_queue.reset_queue(audio_package.first_byte_num, audio_package.payload.size());
                reading_from_queue_allowed = false;
            }

            send_rexmit_request(recv_queue.get_gap_packages(audio_package.first_byte_num));
            recv_queue.write_data(audio_package.first_byte_num, audio_package.payload);
            if (!reading_from_queue_allowed &&
                audio_package.first_byte_num >= options.bsize * 3 / 4 + recv_queue.get_byte0()) {
                reading_from_queue_allowed = true;
                recv_queue.allow_reading();
            }
        }
    }

    void reader_thread_loop() {
        while(!quit.load()) {
            mutex.lock();
            UdpServer receiver(station.port);
            station_changed = false;
            receiver.set_receive_timeout(250);
            if (receiver.join_multicast_group(station.addr.c_str())) {
                std::cerr << "Success joining multicast group" << std::endl;
            } else {
                std::cerr << "Error joining multicast group" << std::endl;
            }
            mutex.unlock();

            std::vector<uint8_t> msg(options.bsize + 16);
            Audio::datagram_t audio_package{};

            if (!read_new_package(receiver, audio_package, msg)){
                continue;
            }
            recv_queue.reset_queue(audio_package.first_byte_num, audio_package.payload.size());
            recv_queue.write_data(audio_package.first_byte_num, audio_package.payload);

            read_stream(receiver, audio_package);
        }
        recv_queue.close();
    }
public:
    explicit ReceiverStream(ReceiverOptions options, const Stations::station_t &s, std::shared_ptr<UdpServer> ctrl_server) :
    options(std::move(options)), recv_queue(options.bsize, options.bsize), ctrl_server(std::move(ctrl_server)),
    station_changed(false), quit(false) {
        set_station(s);
        reader_thread = std::thread([this]() { reader_thread_loop(); });
        rexmit_thread = std::thread([this]() { rexmit_thread_loop(); });
        writer_thread = std::thread([this]() { writer_thread_loop(); });
    }

    ~ReceiverStream() {
        quit.store(true);
        reader_thread.join();
        rexmit_thread.join();
        writer_thread.join();
    }

    void set_station(const Stations::station_t& new_station) {
        std::lock_guard<std::mutex> lock(mutex);
        if (!Stations::cmp_stations(station, new_station)) {
            station_changed = true;
            station = new_station;
            if (!new_station.addr.empty()) {
                station_ctrl_addr = UdpServer::get_sockaddr_in(new_station.addr.c_str(), new_station.port);
                auto ctrl_port = (uint16_t)options.ctrl_port;
                station_ctrl_addr.sin_port = htons(ctrl_port);
            }
        }
    }
};

#endif //ZADANIE2_RECEIVERSTREAM_H
