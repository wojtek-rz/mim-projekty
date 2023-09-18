#ifndef ZADANIE1_OPTIONS_H
#define ZADANIE1_OPTIONS_H

#include <boost/program_options.hpp>
#include <iostream>
#include <arpa/inet.h>
#include "UdpServer.h"

#define MAX_UDP_DATAGRAM_SIZE 65507

namespace po = boost::program_options;

class Options {
public:
    int64_t ctrl_port;
    int64_t rtime;

    bool parse_options(int argc, char *argv[]) {
        try {
            po::options_description desc = get_base_arguments_desc();
            add_arguments(desc);
            po::variables_map vm = get_variables_map(argc, argv, desc);
            if (vm.count("help")) {
                return false;
            }

            process_base_arguments(vm);
            process_additional_arguments(vm);
        }
        catch (std::exception &e) {
            std::cerr << "error: " << e.what() << "\n";
            return false;
        }
        catch (...) {
            std::cerr << "Exception of unknown type!\n";
            return false;
        }
        return true;
    }

    static void check_port(int64_t port){
        if (port <= 0 || port > 65535) {
            throw std::invalid_argument("Port number must be in range 0-65535");
        }
    }

    static void check_name(const std::string &name){
        if (name.empty()){
            throw std::invalid_argument("Name cannot be empty");
        }
    }
protected:
    const int album_number = 438709;

    static void check_positive(int64_t number){
        if (number <= 0) {
            throw std::invalid_argument("Number must be positive");
        }
    }
private:
    [[nodiscard]] po::options_description get_base_arguments_desc() const {
        po::options_description desc("Allowed options");
        desc.add_options()
                ("help", "produce help message")
                ("CTRL_PORT,C", po::value<int64_t>()->default_value(30000 + (album_number % 10000)), "control port for UDP connection")
                ("RTIME,R", po::value<int64_t>()->default_value(250), "retransmission time");
        return desc;
    }

    virtual void add_arguments(po::options_description &desc) = 0;

    void process_base_arguments(po::variables_map &vm) {
        ctrl_port = vm["CTRL_PORT"].as<int64_t>();
        rtime = vm["RTIME"].as<int64_t>();
        check_port(ctrl_port);
        check_positive(rtime);
    }

    virtual void process_additional_arguments(po::variables_map &vm) = 0;

    static po::variables_map get_variables_map(int argc, char *argv[], po::options_description &desc) {
        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);

        if (vm.count("help")) {
            std::cout << desc << "\n";
            return vm;
        }

        po::notify(vm);

        return vm;
    }
};


class SenderOptions : public Options {
public:
    std::string mcast_addr;
    int64_t data_port;
    int64_t psize;
    int64_t fsize;
    int64_t session_id;
    std::string name;

    static void check_mcast_addr(const std::string &addr){
        struct sockaddr_in sa;
        int result = inet_aton(addr.c_str(), &sa.sin_addr);
        if (result != 1 || !UdpServer::is_multicast_address(sa.sin_addr.s_addr)) {
            throw std::invalid_argument("Invalid multicast address");
        }
    }
private:

    void add_arguments(po::options_description &desc) override {
        desc.add_options()
                ("MCAST_ADDR,a", po::value<std::string>()->required(), "multicast address for sending audio data")
                ("DATA_PORT,P", po::value<int64_t>()->default_value(20000 + (album_number % 10000)), "data port for UDP connection")
                ("PSIZE,p", po::value<int64_t>()->default_value(512), "number of audio data bytes in one package")
                ("FSIZE,f", po::value<int64_t>()->default_value(131072), "sender buffer size")
                ("NAME,n", po::value<std::string>()->default_value("Nienazwany Nadajnik"), "name of the radio station");

    }

    void process_additional_arguments(po::variables_map &vm) override {
        mcast_addr = vm["MCAST_ADDR"].as<std::string>();
        data_port = vm["DATA_PORT"].as<int64_t>();
        psize = vm["PSIZE"].as<int64_t>();
        fsize = vm["FSIZE"].as<int64_t>();
        name = vm["NAME"].as<std::string>();

        check_mcast_addr(mcast_addr);
        check_port(data_port);
        check_positive(psize);
        check_name(name);
        if (psize + 16 > MAX_UDP_DATAGRAM_SIZE) {
            throw std::invalid_argument("Package size is too big");
        }
        if (fsize < 0) {
            throw std::invalid_argument("Fsize must be positive");
        }
    }
};

class ReceiverOptions : public Options {
public:
    std::string discover_addr;
    int64_t ui_port;
    int64_t bsize;
    std::string name;

private:
    void add_arguments(po::options_description &desc) override {
        desc.add_options()
                ("DISCOVER_ADDR,d", po::value<std::string>()->default_value("255.255.255.255"),
                 "address for discovering radio stations")
                ("UI_PORT,U", po::value<int64_t>()->default_value(10000 + (album_number % 10000)),
                 "TCP port for text control interface")
                ("BSIZE,b", po::value<int64_t>()->default_value(65536), "size of buffer")
                ("NAME,n", po::value<std::string>(), "name of the radio station");

    }

    static void check_discover_addr(const std::string &addr){
        struct sockaddr_in sa{};
        int result = inet_aton(addr.c_str(), &sa.sin_addr);
        if (result != 1) {
            throw std::invalid_argument("Invalid discover address");
        }
    }

    void process_additional_arguments(po::variables_map &vm) override {
        discover_addr = vm["DISCOVER_ADDR"].as<std::string>();
        ui_port = vm["UI_PORT"].as<int64_t>();
        bsize = vm["BSIZE"].as<int64_t>();

        check_discover_addr(discover_addr);
        check_port(ui_port);
        check_positive(bsize);

        // if name is in arguments, use it and check it
        if (vm.count("NAME")) {
            name = vm["NAME"].as<std::string>();
            check_name(name);
        }
    }
};

#endif //ZADANIE1_OPTIONS_H
