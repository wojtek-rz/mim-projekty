#ifndef ZADANIE2_DISCOVER_H
#define ZADANIE2_DISCOVER_H
#include <string>
#include <vector>
#include <arpa/inet.h>
#include "Stations.h"
#include "Protocols.h"
#include "Options.h"

namespace Discover {
    const std::string BROADCAST_ADDR = "255.255.255.255";

    Stations::station_t parse_response(const std::string &response) {
        Stations::station_t station;
        std::string addr_str, port_str, name;
        size_t i = Ctrl::REPLY_PREFIX.size() + 1;
        while (response[i] == ' ') ++i;
        while(i < response.size()) {
            if (response[i] != ' ') addr_str += response[i];
            else break;
            ++i;
        }

        while (response[i] == ' ') ++i;
        while (i < response.size()) {
            if (std::isdigit(response[i])) port_str += response[i];
            else break;
            ++i;
        }

        while (response[i] == ' ') ++i;
        while (i < response.size() && response[i] != '\n') {
            name += response[i]; ++i;
        }

        int port = std::stoi(port_str);
        SenderOptions::check_port(port);
        SenderOptions::check_mcast_addr(addr_str);
        SenderOptions::check_name(name);

        station.name = name;
        station.addr = addr_str;
        station.port = port;

        return station;
    }

    std::vector<Stations::station_t> discover_stations(const std::shared_ptr<UdpServer> &ctrl_server, uint16_t port) {
        std::vector<Stations::station_t> stations;
        ssize_t len;
        std::vector<uint8_t> buffer(1024);

        struct sockaddr_in addr = UdpServer::get_sockaddr_in(BROADCAST_ADDR.c_str(), port);
        ctrl_server->send_message(Ctrl::LOOKUP_PREFIX + "\n", &addr);

        while ((len = ctrl_server->read_message(buffer)) > 0) {
            std::string response(buffer.begin(), buffer.begin() + len);
            if (response.substr(0, Ctrl::REPLY_PREFIX.size()) == Ctrl::REPLY_PREFIX) {
                try{
                    stations.push_back(parse_response(response));
                    std::cerr << "[Discover] Found station " << stations.back().name << std::endl;
                } catch (std::exception &e) {
                    std::cerr << "[Discover] Error parsing response: " << e.what() << std::endl;
                }
            }
        }
        std::cerr << "[Discover] No more responses" << std::endl;
        std::cerr << "[Discover] Found " << stations.size() << " stations" << std::endl;

        return stations;
    }
}

#endif //ZADANIE2_DISCOVER_H
