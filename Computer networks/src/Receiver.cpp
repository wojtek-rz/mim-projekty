#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <csignal>
#include <atomic>
#include <chrono>

#include "UdpServer.h"
#include "TcpServer.h"
#include "AtomicQueues.h"
#include "DatagramConverter.h"
#include "Options.h"
#include "Telnet.h"
#include "Stations.h"
#include "Discover.h"
#include "ReceiverStream.h"

inline static void install_signal_handler(int signal, void (*handler)(int), int flags) {
    struct sigaction action{};
    sigset_t block_mask;

    sigemptyset(&block_mask);
    action.sa_handler = handler;
    action.sa_mask = block_mask;
    action.sa_flags = flags;

    CHECK_ERRNO(sigaction(signal, &action, nullptr));
}

std::atomic<bool> quit(false);    // signal flag
static void catch_int(int)
{
    quit.store(true);
}

using clock_type = std::chrono::high_resolution_clock;

long calculate_timeout(std::chrono::time_point<clock_type> target){
    auto now = clock_type::now();
    if (now > target) {
        return 0;
    }
    return std::chrono::duration_cast<std::chrono::milliseconds>(target - now).count();
}

int main(int argc, char *argv[]) {
    const long TELNET_TIMEOUT_MS = 50;
    const long DISCOVERY_REFRESH_RATE_MS = 5000;
    const long DISCOVERY_TIMEOUT_MS = 250;

    install_signal_handler(SIGINT, catch_int, SA_RESTART);

    ReceiverOptions options;
    if (!options.parse_options(argc, argv)) {
        return 1;
    }

    Stations stations(options.name);

    auto ctrl_server = std::make_shared<UdpServer>();
    ctrl_server->allow_broadcast_send();
    ctrl_server->set_receive_timeout(DISCOVERY_TIMEOUT_MS);

    TcpServer tcp_server((int)options.ui_port);
    tcp_server.add_introduction_msg([&stations](){
        return Telnet::TELNET_CHARACTER_MODE + Telnet::get_menu_display(stations);
    });

    auto when_started = clock_type::now();
    auto target = when_started + std::chrono::milliseconds(DISCOVERY_REFRESH_RATE_MS);
    stations.update_stations(Discover::discover_stations(ctrl_server, options.ctrl_port));

    ReceiverStream stream(options, stations.get_selected_station(), ctrl_server);

    while(!quit.load()){
        if (calculate_timeout(target) > 0) {
            std::string msg = tcp_server.get_new_message(TELNET_TIMEOUT_MS);
            Telnet::update_selected_station(stations, msg);
        } else {
            stations.update_stations(Discover::discover_stations(ctrl_server, options.ctrl_port));
            target += std::chrono::milliseconds(DISCOVERY_REFRESH_RATE_MS);
        }
        tcp_server.send_message(Telnet::get_menu_display(stations));
        stream.set_station(stations.get_selected_station());
    }


    return 0;
}

