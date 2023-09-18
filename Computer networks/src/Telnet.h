#ifndef TELNET_H
#define TELNET_H

#include <cstring>
#include <iostream>
#include <unistd.h>
#include <algorithm>
#include "Stations.h"

namespace Telnet {
    const std::string KEY_UP = "\e[A";
    const std::string KEY_DOWN = "\e[B";
    const std::string KEY_ENTER = "\15";
    const std::string KEY_PAGE_UP = "\e[5~";

    const std::string TELNET_CHARACTER_MODE = "\377\375\042\377\373\001";
    const std::string CLEAR_SCREEN = "\x1b[2J";
    const std::string GO_TO_BEGIN = "\x1b[H";

    const std::string CRLF = "\r\n";

    std::string get_menu_display(Stations &stations_class) {
        auto stations = stations_class.get_stations();
        auto selected_station = stations_class.get_selected_station();
        // sort stations alphabetically
        std::sort(stations.begin(), stations.end(), [](const Stations::station_t &a, const Stations::station_t &b) {
            return a.name < b.name;
        });

        std::string menu = Telnet::CLEAR_SCREEN + Telnet::GO_TO_BEGIN;
        menu.append("------------------------------------------------------------------------" + Telnet::CRLF + Telnet::CRLF);
        menu.append(" SIK Radio" + Telnet::CRLF + Telnet::CRLF);
        menu.append("------------------------------------------------------------------------" + Telnet::CRLF + Telnet::CRLF);

        for (size_t i = 0; i < stations.size(); i++) {
            if (Stations::cmp_stations(stations[i], selected_station)) {
                menu += " > ";
            }
            menu.append(stations[i].name + Telnet::CRLF + Telnet::CRLF);
        }
        menu.append("------------------------------------------------------------------------");
        return menu;
    }

    void update_selected_station(Stations &stations_class, std::string &msg) {
        if (msg != KEY_DOWN && msg != KEY_UP) return;
        auto stations = stations_class.get_stations();
        auto selected_station = stations_class.get_selected_station();
        // sort stations alphabetically
        std::sort(stations.begin(), stations.end(), [](const Stations::station_t &a, const Stations::station_t &b) {
            return a.name < b.name;
        });

        int selected_station_index = Stations::find_station(stations, selected_station);
        if (selected_station_index == -1) return;

        if (msg == KEY_DOWN) {
            selected_station_index++;
            if (selected_station_index >= (int)stations.size()) selected_station_index = 0;
        } else if (msg == KEY_UP) {
            selected_station_index--;
            if (selected_station_index < 0) selected_station_index = (int)stations.size() - 1;
        }

        stations_class.set_selected_station(stations[selected_station_index]);
    }
}

#endif // TELNET_H