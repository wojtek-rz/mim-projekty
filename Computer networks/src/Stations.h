#ifndef ZADANIE2_STATIONS_H
#define ZADANIE2_STATIONS_H

#include <sys/socket.h>
#include <netinet/in.h>
#include <mutex>
#include <utility>
#include <vector>


class Stations {
public:
    struct station_t {
        std::string name;
        std::string addr;
        uint16_t port{};
        int idle_time = 0;
    };
    using station_t = struct station_t;

private:
    std::vector<station_t> stations;
    // addr and port set to 0 means no station selected
    station_t selected_station;
    // empty string means no favourite station
    std::string favourite;

    void update_selected_not_in_stations() {
        if (find_station(stations, selected_station) == -1) {
            if (favourite.empty() && !stations.empty()) {
                selected_station = stations[0];
            } else {
                selected_station = station_t();
            }
        }
    }
public:
    Stations() = default;
    explicit Stations(std::string favourite) :
        favourite(std::move(favourite)) {
    }

    void update_stations(const std::vector<station_t> &new_stations) {
        for (station_t &station : stations) {
            station.idle_time++;
        }

        for (const station_t &new_station : new_stations) {
            int idx = find_station(stations, new_station);
            if (idx == -1) {
                stations.push_back(new_station);

                if (favourite == new_station.name) {
                    selected_station = new_station;
                }
            } else {
                stations[idx].idle_time = 0;
            }

        }

        auto predicate = [](const station_t &s) { return s.idle_time > 3; };
        stations.erase(std::remove_if(stations.begin(), stations.end(), predicate), stations.end());

        update_selected_not_in_stations();
    }


    std::vector<station_t> get_stations() {
        return stations;
    }

    station_t get_selected_station(){
        return selected_station;
    }

    void set_selected_station(const station_t &station) {
        selected_station = station;
    }

    static bool cmp_stations(const station_t &a, const station_t &b) {
        return a.name == b.name && a.addr == b.addr && a.port == b.port;
    }

    static int find_station(std::vector<station_t> &stations, const station_t &station) {
        for (int i = 0; i < (int)stations.size(); ++i) {
            if (cmp_stations(stations[i], station)) return i;
        }
        return -1;
    }
};

#endif //ZADANIE2_STATIONS_H
