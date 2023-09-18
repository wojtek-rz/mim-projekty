#ifndef AUDIO_H
#define AUDIO_H

#include <cstdint>
#include <cstddef>
#include <arpa/inet.h>
#include <vector>
#include <cstring>
#include <ctime>
#include <stdexcept>

#define htonll(x) ((1==htonl(1)) ? (x) : ((uint64_t)htonl((x) & 0xFFFFFFFF) << 32) | htonl((x) >> 32))
#define ntohll(x) ((1==ntohl(1)) ? (x) : ((uint64_t)ntohl((x) & 0xFFFFFFFF) << 32) | ntohl((x) >> 32))

namespace Audio {
    struct datagram_t {
        uint64_t session_id;
        uint64_t first_byte_num;
        std::vector<uint8_t> payload;
    };
    typedef struct datagram_t datagram_t;

    void deserialize_package(datagram_t *package, std::vector<uint8_t> &bytes) {
        if (bytes.size() < 16)
            throw std::runtime_error("Invalid package size");


        uint64_t session_id_net;
        uint64_t first_byte_num_net;
        std::memcpy(&session_id_net, bytes.data(), 8);
        std::memcpy(&first_byte_num_net, bytes.data() + 8, 8);

        package->session_id = ntohll(session_id_net);
        package->first_byte_num = ntohll(first_byte_num_net);

        package->payload.resize(bytes.size() - 16);
        std::memcpy(package->payload.data(), bytes.data() + 16, bytes.size() - 16);
    }

    void serialize_package(datagram_t *package, std::vector<uint8_t> &bytes) {
        bytes.resize(package->payload.size() + 16);

        uint64_t session_id_net = htonll(package->session_id);
        uint64_t first_byte_num_net = htonll(package->first_byte_num);

        std::memcpy(bytes.data(), &session_id_net, 8);
        std::memcpy(bytes.data() + 8, &first_byte_num_net, 8);
        std::memcpy(bytes.data() + 16, package->payload.data(), package->payload.size());
    }
}

#endif //AUDIO_H
