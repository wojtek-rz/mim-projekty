#include <iostream>
#include <boost/program_options.hpp>
#include <ctime>
#include <thread>
#include "SenderController.h"
#include "UdpServer.h"
#include "DatagramConverter.h"
#include "Options.h"

size_t read_n_bytes(int fd, uint8_t *buf, size_t n) {
    size_t len, tlen;
    len = 0; tlen = 0;
    do {
        tlen = read(fd, buf + len, n - len);
        len += tlen;
    } while (len < n && tlen > 0);
    return len;
}
using namespace std;
int main(int argc, char *argv[]) {
    auto options = make_shared<SenderOptions>();
    if (!options->parse_options(argc, argv)) return 1;

    UdpServer sender;
    sender.allow_broadcast_send();
    sender.set_send_address(options->mcast_addr, options->data_port);

    options->session_id = time(nullptr);
    Audio::datagram_t audio_package{(uint64_t)options->session_id, 0, std::vector<uint8_t>(options->psize, 0)};

    auto send_queue = make_shared<AtomicSendQueue>(options->fsize, options->psize);
    Controller controller(options, send_queue);

    std::vector<uint8_t> msg;
    ssize_t len;
    do {
        len = (ssize_t)read_n_bytes(STDIN_FILENO, audio_package.payload.data(), options->psize);

        if (len == options->psize) {
            Audio::serialize_package(&audio_package, msg);
            sender.send_message(msg);
            send_queue->push_data(audio_package.first_byte_num, audio_package.payload);
//            std::cerr << "Sent package: first_byte_num: " << audio_package.first_byte_num << ", payload_size:  " << audio_package.payload.size() << std::endl;
            audio_package.first_byte_num += len;
        }
    } while (len == options->psize);


    return 0;
}
