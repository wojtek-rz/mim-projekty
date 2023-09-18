#ifndef ZADANIE2_TCPSERVER_H
#define ZADANIE2_TCPSERVER_H

#include <poll.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <vector>
#include <functional>
#include "Err.h"
#include "Telnet.h"

class TcpServer {
    std::vector<struct pollfd> pollfds;
    std::vector<std::vector<uint8_t>> waiting_messages;
    std::function<std::string()> generate_intro_msg = nullptr;

    void add_to_poll_descriptors(int fd) {
        pollfds.push_back({fd, POLLIN, 0});
    }

    void check_for_accepts() {
        if (pollfds[0].revents & POLLIN) {
            int sock = accept(pollfds[0].fd, nullptr, nullptr);
            ENSURE(sock >= 0);

            add_to_poll_descriptors(sock);
            if (generate_intro_msg != nullptr) {
                std::string msg = generate_intro_msg();
                (void)!write(sock, msg.c_str(), msg.size());
            }
        }
    }

public:
    explicit TcpServer(int port) {
        struct sockaddr_in addr{};
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        addr.sin_addr.s_addr = htonl(INADDR_ANY);

        int sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
        ENSURE(sock >= 0);
        CHECK_ERRNO(bind(sock, (struct sockaddr *)&addr, sizeof(addr)));
        CHECK_ERRNO(listen(sock, 5));
        std::cerr << "[TCPServer] listening on port " << port << std::endl;
        add_to_poll_descriptors(sock);
    }

    void add_introduction_msg(const std::function<std::string()> &callback) {
        generate_intro_msg = callback;
    }

    ~TcpServer() {
        std::cerr << "[TCPServer] closing connections" << std::endl;
        for (size_t i = 0; i < pollfds.size(); i++) {
            close(pollfds[i].fd);
        }
    }

    std::string get_new_message(long timeout = -1) {
        for (size_t i = 0; i < pollfds.size(); i++) {
            pollfds[i].revents = 0;
        }

        while (waiting_messages.empty())
        {
            int poll_status = poll(pollfds.data(), pollfds.size(), (int)timeout);

            if (poll_status == 0) {
                return "";
            }

            check_for_accepts();

            std::vector<size_t> closed_connections;
            std::vector<uint8_t> data(1024);
            for (size_t i = 1; i < pollfds.size(); i++) {
                if (pollfds[i].revents & (POLLIN | POLLERR)) {
                    data.resize(1024);
                    ssize_t bytes_read = read(pollfds[i].fd, data.data(), data.size());

                    if (bytes_read == 0) {
                        closed_connections.push_back(i);
                    }
                    else if (bytes_read > 0) {
                        data.resize(bytes_read);
                        waiting_messages.push_back(data);
                    }
                }
            }

            for (int i = (int)closed_connections.size() - 1; i >= 0; i--) {
                close(pollfds[closed_connections[i]].fd);
                pollfds.erase(pollfds.begin() + (long)closed_connections[i]);
            }
        }

        std::string msg(waiting_messages.back().begin(), waiting_messages.back().end());
        waiting_messages.pop_back();
        return msg;
    }

    void send_message(const std::string &msg) {
        for (size_t i = 1; i < pollfds.size(); i++) {
            (void)!write(pollfds[i].fd, msg.c_str(), msg.size());
        }
    }
};

#endif //ZADANIE2_TCPSERVER_H