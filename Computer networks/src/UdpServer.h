#ifndef ZADANIE2_UDPSERVER_H
#define ZADANIE2_UDPSERVER_H

#include <cstdint>
#include <cerrno>
#include <memory>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <vector>
#include <iostream>
#include "Err.h"

class UdpServer {
private:
    int socket_fd;
    const int flags = 0;
    struct sockaddr_in send_address{};
    bool check_receive_address = false;
    struct sockaddr_in receive_address{};
    struct ip_mreq ip_mreq_mult{};

    inline void bind_socket_to_address(int socket, struct sockaddr_in address) const {
        // set reuse port
        int optval = 1;
        CHECK_ERRNO(setsockopt(socket, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval)));
        CHECK_ERRNO(bind(socket_fd, (struct sockaddr *) &address,
                         (socklen_t) sizeof(address)));
    }

    inline static bool compare_sockaddr_in(struct sockaddr_in a, struct sockaddr_in b){
        if (a.sin_port == 0 || b.sin_port == 0)
            return a.sin_family == b.sin_family &&
               a.sin_addr.s_addr == b.sin_addr.s_addr;
        else
            return a.sin_family == b.sin_family &&
                    a.sin_addr.s_addr == b.sin_addr.s_addr &&
                    a.sin_port == b.sin_port;
    }

    inline static int create_socket(){
        int sock = socket(AF_INET, SOCK_DGRAM, 0);
        ENSURE(sock >= 0);
        return sock;
    }

    inline static int port_valid(int port){
        if (port < 0 || port > 65535) {
            fatal("Invalid port number: %d", port);
        }
        return port;
    }

    static uint16_t get_port_from_socket(int socket_fd) {
        struct sockaddr_in address{};
        socklen_t address_length = sizeof(address);
        CHECK_ERRNO(getsockname(socket_fd, (struct sockaddr *) &address, &address_length));
        return ntohs(address.sin_port);
    }

    ssize_t _send_message(void *msg, size_t msg_len, struct sockaddr_in *receiver_ptr = nullptr) const {
        struct sockaddr_in receiver{};

        if (receiver_ptr == nullptr) receiver = send_address;
        else receiver = *receiver_ptr;

        auto address_length = static_cast<socklen_t>(sizeof(receiver));
        ssize_t sent_length = sendto(socket_fd, msg, msg_len, flags,
                                     (struct sockaddr *) &receiver, address_length);

        ENSURE(sent_length == (ssize_t) msg_len);
        return sent_length;
    }
public:
    explicit UdpServer(int port = 0) {
        socket_fd = create_socket();

        int optval = 1;
        CHECK_ERRNO(setsockopt(socket_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)));

        if (port != 0){
            port = port_valid(port);
            bind_socket_to_address(socket_fd, get_sockaddr_in_any(port));
        } else {
            bind_socket_to_address(socket_fd, get_sockaddr_in_any(0));
            port = get_port_from_socket(socket_fd);
        }

        std::cerr << "[UDPServer] listening on port " << port << std::endl;
    }

    void allow_broadcast_send() {
        int broadcast_enable = 1;
        CHECK_ERRNO(setsockopt(socket_fd, SOL_SOCKET, SO_BROADCAST, &broadcast_enable,
                                sizeof(broadcast_enable)));
    }

    void set_receive_timeout(long milliseconds) {
        struct timeval tv{};
        tv.tv_sec = milliseconds / 1000;
        tv.tv_usec = (milliseconds % 1000) * 1000;
        CHECK_ERRNO(setsockopt(socket_fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)));
    }

    bool join_multicast_group(const char *mcast_addr){
        /* podłączenie do grupy rozsyłania (ang. multicast) */
        ip_mreq_mult.imr_interface.s_addr = htonl(INADDR_ANY);
        if (inet_aton(mcast_addr, &ip_mreq_mult.imr_multiaddr) == 0) {
            return false;
        }
        if (!is_multicast_address(ip_mreq_mult.imr_multiaddr.s_addr)) {
            return false;
        }
        CHECK_ERRNO(setsockopt(socket_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *) &ip_mreq_mult, sizeof ip_mreq_mult));
        return true;
    }

    void leave_multicast_group(){
        CHECK_ERRNO(setsockopt(socket_fd, IPPROTO_IP, IP_DROP_MEMBERSHIP, (void *) &ip_mreq_mult, sizeof ip_mreq_mult));
    }

    void set_receive_address(const std::string& address, uint16_t p = 0){
        check_receive_address = true;
        receive_address = get_sockaddr_in(address.c_str(), port_valid(p));
    }

    void set_send_address(const std::string& address, uint16_t port){
        send_address = get_sockaddr_in(address.c_str(), port_valid(port));
    }

    ~UdpServer() {
        CHECK_ERRNO(close(socket_fd));
    }

    ssize_t read_message(std::vector<uint8_t> &buffer, struct sockaddr_in *sender_ptr = nullptr) const {
        struct sockaddr_in msg_address{};
        auto address_length = static_cast<socklen_t>(sizeof(msg_address));
        ssize_t len;
        do {
            len = recvfrom(socket_fd, (void *) buffer.data(), (size_t)buffer.size(), flags,
                           (struct sockaddr *) &msg_address, &address_length);
            if (len < 0 && errno == EAGAIN && errno == EWOULDBLOCK){
                return 0;
            } else if (len < 0) {
                PRINT_ERRNO();
            }
        } while (len < 0 && check_receive_address && !compare_sockaddr_in(msg_address, receive_address));

//        std::cerr << "[UDPServer] received " << len << " bytes from " << inet_ntoa(msg_address.sin_addr) <<
//            "port = " << ntohs(msg_address.sin_port) << std::endl;

        if (sender_ptr != nullptr) *sender_ptr = msg_address;
        return len;
    }

    ssize_t send_message(const std::vector<uint8_t> &message, struct sockaddr_in *receiver_ptr = nullptr) const {
        return _send_message((void *) message.data(), message.size(), receiver_ptr);
    }

    ssize_t send_message(const std::string &message, struct sockaddr_in *receiver_ptr = nullptr) const {
        return _send_message((void *) message.c_str(), message.size(), receiver_ptr);
    }

    static bool is_multicast_address(in_addr_t s_addr)
    {
        //in_addr_t stored in network order
        uint32_t address = ntohl(s_addr); 

        return (address & 0xF0000000) == 0xE0000000;
    }

    static struct sockaddr_in get_sockaddr_in(char const *host, uint16_t port) {
        struct addrinfo hints{};
        struct sockaddr_in address{};

        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_INET; // IPv4
        hints.ai_socktype = SOCK_DGRAM;
        hints.ai_protocol = 0;

        struct addrinfo *address_result;
        CHECK(getaddrinfo(host, nullptr, &hints, &address_result));

        address.sin_family = AF_INET; // IPv4
        address.sin_addr.s_addr =
                ((struct sockaddr_in *) (address_result->ai_addr))->sin_addr.s_addr; // IP address
        address.sin_port = htons(port); // port from the command line

        freeaddrinfo(address_result);
        return address;
    }

    static struct sockaddr_in get_sockaddr_in_any(int port){
        struct sockaddr_in address{};
        address.sin_family = AF_INET; // IPv4
        address.sin_addr.s_addr = htonl(INADDR_ANY); // listening on all interfaces
        address.sin_port = htons(port);
        return address;
    }

};



#endif //ZADANIE2_UDPSERVER_H