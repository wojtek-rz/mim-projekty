cmake_minimum_required(VERSION 3.21)
project(zadanie2)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_FLAGS -Wall)

add_executable(sikradio-sender src/Sender.cpp src/UdpServer.h src/Err.h src/DatagramConverter.h src/AtomicQueues.h src/SenderController.h)
add_executable(sikradio-receiver src/Receiver.cpp src/UdpServer.h src/Err.h src/DatagramConverter.h src/AtomicQueues.h src/Telnet.h src/TcpServer.h src/ReceiverStream.h)

find_package(Boost COMPONENTS program_options REQUIRED)
target_link_libraries(sikradio-sender Boost::program_options)
target_link_libraries(sikradio-sender pthread)
target_link_libraries(sikradio-receiver Boost::program_options)
target_link_libraries(sikradio-receiver pthread)