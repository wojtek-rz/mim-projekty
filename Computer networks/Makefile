# Makefile

# Compiler
CXX=g++
CXXFLAGS=-Wall -O2 -std=c++20 -L/usr/include/boost
LIBS= -lboost_program_options -pthread

SRC_DIR=src

SOURCES = DatagramConverter.h err.h PackageQueue.h UdpTransmitter.h
EXECUTABLE = sikradio-receiver sikradio-sender
SENDER_FILES = 	src/Sender.cpp src/AtomicQueues.h src/DatagramConverter.h src/Err.h \
				src/Options.h src/Protocols.h src/SenderController.h src/UdpServer.h

RECEIVER_FILES= src/Receiver.cpp src/AtomicQueues.h src/DatagramConverter.h src/Err.h \
				src/Options.h src/Protocols.h src/ReceiverStream.h src/Stations.h \
				src/TcpServer.h src/UdpServer.h src/Telnet.h

all: $(EXECUTABLE)

%.o: $(SRC_DIR)/%.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

sikradio-receiver: $(RECEIVER_FILES)
	$(CXX) $(CXXFLAGS) $(RECEIVER_FILES) -o sikradio-receiver $(LIBS)

sikradio-sender: $(SENDER_FILES)
	$(CXX) $(CXXFLAGS) $(SENDER_FILES) -o sikradio-sender $(LIBS)

clean:
	rm -f *.o $(EXECUTABLE)
