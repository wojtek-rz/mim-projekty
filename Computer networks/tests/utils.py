from time import sleep
import socket
import struct
import subprocess
import sys

SLEEP_BETWEEN_MSGS = 0.005

class UDPServer:
    def __init__(self, address, port):
        # if listening to broadcast, address should be broadcast address
        self.socket = socket.socket(socket.AF_INET, # Internet
                socket.SOCK_DGRAM) # UDP
        
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
        self.socket.bind((address, port))

        self.address = self.socket.getsockname()[0]
        self.port = int(self.socket.getsockname()[1])


    def add_to_multicast_group(self, address):
        mreq = struct.pack("4sl", socket.inet_aton(address), socket.INADDR_ANY)
        self.socket.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)

    def set_receiver_address(self, address, port):
        self.receiver_address = address
        self.receiver_port = port

    def send(self, message):
        sleep(SLEEP_BETWEEN_MSGS)
        self.socket.sendto(message, (self.receiver_address, self.receiver_port))

    def recive(self, size, timeout = 0):
        if timeout > 0:
            self.socket.settimeout(timeout)
        
        data = self.socket.recv(1024)
        return data

#   0                                63
#  +-----------------------------------+
#  |            Session id             |
#  +-----------------------------------+
#  |          First byte num           | 
#  +-----------------------------------+  
#  |          data octets ...          | 
#  +---------------- ...               
def get_message(session_id: int, first_byte_num: int, data: list) -> bytes:
    # treat session_id as 64-bytes value and convert it to network bytes order
    session_id = session_id.to_bytes(8, byteorder='big')
    first_byte_num = first_byte_num.to_bytes(8, byteorder='big')
    data = bytes(data)
    return session_id + first_byte_num + data



# ==================== CONFIG ====================
album_nr = 438709
DATA_PORT = 20000 + (album_nr % 10000)
CTRL_PORT = 30000 + (album_nr % 10000)
PSIZE = 512
FSIZE = 131072
RTIME = 250
NAME = "Nienazwany Nadajnik"
# ODBIORNIK
DISCOVER_ADDR = "255.255.255.255"
UI_PORT = 10000 + (album_nr)
BSIZE = 65536


def runSender(mcast_addr, 
              data_port = DATA_PORT,
              ctrl_port = CTRL_PORT,
              psize = PSIZE,
              fsize = FSIZE,
              rtime = RTIME,
              name = NAME):
    path = sys.argv[1]
    return subprocess.Popen([path + "/sikradio-sender",
                                "-a", mcast_addr,
                                "-P", str(data_port),
                                "-C", str(ctrl_port),
                                "-p", str(psize),
                                "-f", str(fsize),
                                "-R", str(rtime),
                                "-n", name],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                stdin=subprocess.PIPE)


def runReceiver(discover_addr = DISCOVER_ADDR,
              ctrl_port = CTRL_PORT,
              bsize = BSIZE,
              rtime = RTIME,
              name = ""):
    path = sys.argv[1]
    if name == "":
        return subprocess.Popen([path + "/sikradio-sender",
                                    "-a", mcast_addr,
                                    "-P", str(data_port),
                                    "-C", str(ctrl_port),
                                    "-p", str(psize),
                                    "-f", str(fsize),
                                    "-R", str(rtime),
                                    "-n", name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    stdin=subprocess.PIPE)
    else:
        pass
        


def print_debug(stdout, stderr):
    print()
    print("============== stdout ================")
    if (stdout != None):
        print(stdout.decode("utf-8"))
    else:
        print("None")
    
    print("============== stderr ================")
    if (stderr != None):
        print(stderr.decode("utf-8"))
    else:
        print("None")
    