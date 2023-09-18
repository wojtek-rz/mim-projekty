import subprocess
from utils import *
import sys
from time import sleep
import unittest

debug = True

SAMPLE_BYTES = bytes([0x02] * 256 + [0x01] * 256)

class SenderCtrlTests(unittest.TestCase):
    def clear_up(self, process, *servers):
        for server in servers:
            server.socket.close()
        
        process.kill()
        stdout, stderr = process.communicate()
        if debug:
            print_debug(stdout, stderr)
    
    def test01a_lookup(self):
        MCAST_ADDR = "239.10.11.12"
        process = runSender(MCAST_ADDR)

        sleep(0.2)
        udpServer = UDPServer("localhost", CTRL_PORT)
        udpServer.set_receiver_address("255.255.255.255", CTRL_PORT)
        udpServer.send(b"ZERO_SEVEN_COME_IN\n")
        sleep(0.5)

        try:
            msg = udpServer.recive(1024, 0.4)
        except:
            self.clear_up(process, udpServer)
            self.fail("No data recived back, timeout")
        
        self.clear_up(process, udpServer)
        correct_msg = f"BOREWICZ_HERE {MCAST_ADDR} {DATA_PORT} {NAME}\n"
        self.assertEqual(msg, correct_msg.encode("utf-8"))

    def test01b_lookup(self):
        MCAST_ADDR = "239.10.11.13"
        DATA_PORT = 20001
        NAME = '""'
        process = runSender(MCAST_ADDR, data_port=DATA_PORT, name=NAME)

        sleep(0.2)
        udpServer = UDPServer("localhost", CTRL_PORT)
        udpServer.set_receiver_address("255.255.255.255", CTRL_PORT)
        udpServer.send(b"ZERO_SEVEN_COME_IN\n")
        sleep(0.5)

        try:
            msg = udpServer.recive(1024, 0.4)
        except:
            self.clear_up(process, udpServer)
            self.fail("No data recived back")
        
        self.clear_up(process, udpServer)
        correct_msg = f"BOREWICZ_HERE {MCAST_ADDR} {DATA_PORT} {NAME}\n"
        self.assertEqual(msg, correct_msg.encode("utf-8"))

    def test02a_rexmit(self):
        MCAST_ADDR = "239.10.11.12"
        PSIZE = 16
        SAMPLE_BYTES = bytes([0x02] * 8 + [0x01] * 8)
        SAMPLE_BYTES2 = bytes([0x03] * 8 + [0x04] * 8)
        SAMPLE_BYTES3 = bytes([0x05] * 8 + [0x06] * 8)

        process = runSender(MCAST_ADDR, psize = PSIZE)
        sleep(0.2)

        udpServerCtrl = UDPServer("localhost", 0)
        udpServerCtrl.set_receiver_address("localhost", CTRL_PORT)

        udpServerData = UDPServer('', DATA_PORT)
        udpServerData.add_to_multicast_group(MCAST_ADDR)

        process.stdin.write(SAMPLE_BYTES)
        process.stdin.write(SAMPLE_BYTES2)
        process.stdin.write(SAMPLE_BYTES3)
        process.stdin.flush()

        sleep(0.5)

        try:
            for i in range(3):
                normal_bytes = udpServerData.recive(PSIZE + 16, 0.5)
        
            sleep(0.2)
            udpServerCtrl.send(f"LOUDER_PLEASE {0}, {PSIZE * 2}, {PSIZE}, {0}\n".encode("utf-8"))
            sleep(0.2)

            bytes_recived_1 = udpServerData.recive((PSIZE + 16) * 5, 0.5)
            bytes_recived_2 = udpServerData.recive((PSIZE + 16) * 5, 0.5)
            bytes_recived_3 = udpServerData.recive((PSIZE + 16) * 5, 0.5)

            # decode first 8 bytes as number
            first_byte_num_1 = int.from_bytes(bytes_recived_1[8:16], "big")
            first_byte_num_2 = int.from_bytes(bytes_recived_2[8:16], "big")
            first_byte_num_3 = int.from_bytes(bytes_recived_3[8:16], "big")


        except:
            self.clear_up(process, udpServerCtrl, udpServerData)
            self.fail("No data recived")

        self.clear_up(process, udpServerCtrl, udpServerData)

        self.assertEqual({(first_byte_num_1, bytes_recived_1[16:]), 
                          (first_byte_num_2, bytes_recived_2[16:]),
                          (first_byte_num_3, bytes_recived_3[16:])}, 
                          { (0, SAMPLE_BYTES),
                            (PSIZE, SAMPLE_BYTES2), 
                            (PSIZE * 2, SAMPLE_BYTES3)})
        self.assertEqual(normal_bytes[:8], bytes_recived_1[:8], "session_id is not the same")
        self.assertEqual(normal_bytes[:8], bytes_recived_2[:8], "session_id is not the same")
        self.assertEqual(normal_bytes[:8], bytes_recived_3[:8], "session_id is not the same")

    def test02b_rexmit(self):
        MCAST_ADDR = "239.10.11.12"
        PSIZE = 16
        SAMPLE_BYTES = bytes([0x02] * 8 + [0x01] * 8)

        process = runSender(MCAST_ADDR, psize = PSIZE)

        udpServerCtrl = UDPServer("localhost", 0)
        udpServerCtrl.set_receiver_address("localhost", CTRL_PORT)

        udpServerData = UDPServer(MCAST_ADDR, DATA_PORT)
        udpServerData.add_to_multicast_group(MCAST_ADDR)


        for i in range(4):
            process.stdin.write(SAMPLE_BYTES)
            process.stdin.flush()

        sleep(0.5)

        try:
            bytes_recived = []
            for i in range(4):
                bytes_recived.append(udpServerData.recive(PSIZE + 16, 0.5))
            
            sleep(0.2)
            udpServerCtrl.send(f"LOUDER_PLEASE 15000,-32,16a\n".encode("utf-8"))
            sleep(0.5)

            for i in range(4):
                process.stdin.write(SAMPLE_BYTES)
                process.stdin.flush()

            # program should not break
            for i in range(4):
                bytes_recived.append(udpServerData.recive((PSIZE + 16), 0.5))

        except:
            self.clear_up(process, udpServerData, udpServerCtrl)
            self.fail("No data recived")
        
        self.clear_up(process, udpServerData, udpServerCtrl)
        first_byte_nums = [int.from_bytes(x[8:16], "big") for x in bytes_recived]
        self.assertEqual(first_byte_nums, [i * PSIZE for i in range(8)])


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test1.py <path_to_dir_with_receiver>")
        print("Usage2: python3 test1.py <path_to_dir_with_receiver> <TestClass.test_method>")
        exit(1)

    unittest.main(argv=sys.argv[1:], verbosity=2)