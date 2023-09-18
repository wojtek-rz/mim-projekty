import subprocess
from utils import *
import sys
from time import sleep
import unittest

# ==================== CONFIG ====================
album_nr = 438709
MCAST_ADDR = "239.10.11.12"
DATA_PORT = 20000 + (album_nr % 10000)
CTRL_PORT = 30000 + (album_nr % 10000)
PSIZE = 512
FSIZE = 131072
RTIME = 250
NAME = "Nienazwany Nadajnik"

SAMPLE_BYTES = bytes([0x02] * 256 + [0x01] * 256)

class SenderArgsTests(unittest.TestCase):

    def sender_should_fail(self, mcast_addr, **kwargs):
        process = runSender(mcast_addr, **kwargs)
        sleep(0.1)
        
        try:
            stdout, stderr = process.communicate(SAMPLE_BYTES * 20, timeout=0.5)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
            print_debug(stderr, stdout)

            self.fail("Sender should fail with args: " + str(kwargs))
        
        print_debug(stderr, stdout)

        self.assertEqual(process.returncode, 1, "wrong return code")

    def sender_should_ok(self, mcast_addr, **kwargs):
        process = runSender(mcast_addr, **kwargs)
        sleep(0.1)
        
        try:
            stdout, stderr = process.communicate(SAMPLE_BYTES * 20, timeout=1.5)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
            print_debug(stderr, stdout)

            self.fail("Sender should ok with args: " + str(kwargs))
        
        print_debug(stderr, stdout)

        self.assertEqual(process.returncode, 0, "wrong return code")

    def test01a(self):
        self.sender_should_fail(mcast_addr="localhost")

    def test01b(self):
        self.sender_should_fail(mcast_addr="192.168.0.1")
    
    def test01c(self):
        self.sender_should_fail(mcast_addr="?")
    
    def test01d(self):
        self.sender_should_fail(mcast_addr="15578.32154152.3124152.21390109")

    def test02a(self):
        self.sender_should_fail(MCAST_ADDR, data_port=0)

    def test02b(self):
        self.sender_should_fail(MCAST_ADDR, data_port=65536)
    
    def test02c(self):
        self.sender_should_fail(MCAST_ADDR, data_port=-1)
    
    def test02d(self):
        self.sender_should_fail(MCAST_ADDR, data_port="2a")

    def test03a(self):
        self.sender_should_fail(MCAST_ADDR, ctrl_port=0)
    
    def test03b(self):
        self.sender_should_fail(MCAST_ADDR, ctrl_port=65536)

    def test03c(self):
        self.sender_should_fail(MCAST_ADDR, ctrl_port=-1)

    def test03d(self):
        self.sender_should_fail(MCAST_ADDR, ctrl_port="2a")

    def test04a(self):
        self.sender_should_fail(MCAST_ADDR, psize=0)

    def test04b(self):
        self.sender_should_fail(MCAST_ADDR, psize=65507 - 15) # 16 bytes for header

    def test04c(self):
        self.sender_should_fail(MCAST_ADDR, psize=-1)
    
    def test04d(self):
        self.sender_should_fail(MCAST_ADDR, psize="2a")
    
    def test05a(self):
        self.sender_should_ok(MCAST_ADDR, fsize=0)
    
    def test05b(self):
        self.sender_should_fail(MCAST_ADDR, fsize=-1)
    
    def test05c(self):
        self.sender_should_fail(MCAST_ADDR, fsize="2a")
    
    def test05d(self):
        self.sender_should_fail(MCAST_ADDR, fsize=2.5)

    def test05e(self):
        self.sender_should_fail(MCAST_ADDR, fsize="124155034093142312841211413214523")
    
    def test06(self):
        self.sender_should_ok(MCAST_ADDR, fsize=10, psize=11)
    
    def test07a(self):
        self.sender_should_fail(MCAST_ADDR, rtime=0)
    
    def test07b(self):
        self.sender_should_fail(MCAST_ADDR, rtime=-1)

    def test07c(self):
        self.sender_should_fail(MCAST_ADDR, rtime="2a")

    def test07d(self):
        self.sender_should_fail(MCAST_ADDR, rtime=2.5)
    
    def test08a(self):
        self.sender_should_fail(MCAST_ADDR, rtime="1234565432222222223455334543224564332434345256")

    def test09(self):
        self.sender_should_fail(MCAST_ADDR, name="")

    def test10(self):
        process = subprocess.Popen([sys.argv[1] + "/sikradio-sender"])
        sleep(0.1)

        try:
            stdout, stderr = process.communicate(SAMPLE_BYTES * 20, timeout=0.5)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
            print_debug(stderr, stdout)

            self.fail("Sender should fail without args")
        
        print_debug(stderr, stdout)
        self.assertEqual(process.returncode, 1, "wrong return code")
        

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test1.py <path_to_dir_with_receiver>")
        print("Usage2: python3 test1.py <path_to_dir_with_receiver> <TestClass.test_method>")
        exit(1)

    unittest.main(argv=sys.argv[1:], verbosity=2)