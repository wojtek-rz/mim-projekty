import unittest
import sys
from sender_args_tests import SenderArgsTests
from sender_ctrl_tests import SenderCtrlTests

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test1.py <path_to_dir_with_receiver>")
        print("Usage2: python3 test1.py <path_to_dir_with_receiver> <TestClass.test_method>")
        exit(1)

    unittest.main(argv=sys.argv[1:], verbosity=2)