#!/usr/bin/python

import os
import sys
import subprocess


walk_dir = sys.argv[1]

print("this is walk_dir" + walk_dir)

for root, subdirs, files in os.walk(walk_dir):
#    print("patrze na testy:  " + root)
    for file in files:
        if file.endswith(".in"):
            print("current dir: " + root)
            os.system("time " + "./test_no_verbose.sh main.exe " + root + "/")
            break