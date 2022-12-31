import subprocess
import os
import threading

# Global variable to be shared by multiple threads
global_tasks = 0
# Mutex to synchronize access to the global variable
mutex = threading.Lock()

def thread_main():
    while True:
        # Do something here
        print('Hello from the while loop')


thread = threading.Thread(target=thread_main)
thread.start()

# Wait for the threads to complete
thread.join()


os.chdir('build_tests')
# Start the child process
process = subprocess.Popen(['program_name'], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

input_data = 'run ./task1\n'
process.stdin.write(input_data.encode())
process.stdin.flush()


process.sdout.write()