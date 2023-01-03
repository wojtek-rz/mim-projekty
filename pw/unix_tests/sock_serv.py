import socket
import threading
import subprocess
from time import sleep
import time
import statistics

N_THREADS = 400
N_LINES = 100

all_clients_started = threading.Event()
clients_ended = threading.Barrier(N_THREADS + 1)
mean = []


class ExecutorThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.mutex = threading.Lock()
    
    def send_to_executor(self, msg):
        with self.mutex:
            # print(msg)
            msg = msg + "\n"
            self.process.stdin.write(msg.encode())
            self.process.stdin.flush()
            output = self.process.stdout.readline().decode()
            print(output[:-1])

        return output
    
    def get_from_executor(self):
        with self.mutex:
            output = self.process.stdout.readline().decode()
            print(output[:-1])

        return output
    
    def check_executor(self, task_id, msg):
        output = self.send_to_executor(f"out {task_id}")
        expected = f"Task {task_id} stdout: '{msg}'.\n"
        i = 1
        while output != expected:
            print(f'got {output[:-1]} but wanted {expected[:-1]}')
            i += 1
            output = self.send_to_executor(f"out {task_id}")
        
        # print(f"Task {task_id} got '{msg}' after {i} tries")
        

    def run(self):
        # run the command as a subprocess
        with self.mutex:
            self.process = subprocess.Popen(["./build_tests/executor"],  stdin=subprocess.PIPE, stdout=subprocess.PIPE)

        for i in range(N_THREADS):
            print(self.send_to_executor(f"run python3 sock_client.py {i}")[:-1])
        all_clients_started.set()

        clients_ended.wait()

        clients_ended_array = [False] * N_THREADS
        for i in range(N_THREADS):
            output = self.get_from_executor()
            if 'ended: status 0.\n' in output:
                task_id = int(output.split(' ')[1])
                clients_ended_array[task_id] = True
        assert(all(clients_ended_array))
        print(f"Mean tasks waiting time for line update: {mean:.6}.")


executor_thread = ExecutorThread()
executor_thread.start()

class ClientThread(threading.Thread):
    def __init__(self, client_address, client_socket, task_id):
        threading.Thread.__init__(self)
        self.csocket = client_socket
        self.task_id = int(self.wait_for_data_recived()) # first msg is task_id
        self.elapsed_times = []
        self.msgs = [f"line {i} from {task_id}" for i in range(N_LINES)]
        # print(f"New connection from {client_address}")


    def wait_for_data_recived(self):
        return self.csocket.recv(1024)
        # print(data.decode())

    def send_to_client(self, msg):
        self.csocket.sendall(msg.encode())

    def run(self):

        all_clients_started.wait()
        for msg in self.msgs:
            
            self.send_to_client(msg)

            start_time = time.perf_counter()
            executor_thread.check_executor(self.task_id, msg)
            end_time = time.perf_counter()

            # Calculate elapsed time
            elapsed_time = end_time - start_time
            self.elapsed_times.append(elapsed_time)
            
            self.wait_for_data_recived()
        
        clients_ended.wait()
        self.send_to_client('CLOSE')


# create a socket object
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# get local machine name
host = socket.gethostname()

port = 9999

# bind to the port
server_socket.bind((host, port))

# queue up to 5 requests
server_socket.listen(5)


threads = []
for i in range(N_THREADS):
    client_socket, client_address = server_socket.accept()
    new_thread = ClientThread(client_address, client_socket, i)
    threads.append(new_thread)
    new_thread.start()


for thread in threads:
    thread.join()

means = [statistics.mean(thread.elapsed_times) for thread in threads]
mean = statistics.mean(means)

executor_thread.join()

server_socket.close()
