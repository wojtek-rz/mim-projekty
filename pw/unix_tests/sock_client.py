import socket
import sys
import time

# create a socket object
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# get local machine name
host = socket.gethostname()

port = 9999

# connection to hostname on the port.
client_socket.connect((host, port))

task_id = sys.argv[1]
client_socket.sendall(task_id.encode())

# receive data from the server
while True:
    data = client_socket.recv(1024)
    if not data or data.decode() == "CLOSE":
        break
    
    print(f"{data.decode()}", flush=True)
    client_socket.sendall(b'ODBIOR')
    

client_socket.close()