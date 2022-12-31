import socket

# Create a client socket
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client_socket.connect(('localhost', 8000))

# Send a message to the server
message = 'Hello from the client'
client_socket.send(message.encode())

# Receive a message from the server
message = client_socket.recv(1024).decode()
print(f'Received message from server: {message}')

# Close the socket
client_socket.close()
