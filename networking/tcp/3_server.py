import socket

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.bind(("0.0.0.0", 8080))
server.listen()

while True:
    # single conn at a time
    # blocking
    conn, addr = server.accept()
    data = conn.recv(1024)
    conn.sendall(b"OK")
    conn.close()
