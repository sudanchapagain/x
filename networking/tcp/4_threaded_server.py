import socket
import threading


def handle(conn):
    data = conn.recv(1024)
    conn.sendall(b"OK" + data)
    conn.close()


server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.bind(("0.0.0.0", 8080))
server.listen()

while True:
    conn, addr = server.accept()
    threading.Thread(target=handle, args=(conn,)).start()
