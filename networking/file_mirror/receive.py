import socket
import struct

server = socket.socket()
server.bind(("0.0.0.0", 9000))
server.listen()

conn, _ = server.accept()

with open("copy.bin", "wb") as f:
    while True:
        hdr = conn.recv(4)
        if not hdr:
            break
        size = struct.unpack("!I", hdr)[0]
        data = conn.recv(size)
        f.write(data)
