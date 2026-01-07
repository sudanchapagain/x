import socket
import struct

sock = socket.create_connection(("receiver", 9000))

with open("data.bin", "rb") as f:
    while chunk := f.read(4096):
        sock.sendall(struct.pack("!I", len(chunk)) + chunk)
