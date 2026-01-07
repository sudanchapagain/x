import socket
import select

server = socket.socket()
server.bind(("0.0.0.0", 8080))
server.listen()
server.setblocking(False)

sockets = [server]

while True:
    readable, _, _ = select.select(sockets, [], [])
    for s in readable:
        if s is server:
            conn, _ = server.accept()
            conn.setblocking(False)
            sockets.append(conn)
        else:
            data = s.recv(1024)
            if not data:
                sockets.remove(s)
                s.close()
            else:
                s.sendall(b"OK")
