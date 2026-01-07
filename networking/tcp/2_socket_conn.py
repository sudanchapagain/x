import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("example.com", 80))

s.sendall(b"GET / HTTP/1.1\r\nHost: sudanchapagain.com.np\r\n\r\n")
data = s.recv(4096)

print(data.decode(errors="ignore"))
s.close()
