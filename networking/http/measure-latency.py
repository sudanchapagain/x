import socket
import ssl
import time

host = "sudanchapagain.com.np"
port = 8000

start = time.perf_counter()

sock = socket.create_connection((host, port))

tcp_time = time.perf_counter()

context = ssl.create_default_context()

ssock = context.wrap_socket(sock, server_hostname=host)

tls_time = time.perf_counter()

ssock.sendall(b"GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")

ssock.recv(1024)

end = time.perf_counter()

print("TCP:", (tcp_time - start) * 1000)
print("TLS:", (tls_time - tcp_time) * 1000)
print("HTTP:", (end - tls_time) * 1000)
