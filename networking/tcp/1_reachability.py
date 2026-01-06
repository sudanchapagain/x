import socket


def tcp_reachable(host, port, timeout=2):
    try:
        with socket.create_connection((host, port), timeout):
            return True
    except OSError:
        return False

print(tcp_reachable("sudanchapagain.com.np", 80))
