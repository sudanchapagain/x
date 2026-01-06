import ipaddress

ip = ipaddress.ip_address("192.168.0.1")

print(ip.version)
print(ip.is_loopback)
print(ip.is_private)
print(ip.is_multicast)
print(ip.is_link_local)
