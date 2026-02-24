raw = b"""GET /path?x=1 HTTP/1.1\r
Host: sudanchapagain.com.np\r
User-Agent: test\r
\r
body=meow_meow_biralo
"""

lines = raw.split(b"\r\n")
request_line = lines[0].decode()
method, path, version = request_line.split()

headers = {}
i = 1
while lines[i]:
    key, value = lines[i].decode().split(":", 1)
    headers[key.strip()] = value.strip()
    i += 1

body = b"\r\n".join(lines[i + 1 :])

print(method, path, version)
print(headers)
print(body)
