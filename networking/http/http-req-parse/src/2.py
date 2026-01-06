from http.server import BaseHTTPRequestHandler
from io import BytesIO

class Request(BaseHTTPRequestHandler):
    def __init__(self, request_text):
        self.rfile = BytesIO(request_text)
        self.raw_requestline = self.rfile.readline()
        self.error_code = self.error_message = None
        self.parse_request()

raw = b"POST /submit HTTP/1.1\r\nHost: sudanchapagain.com.np\r\n\r\nmeow"
req = Request(raw)

print(req.command)
print(req.path)
print(req.headers)

