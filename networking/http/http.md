# HTTP

The Hypertext Transfer Protocol (HTTP) is an application layer protocol used
for transmitting hypermedia documents, such as HTML. It is stateless, meaning
each request from a client is treated as an independent transaction, without any
context from previous requests.

<https://developer.mozilla.org/en-US/docs/Web/HTTP/Resources_and_specifications>

A socket is an endpoint for sending or receiving data across a computer
network. It is defined by an IP address and a port number. In this case,
we're using TCP sockets for reliable communication. The Transmission Control
Protocol (TCP) is part of the Internet Protocol Suite (TCP/IP). It is a
connection-oriented protocol that ensures data is sent and received reliably and
in order.

An HTTP request typically includes:

- Method: (e.g., GET, POST) indicates the desired action.
  <https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods>

- Path: The resource being requested (e.g., `/index.html`).

- Headers: Additional information about the request (e.g., `Accept-Encoding`).
  <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers>

- Body: (optional) Contains data sent by the client, primarily in POST requests.

An HTTP response includes:

- Status Line: Indicates the status of the response (e.g., `HTTP/1.1 200 OK`).
  <https://developer.mozilla.org/en-US/docs/Web/HTTP/Status>

- Headers: Information about the response (e.g., `Content-Type`, `Content-Encoding`).

- Body: The actual content being returned (e.g., HTML, JSON).

flow of network
---------------

HTTP -> TLS -> TCP -> IP -> NIC -> Router

you call `get https://www.example.com` from your code or script.
let’s say the above is nu shell.

nu parses the url and figures out:

1. it has to make an HTTP GET request
2. it needs to resolve the domain name
3. it's HTTPS, so TLS handshake is required

nu shell doesn't implement HTTP, TLS, or DNS itself. it uses lower-level
system libs (like `libcurl`, `reqwest`, sockets).

*dns resolution*

first step is dns resolution for the url host. `getaddrinfo()` is called
(via glibc or musl). it resolves the domain name to an IP address.

for this the following is done

- check `/etc/hosts`
- check local DNS cache
- if not found, send UDP packet to DNS server (`8.8.8.8` or from `/etc/resolv.conf`)

DNS request/response happens over UDP (port 53), handled by kernel’s networking
stack. DNS server replies with an IP, e.g. `93.184.216.34`

*tcp connection*

now that we have IP of the url. the client calls `connect()` with the resolved
IP + port (443 for HTTPS). for this the kernel does the following

- allocates socket
- starts TCP 3-way handshake:
    - SYN -> SYN-ACK -> ACK
- assigns ephemeral port
- sets sequence/ack numbers
- opens TCP socket

packets are handed to NIC driver for transmission

*tls handshake*

TLS happens after TCP is connected. libs like OpenSSL or Rustls takes over:

- client sends `ClientHello`
- server sends `ServerHello`, certs
- key exchange (e.g., ECDHE)
- session keys negotiated
- encrypted tunnel established

now that we have a connection. nu or the lib it is using (most likely reqwest
or libcurl) builds the http request.

```txt
GET / HTTP/1.1
Host: www.example.com
User-Agent: Nu/Shell
```

this is then encrypted with TLS
-> wrapped in TCP segment
-> inside IP packet
-> into Ethernet frame
-> to NIC

*nic and physical layer*

NIC takes Ethernet frame and converts the binary data to electrical signals
(ethernet) or to radio waves (wifi) which is sent local router/switch.

*router and forwarding*

router receives frame then reads Ethernet header then reads IP header to check
dest IP. it decrements TTL and forwards to next hop until it reaches web
server.

---

the server receives packets. then following happens

TCP reassembly -> TLS decryption -> HTTP request parsing

server sends HTTP response (HTML, JSON, etc.) and it goes through same layers
in reverse

to implement http server
------------------------

- bind to a specific ip and port.
- use a loop to listen for incoming connections from clients.
- read data from incoming connection and parse it
- based on the incoming information perform approprite action
- send appropriate request back.

additional things that can/should be implemented

- logging
- use thread-pool or thread per request to handle concurrent connections
- implement compression
- security (things like path traversal, CSRF, HTTPS implementation, etc etc)

## basic HTTP server

see `http-server-from-scratch/src/main.rs`.
