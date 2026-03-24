## Introduction

Network is a group of two or more computers or other electronic devices that
are interconnected for the purpose of exchanging data and sharing resources.

Process of designing and implementing programs that enable communication
between different devices over a network is network programming.

**Features:**

- Socket Programming
- Client Server Architecture
- Protocols
- Multithreading
- Security
- Asynchronous Input/Output
- Remote Procedure Cull

**Scope:**

- Web & Internet
- Communication and Message Platforms
- Cloud Computing and Distributed Systems
- Internet of things (IoT)
- Cybersecurity and Network Monitoring
- File transfer and data sharing
- Multimedia streaming
- Gaming
- E-commerce

## Client Server Architecture

> aka 2 Tier Architecture

Distributed computing with server and client. It is

- Scalable,
- Performant,
- Allows communication,
- Secure.

but, it also has

- Single point of failure,
- Network congestion issues,
- Dependency on network,

### 3-Tier Architecture

```
[client] <----> [server] <----> [database]
```

### Connection Types

**Connection Oriented**:

> example: TCP

- A path is constructed and subsequently used.
- High overhead.
- Required path to be constructed first.
- Packets delivered in correct order.
- Reliable.

**Connection Less**:

> example: UDP

- Packet does not depend on predecessor's path. Potentially a new path for each packet.
- Low overhead.
- Does not require path to be constructed first.
- Packets could be in out of order.
- Unreliable.

## Internet Protocol

Unique identifying number associated to every device connected to the network / internet.

**IPv4**:

- 32-bits
- Numeric Address
- Separated by dot.

`[8 bits | 8 bits | 8 bits | 8 bits]`

**IPv6**:

- 128 bits.
- Separated by colon (`:`).
- 16 groups.

in Java, the `InetAddress` (`java.net.InetAddress`) class is used to represent IP addresses.
It has no public constructors. It does have three static methods that returns suitably
initialized `InetAddress` object.

1. `InetAddress.getByName(String ip/hostname)`

    Returns IP address of host

    ```java
    public static InetAddress InetAddress.getByName(String hostName) throws UnknownHostException;
    ```

    ```java
    try {
        InetAddress adr = InetAddress.getByName("www.google.com");
        System.out.println(adr);
    } catch (Exception e) {
        System.out.println(e);
    }
    ```

2. `InetAddress.getAllByName(String Ip/hostname)`

    Returns array of IP address of Host

    ```java
    try {
        InetAddress[] adrs = InetAddress.getAllByName("www.google.com");
        for (InetAddress inet : adrs) {
            System.out.println(inet);
        }
    } catch (Exception e) {
        System.out.println(e);
    }
    ```

3. `InetAddress.getLocalHost()`

    Prints address of machine it's running on.

    ```java
    try {
        InetAddress mAdr = InetAddress.getLocalHost();
        System.out.println(mAdr);
    } catch (Exception e) { /**/ }
    ```

The `InetAddress` class also has three getter methods.

```java
public String getHostName() {
} // returns string containing name of host

public byte[] getAddress() {
} // returns a string containing the dotted quad format of the IP address

public String getHostAddress() {
} // returns array of byte in network byte order. length depends upon IP version

public String getCanonicalHostName() {
} // returns fully qualified domain name
```

**Address Types**:

- Wildcard: `0.0.0.0` or `::`, `isAnyLocalAddress()`
- Loopback: same computer connecting IP. `isLoopbackAddress()`
- Link local: only valid for local network segment. `isLinkLocalAddress()`
- Site local: `isSiteLocalAddress()`
- Multicast: `isMulticastAddress()`
- Global multicast: `isMCGlobal()`

To test for reach-ability:

```java
try {
    InetAddress ad = InetAddress.getByName("www.google.com");
    if (ad.isReachable(5000)) System.out.println("yes");
    else System.out.println("no");
} catch (IOException e) {
    System.out.println( e.getMessage() );
}
```

Also, since it inherits from `java.lang.Object`, the following methods
`equals(s, d)`, `hashCode()`, `toString()` can be used on `InetAddress`.

To specifically represent `v4` or `v6`:

```java
Inet4Address ip1 = Inet4address.getByName(url);

Inet6Address ip2 = Inet6Address.getByName(url);
```

To find out which version, you can use length:

```java
try {
    InetAddress address = InetAddress.getByName("www.google.com");
    byte[] add = address.getAddress();

    if (add.length == 4) {
        System.out.println("IPv4");
    } else {
        System.out.println("IPv6");
    }
} catch (UnknownHostException e) {
    System.out.println("Error Caught: " + e.getMessage());
}
```

## Network Interface

`NetworkInterface` class represents network interface on the device that is
made up of a name, and a list of IP addresses assigned to it.

```java
// getByName(String name) retrieves network interface by its name
NetworkInterface ni = NetworkInterface.getByName("eth0");

NetworkInterface ni2 = NetworkInterface.getByInetAddress(
    new InetAddress.getByName("www.google.com")
);

// getByIndex(int)
// getNetworkInterface()
Enumeration<NetworkInterface> nis = NetworkInterface.getNetworkInterfaces(); // gets all NI from device
```

Getter methods for `NetworkInterface`:

```java
NetworkInterface ni = NetworkInterface.getByName("eth0");
ni.getName();
ni.getDisplayName();
ni.getHardwareAddress(); // MAC address
ni.getInetAddresses();
ni.getMTU() // max transmission unit
ni.isUP() // up and running?
ni.isLoopback() // loopback?
```

## URI

> URI: Uniform Resource Identifier.
> URL: Uniform Resource Locator.
> URN: Uniform Resource Names.

- `urn:isbn:28383883833`
- `http://example.com/abc.html`

```
uri [parent]
 |
 +---+
     |
     v
(URL, URN) [children]
```

In Java, we can use `java.net.URL`

### URL Connections

```java
URL u = new URL("http://www.google.com");
System.out.println(u.toString());

URLConnection con = u.openConnection();
con.connect();

int i = 0;
String headerKey;
while((headerKey = con.getHeaderFieldKey(i)) != null ) {
    String val = con.getHeaderField(i);
    System.out.println("headerKey:" + val);
    i++;
}
```

using `openStream()` (returns byte array):

> `new InputStreamReader(...)` wraps the `InputStream` and converts bytes into
> characters. `BufferedReader(...)` wraps `InputStreamReader` and provides a
> convenient method `readLine()`.

```java
import java.net.*;
import java.io.*;

public class RetrieveDataUsingOpenStream {
    public static void main(String[] args) {
        try {
            URL url = new URL("https://www.example.com");
            BufferedReader reader = new BufferedReader(
                new InputStreamReader(
                    url.openStream()
                )
            );

            String line;
            while((line = reader.readline()) != null) {
                System.out.println(line);
            }
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

using `openConnection()`:

```java
try {
    URL url = new URL("https://www.example.com");
    HTTPURLConnection con = (HTTPURLConnection) url.openConnection();
    con.setRequestMethod("GET");

    String line;
    BufferedReader r = new BufferedReader(
        new InputStreamReader(c.getInputStream())
    );

    while ((line = r.readLine()) != null) {
        System.out.println(line);
    }
    con.disconnect();
} catch (MalformedURLException e) {
    e.printStackTrace();
} catch (IOException e) {
    e.printStackTrace();
}
```

### To Split URLs

Structure of URLs:

```
https://www.example.com:8080/students/profile.php?id=3#top
----------------------------------------------------------

https -> protocol
www.example.com -> host
8080 -> port
/students/profile.php -> path
/students/profile.php?id=4 -> file
#top -> fragment ref
```

Methods available:

```java
url.getProtocol();
url.getHost();
url.getPort();
url.getFile();
url.getPath();
url.getQuery();
url.getRef();
```

Example program:

```java
public class URLSplitter {
    public static void main(String[] args) {
        try {
            URL url = new URL("https://www.example.com:8080/students/profile.php?id=4#top");
            System.out.println("protocol: " + url.getProtocol()
                            + "\nHost: " + url.getHost()
                            + "\nPort: " + url.getPort()
                            + "\nFile: " + url.getFile()
                            + "\nPath: " + url.getPath()
                            + "\nQuery: " + url.getQuery()
                            + "\nFragment Ref: " + url.getRef()
            );
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
    }
}
```

MISC:

`equals()` returns true if they have same protocol, host, port, file/anchor/ref for two URL objects

`sameFile()` returns true if both URLs have the same protocol, host, port, and file. (anchor/fragment is not checked)

---

`toString()`

`toURI()` to URI is good for handling special characters safely. It throws `URISyntaxException` if the URL is malformed.

```java
try {
    URL url = new URL("https://www.example.com/with spaces");
    URI  uri = url.toURI(); // to URI (encoding)
    System.out.println(uri); // should print `https://www.example.com/with%20spaces`
} catch (URLSyntaxException r) {
    e.printStackTrace();
}
```

### URI Class

More flexible and compliant than URL.

```
abc://username:password@example.com:1234/path/data?key=value#fragid1
--------------------------------------------------------------------

abc -> schema
username:password@example.com:1234 -> authority
username:password:` us -> information
example.com -> host
1234 -> port
/path/data -> path
key=value -> query
fragid1 -> fragment
```

We can use methods such as:

```java
uri.getScheme();
uri.getUserInfo();
uri.getAuthority();
uri.getHost();
uri.getPort();
uri.getPath();
uri.getQuery();
uri.getRef();
```

To resolving relative URLs:

```java
URI base = new URI("http://example.com/folder/");
URI relative = new URI("page.html");
URI resolved = base.resolve(relative); // http://example.com/folder/page.html
```

The resolve method mimics how browsers resolve.

MISC:

```java
uri1.equals(uri2);

URI uri1 = new URI("http://a.com");
URI uri2 = new URI("http://b.com");
System.out.println(uri1.compareTo(uri2)); // negative (a < b) lexicographically

uri1.toString();

uri1.toASCIIString // spaces as `%20`
```

`x-www-form-urlencoded`

It is a format to encode key-value pairs in URLs. It is used in HTML forms and
HTTP requests. It works by replacing things like spaces with `%20`, special
chars like `&`, `=`, `#` encoded as `%xx`.

`java.net.URLEncoder` converts strings into a URL safe format.

```java
import java.net.*;

public class URLEncoderEx {
    public static void main(String[] args) {
        try {
            String query = "name=MeFirst MeLast&age=2";
            String encoded = URLEncoder.encode(query, "UTF-8"); // name%3DMeFirst+MeLast%26age%3D2
            System.out.println(encoded);
        } catch {UnSupportedEncodingException e} catch {Exception e}
    }
}
```

`URLDecoder` reverses it:

```java
String query = "name=MeFirst MeLast&age=2";
String encoded = URLEncoder.encode(query, "UTF-8"); // name%3DMeFirst+MeLast%26age%3D2

String original = URLDecoder.decode(encoded, "UTF-8");
```

Proxies are intermediate servers. i.e. `[client] <---> [proxy] <---> [server]`

to set system properties:

```
http.proxyHost;
http.proxyPort;
http.proxyHosts;

https.proxyHost;
https.proxyPort;

ftp.proxyHost;
ftp.proxyPort;

socketsProxyHost;
socketsProxyPort;
```

```java
// System.setProperty(key, value)
System.setProperty("http.proxyHost", "127.0.01");
System.setProperty("http.proxyPort", "1822");
```

or

```console
java -Dhttp.proxyHost=proxy.example.com -Dhttp.proxyPort=8080 MyProgram
```

In Java, the `Proxy` class represents proxy setting for specific connection.

Types of Proxy:

```java
Proxy.Type.DIRECT; // no proxy
Proxy.Type.HTTP; // http proxy
Proxy.Type.SOCKS; // socket proxy
```

Constructor:

```java
new Proxy(
    Proxy.Type.HTTP,
    new InetSocketAddress("proxy.example.com", 8080)
)
```

Example:

```java
import java.io.*;
import java.net.*;

public class ProxyEx {
    public static void main(String[] args) {
        try {
            Proxy proxy = new Proxy(Proty.Type.HTTP, new InetSocketAddress("example.proxy.com", 1234));

            URL url = new URL("http://www.example.com");
            HTTPURLConnection con = url.openConnection(proxy);

            BufferedReader bf = new BufferedReader(
                new InputReaderStream(con.getInputStream())
            );

            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }

            con.disconnect();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

`ProxySelector` class provides a way to dynamically determine which network
connection to connect to a given URL or URI

```java
public class ProxySelectorExample {
    static class MyProxySelector extends ProxySelector {
        private final Proxy proxy = new Proxy(
            Proxy.Type.HTTP,
            new InetSocketAddress("example.socket.com", 1234)
        );

        @Override
        public List<Proxy> select(URI uri) {
            String host = uri.getHost();

            if (host.equals("localhost")) {
                System.out.println("By passing " + host);
                return List.of(Proxy.NO_PROXY);
            }

            System.out.println("using proxy for " + host);
            return List.of(proxy);
        }

        @Override
        public void connectionFailed(
            URI uri,
            SocketAddress sa,
            IOException ioe
        ) {
            System.out.println("Connection to proxy failed" + uri);
            ioe.printStackTrace();
        }
    }

    public static void main(String[] args) {
        ProxySelector.setDefault(new MyProxySelector());

        try {
            URL url = new URL("http://www.example.com");
            HTTPURLConnection con = (HTTPURLConnection) url.openConnection();
            BufferedReader r = new BufferedReader(
                new InputStreamReader(con.getInputStream())
            );

            String line;
            while((line = r.readLine()) != null) {
                System.out.println(line);
            }
            con.disconnect();
        } catch (Exception e) { e.printStackTrace(); }
    }
}
```

An example of communicating with server:

```java
import java.io.*;
import java.net.*;

public class Example {
    public static void main(String[] args) {
        // add try catch too
        String url = "http://www.example.com";
        String param = "pid=5&category=nepal";
        String encodedParam = URLEncoder.encode(param, "UTF-8");
        URL url = new URL(url + "?" + encodedParam);

        HTTPURLConnection con = (HTTPURLConnection) url.openConnection(1234);
        BufferedReader r = new BufferedReader(
            new InputStreamReader(con.getInputStream());
        );

        String line;
        while ((line = r.readLine()) != null) {
            System.out.println(line);
        }
        con.disconnect();
    }
}
```

In case of accessing password-protected sites:

```java
import java.io.*;
import java.net.*;

public class AuthenticatoEx {
    public static void main(String[] args) {
        try {
            Authenticator.setDefault(new Authenticator() {
                private PasswordAuthenticator getPasswordAuthenticator() {
                    return new PasswordAuthenticator("myuser", "mypassword".toCharArray());
                }
            })

            URL url = new URL("https://httpbin.org/basic-auth/myuser/mypassword");
            HTTPURLConnection con = (HTTPURLConnection) url.openConnection();

            BufferedReader bf = new BufferedReader(
                new InputStreamReader(con.getInputStream())
            );

            String line;
            while ((line = bf.readLine()) != null) {
                System.out.println(line);
            }

            con.disconnect();
        } catch () {}
    }
}
```
