package np.com.sudanchapagain.lab06;

import java.net.URL;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("https://example.com:1234/path/page.html?q=1#frag");
            System.out.println("Protocol: " + u.getProtocol());
            System.out.println("Host: " + u.getHost());
            System.out.println("Port: " + u.getPort());
            System.out.println("Path: " + u.getPath());
            System.out.println("Query: " + u.getQuery());
            System.out.println("Ref: " + u.getRef());
        } catch (Exception e) {
            System.out.println("URL error: " + e.getMessage());
        }
    }
}
