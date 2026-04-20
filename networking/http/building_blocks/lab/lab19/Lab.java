package np.com.sudanchapagain.lab19;

import java.net.*;

public class Lab {
    public static void run() {
        try (Socket s = new Socket("example.com", 80)) {
            System.out.println("Local: " + s.getLocalAddress() + ":" + s.getLocalPort());
            System.out.println("Remote: " + s.getInetAddress() + ":" + s.getPort());
            System.out.println("TCPNoDelay=" + s.getTcpNoDelay());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
