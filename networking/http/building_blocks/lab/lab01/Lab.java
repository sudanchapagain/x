package np.com.sudanchapagain.lab01;

import java.net.InetAddress;

public class Lab {
    public static void run() {
        try {
            InetAddress ip = InetAddress.getLocalHost();
            System.out.println("IP Address: " + ip.getHostAddress());
        } catch (Exception e) {
            System.out.println("Error retrieving IP: " + e.getMessage());
        }
    }
}
