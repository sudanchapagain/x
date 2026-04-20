package np.com.sudanchapagain.lab03;

import java.net.InetAddress;
import java.net.Inet4Address;
import java.net.Inet6Address;

public class Lab {
    public static void run() {
        try {
            InetAddress a = InetAddress.getLocalHost();
            if (a instanceof Inet4Address) System.out.println("Type: IPv4");
            else if (a instanceof Inet6Address) System.out.println("Type: IPv6");
            else System.out.println("Type: Unknown");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
