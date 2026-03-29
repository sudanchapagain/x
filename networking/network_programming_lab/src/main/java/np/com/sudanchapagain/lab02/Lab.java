package np.com.sudanchapagain.lab02;

import java.net.InetAddress;

public class Lab {
    public static void run() {
        try {
            InetAddress local = InetAddress.getLocalHost();
            System.out.println("Address: " + local.getHostAddress());
            System.out.println("Is loopback: " + local.isLoopbackAddress());
            System.out.println("Is any local: " + local.isAnyLocalAddress());
            System.out.println("Is link local: " + local.isLinkLocalAddress());
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
