package np.com.sudanchapagain.lab26;

import java.net.*;

public class Lab {
    public static void run() {
        try (ServerSocket ss = new ServerSocket()) {
            ss.setReuseAddress(true);
            ss.bind(new InetSocketAddress(6160));
            System.out.println("Bound on " + ss.getLocalPort() + ", reuseAddress=" + ss.getReuseAddress());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
