package np.com.sudanchapagain.lab28;

import java.net.*;

public class Lab {
    public static void run() {
        try (DatagramSocket ds = new DatagramSocket()) {
            System.out.println("Local port: " + ds.getLocalPort());
            System.out.println("Broadcast: " + ds.getBroadcast());
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
