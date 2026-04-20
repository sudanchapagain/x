package np.com.sudanchapagain.lab34;

import java.net.*;

public class Lab {
    public static void run() {
        try (DatagramSocket ds = new DatagramSocket()) {
            ds.setSoTimeout(1500);
            ds.setBroadcast(true);

            System.out.println("SoTimeout=" + ds.getSoTimeout() + ", broadcast=" + ds.getBroadcast());
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
