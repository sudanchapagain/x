package np.com.sudanchapagain.lab35;

import java.net.*;

public class Lab {
    public static void run() {
        try (MulticastSocket ms = new MulticastSocket()) {
            InetAddress g = InetAddress.getByName("230.0.0.1");
            byte[] m = "hi multicast Roll 64".getBytes();
            ms.send(new DatagramPacket(m, m.length, g, 6290));
            System.out.println("Sent multicast packet");
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
