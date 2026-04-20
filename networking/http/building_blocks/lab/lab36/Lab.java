package np.com.sudanchapagain.lab36;

import java.net.*;

public class Lab {
    public static void run() {
        Thread t = new Thread(() -> {
            try (MulticastSocket ms = new MulticastSocket(6290)) {
                InetAddress g = InetAddress.getByName("230.0.0.1");
                ms.joinGroup(g);
                ms.setSoTimeout(1000);

                byte[] buf = new byte[256];
                DatagramPacket p = new DatagramPacket(buf, buf.length);
                try {
                    ms.receive(p);
                    System.out.println("Sniffer got: " + new String(p.getData(), 0, p.getLength()));
                } catch (Exception ex) {
                    System.out.println("No multicast heard");
                }
                ms.leaveGroup(g);
            } catch (Exception e) {
                System.out.println(e.getMessage());
            }
        });
        t.setDaemon(true);
        t.start();
        try {
            t.join(1200);
        } catch (InterruptedException ignored) {
        }
    }
}
