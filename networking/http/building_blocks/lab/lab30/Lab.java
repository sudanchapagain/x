package np.com.sudanchapagain.lab30;

import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (DatagramSocket c = new DatagramSocket()) {
            byte[] msg = "ping".getBytes();
            DatagramPacket p = new DatagramPacket(msg, msg.length, InetAddress.getByName("localhost"), 6250);
            c.send(p);
            byte[] buf = new byte[256];
            DatagramPacket r = new DatagramPacket(buf, buf.length);
            c.setSoTimeout(1000);
            c.receive(r);
            System.out.println("UDP reply: " + new String(r.getData(), 0, r.getLength()));
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (DatagramSocket ds = new DatagramSocket(6250)) {
                byte[] buf = new byte[256];
                DatagramPacket p = new DatagramPacket(buf, buf.length);
                ds.receive(p);
                ds.send(new DatagramPacket(p.getData(), p.getLength(), p.getAddress(), p.getPort()));
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        return srv;
    }
}
