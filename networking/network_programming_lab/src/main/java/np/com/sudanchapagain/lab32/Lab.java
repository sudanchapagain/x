package np.com.sudanchapagain.lab32;

import java.net.*;
import java.io.*;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (DatagramSocket c = new DatagramSocket()) {
            byte[] msg = "hello udp".getBytes();
            DatagramPacket p = new DatagramPacket(msg, msg.length, InetAddress.getByName("localhost"), 6270);

            c.send(p);
            byte[] buf = new byte[512];

            DatagramPacket r = new DatagramPacket(buf, buf.length);
            c.setSoTimeout(1000);
            c.receive(r);

            System.out.println("Reply: " + new String(r.getData(), 0, r.getLength()));
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (DatagramSocket ds = new DatagramSocket(6270)) {
                byte[] buf = new byte[512];
                DatagramPacket p = new DatagramPacket(buf, buf.length);
                ds.receive(p);
                String msg = new String(p.getData(), 0, p.getLength());

                ds.send(new DatagramPacket(("OK:" + msg).getBytes(), ("OK:" + msg).length(), p.getAddress(), p.getPort()));
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        return srv;
    }
}
