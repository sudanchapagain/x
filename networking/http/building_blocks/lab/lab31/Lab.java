package np.com.sudanchapagain.lab31;

import java.net.*;
import java.io.*;
import java.util.Date;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (DatagramSocket c = new DatagramSocket()) {
            byte[] msg = "time?".getBytes();
            DatagramPacket p = new DatagramPacket(msg, msg.length, InetAddress.getByName("localhost"), 6260);
            c.send(p);
            byte[] buf = new byte[256];
            DatagramPacket r = new DatagramPacket(buf, buf.length);
            c.setSoTimeout(1000);
            c.receive(r);
            System.out.println("Daytime: " + new String(r.getData(), 0, r.getLength()));
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (DatagramSocket ds = new DatagramSocket(6260)) {
                byte[] buf = new byte[256];
                DatagramPacket p = new DatagramPacket(buf, buf.length);
                ds.receive(p);
                String resp = new Date().toString();
                ds.send(new DatagramPacket(resp.getBytes(), resp.length(), p.getAddress(), p.getPort()));
            } catch (Exception ignored) {
            }
        });

        srv.setDaemon(true);
        return srv;
    }
}
