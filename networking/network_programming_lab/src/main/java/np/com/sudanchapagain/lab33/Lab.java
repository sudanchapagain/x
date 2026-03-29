package np.com.sudanchapagain.lab33;

import java.net.*;
import java.io.*;

public class Lab {
    private static boolean isArmstrong(int n) {
        int tmp = n, sum = 0;
        while (tmp > 0) {
            int d = tmp % 10;
            sum += d * d * d;
            tmp /= 10;
        }
        return sum == n;
    }

    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (DatagramSocket c = new DatagramSocket()) {
            String s = "153";
            DatagramPacket p = new DatagramPacket(s.getBytes(), s.length(), InetAddress.getByName("localhost"), 6280);

            c.send(p);
            byte[] buf = new byte[32];
            DatagramPacket r = new DatagramPacket(buf, buf.length);
            c.setSoTimeout(1000);
            c.receive(r);
            System.out.println("153 -> " + new String(r.getData(), 0, r.getLength()));
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (DatagramSocket ds = new DatagramSocket(6280)) {
                byte[] buf = new byte[32];
                DatagramPacket p = new DatagramPacket(buf, buf.length);
                ds.receive(p);
                int n = Integer.parseInt(new String(p.getData(), 0, p.getLength()).trim());
                String r = isArmstrong(n) ? "armstrong" : "not";

                ds.send(new DatagramPacket(r.getBytes(), r.length(), p.getAddress(), p.getPort()));
            } catch (Exception ignored) {
            }
        });

        srv.setDaemon(true);
        return srv;
    }
}
