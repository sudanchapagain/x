package np.com.sudanchapagain.lab21;

import java.io.*;
import java.net.*;
import java.util.Date;

public class Lab {
    public static void run() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6013)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();
                try (PrintWriter out = new PrintWriter(s.getOutputStream(), true)) {
                    out.println(new Date());
                }
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        srv.start();

        try (Socket c = new Socket("localhost", 6013); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            System.out.println("Daytime: " + in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
