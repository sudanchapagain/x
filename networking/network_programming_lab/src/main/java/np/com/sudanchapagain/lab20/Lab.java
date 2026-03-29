package np.com.sudanchapagain.lab20;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6100)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();

                try (BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream()));

                     PrintWriter out = new PrintWriter(s.getOutputStream(), true);) {
                    out.println("ACK from server");
                }
            } catch (Exception ignored) {
            }
        });

        srv.setDaemon(true);
        srv.start();

        try (Socket c = new Socket("localhost", 6100); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            System.out.println("Server replied: " + in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
