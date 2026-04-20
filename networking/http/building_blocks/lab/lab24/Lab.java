package np.com.sudanchapagain.lab24;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6140)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();
                try (PrintWriter out = new PrintWriter(s.getOutputStream(), true)) {
                    out.println("This is file content. Roll No: 64");
                }
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        srv.start();

        try (Socket c = new Socket("localhost", 6140); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            System.out.println("File line: " + in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
