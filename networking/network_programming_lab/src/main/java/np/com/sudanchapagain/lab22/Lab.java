package np.com.sudanchapagain.lab22;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (Socket c = new Socket("localhost", 6120); PrintWriter out = new PrintWriter(c.getOutputStream(), true); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            out.println("Hello from client");
            System.out.println(in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6120)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();
                try (BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream())); PrintWriter out = new PrintWriter(s.getOutputStream(), true)) {
                    String msg = in.readLine();
                    out.println("Received: " + msg);
                }
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        return srv;
    }
}
