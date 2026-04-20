package np.com.sudanchapagain.lab23;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (Socket c = new Socket("localhost", 6130); PrintWriter out = new PrintWriter(c.getOutputStream(), true); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            out.println("6");
            System.out.println("Factorial(6)=" + in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6130)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();
                try (BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream())); PrintWriter out = new PrintWriter(s.getOutputStream(), true)) {
                    long n = Long.parseLong(in.readLine());
                    long f = 1;
                    for (long i = 2; i <= n; i++) f *= i;
                    out.println(f);
                }
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        return srv;
    }
}
