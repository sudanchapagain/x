package np.com.sudanchapagain.lab25;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(6150)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();

                new Thread(() -> {
                    try (BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream()));

                         PrintWriter out = new PrintWriter(s.getOutputStream(), true);) {
                        long n = Long.parseLong(in.readLine());
                        boolean prime = n > 1;

                        for (long i = 2; i * i <= n; i++) {
                            if (n % i == 0) {
                                prime = false;
                                break;
                            }
                        }
                        out.println(prime ? "prime" : "not prime");
                    } catch (Exception ignored) {
                    }
                }).start();
            } catch (Exception ignored) {
            }
        });

        srv.setDaemon(true);
        srv.start();

        try (Socket c = new Socket("localhost", 6150); PrintWriter out = new PrintWriter(c.getOutputStream(), true); BufferedReader in = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
            out.println("17");
            System.out.println("17 is " + in.readLine());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
