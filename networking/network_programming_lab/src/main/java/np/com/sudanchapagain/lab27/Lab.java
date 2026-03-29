package np.com.sudanchapagain.lab27;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;

public class Lab {
    public static void run() {
        Thread srv = getThread();
        srv.start();

        try (InputStream in = new URL("http://localhost:8031/").openStream(); BufferedReader br = new BufferedReader(new InputStreamReader(in))) {
            System.out.println("File server first line: " + br.readLine());
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    private static Thread getThread() {
        Thread srv = new Thread(() -> {
            try (ServerSocket ss = new ServerSocket(8031)) {
                ss.setSoTimeout(2000);
                Socket s = ss.accept();
                try (BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream())); OutputStream out = s.getOutputStream()) {
                    String body = "File content";
                    String resp = "HTTP/1.0 200 OK\r\nContent-Length: " + body.length() + "\r\n\r\n" + body;
                    out.write(resp.getBytes(StandardCharsets.UTF_8));
                }
            } catch (Exception ignored) {
            }
        });
        srv.setDaemon(true);
        return srv;
    }
}
