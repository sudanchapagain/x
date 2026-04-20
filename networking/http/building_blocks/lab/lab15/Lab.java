package np.com.sudanchapagain.lab15;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            URLConnection c = u.openConnection();

            String ct = c.getContentType();
            String cs = null;

            if (ct != null) {
                for (String p : ct.split(";")) {
                    if (p.trim().toLowerCase().startsWith("charset=")) {
                        cs = p.split("=")[1];
                    }
                }
            }

            try (InputStream in = c.getInputStream(); Reader r = cs == null ? new InputStreamReader(in) : new InputStreamReader(in, cs);

                 BufferedReader br = new BufferedReader(r)) {
                System.out.println("First line: " + br.readLine());
            }
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
