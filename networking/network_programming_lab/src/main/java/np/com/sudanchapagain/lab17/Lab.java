package np.com.sudanchapagain.lab17;

import java.net.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            URLConnection c = u.openConnection();
            c.setConnectTimeout(1000);
            c.setReadTimeout(2000);
            c.setRequestProperty("User-Agent", "Lab24/1.0");
            c.connect();
            System.out.println("Connected, content-length=" + c.getContentLength());
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
