package np.com.sudanchapagain.lab13;

import java.net.*;
import java.util.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            HttpURLConnection c = (HttpURLConnection) u.openConnection();
            c.connect();
            System.out.println("Content-Type: " + c.getHeaderField("Content-Type"));
            System.out.println("Date: " + c.getHeaderFieldDate("Date", 0));
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
