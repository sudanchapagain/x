package np.com.sudanchapagain.lab16;

import java.net.*;
import java.util.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            HttpURLConnection c = (HttpURLConnection) u.openConnection();
            c.connect();

            int i = 0;
            String k;

            while ((k = c.getHeaderFieldKey(i)) != null || c.getHeaderField(i) != null) {
                System.out.println((k == null ? "Status" : k) + ": " + c.getHeaderField(i));
                i++;
            }
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
