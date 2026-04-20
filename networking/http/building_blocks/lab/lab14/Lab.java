package np.com.sudanchapagain.lab14;

import java.net.*;
import java.util.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            URLConnection c = u.openConnection();
            Map<String, List<String>> h = c.getHeaderFields();

            for (Map.Entry<String, List<String>> e : h.entrySet()) {
                System.out.println((e.getKey() == null ? "Status" : e.getKey()) + ": " + String.join(";", e.getValue()));
            }
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
