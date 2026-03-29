package np.com.sudanchapagain.lab18;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        try {
            URL u = new URL("http://example.com/");
            try (InputStream in = u.openConnection().getInputStream(); BufferedReader br = new BufferedReader(new InputStreamReader(in))) {
                System.out.println("First: " + br.readLine());
            }
        } catch (Exception e) {
            System.out.println("err: " + e.getMessage());
        }
    }
}
