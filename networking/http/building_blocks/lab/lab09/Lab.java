package np.com.sudanchapagain.lab09;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

public class Lab {
    public static void run() {
        try {
            String enc = "a+b%3Dc%26d";
            System.out.println(URLDecoder.decode(enc, StandardCharsets.UTF_8));
        } catch (Exception e) {
            System.out.println("err");
        }
    }
}
