package np.com.sudanchapagain.lab08;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

public class Lab {
    public static void run() {
        try {
            String s = "a b=c&d";
            System.out.println(URLEncoder.encode(s, StandardCharsets.UTF_8));
        } catch (Exception e) {
            System.out.println("err");
        }
    }
}
