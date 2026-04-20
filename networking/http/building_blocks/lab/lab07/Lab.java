package np.com.sudanchapagain.lab07;

import java.net.URL;

public class Lab {
    public static void run() {
        String[] probes = {"http://example.com/", "https://example.com/", "ftp://example.com/"};

        for (String p : probes) {
            try {
                new URL(p);
                System.out.println(p.split(":")[0] + " is supported");
            } catch (Exception e) {
                System.out.println(p.split(":")[0] + " is not supported");
            }
        }
    }
}
