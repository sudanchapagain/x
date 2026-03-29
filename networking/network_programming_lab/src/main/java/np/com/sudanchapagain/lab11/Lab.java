package np.com.sudanchapagain.lab11;

import java.net.*;
import java.util.*;

public class Lab {
    public static void run() {
        CookieManager cm = new CookieManager(null, (uri, cookie) -> {
            String h = uri.getHost();
            return h == null || !h.endsWith(".gov");
        });
        CookieHandler.setDefault(cm);
    }
}
