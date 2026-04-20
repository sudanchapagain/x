package np.com.sudanchapagain.lab12;

import java.net.*;
import java.util.*;

public class Lab {
    public static void run() {
        CookieHandler ch = CookieHandler.getDefault();
        if (ch instanceof CookieManager) {
            CookieStore store = ((CookieManager) ch).getCookieStore();
            System.out.println("Cookies stored: " + store.getCookies());
        } else System.out.println("No CookieManager installed");
    }
}
