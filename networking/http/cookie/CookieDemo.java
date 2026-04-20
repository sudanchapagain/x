import java.net.CookieManager;
import java.net.HttpCookie;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;

public class CookieDemo {
    public static void main(String[] args) throws Exception {
        CookieManager manager = new CookieManager();
        HttpCookie cookie = new HttpCookie("sessionID", "12345");
        manager.getCookieStore().add(new URI("https://example.com"), cookie);

        URL url = new URI("https://example.com").toURL();
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        System.out.println("Response Code: " + conn.getResponseCode());
    }
}
