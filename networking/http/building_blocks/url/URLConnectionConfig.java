import java.net.URL;
import java.net.URLConnection;

public class URLConnectionConfig {
    public static void main(String[] args) throws Exception {
        URL url = new URL("https://example.com");
        URLConnection conn = url.openConnection();
        conn.setDoOutput(true);
        conn.setConnectTimeout(5000);
        conn.setRequestProperty("User-Agent", "Mozilla/5.0");
    }
}
