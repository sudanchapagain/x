import java.net.URLEncoder;
import java.net.URLDecoder;

public class URLEncodeDecode {
    public static void main(String[] args) throws Exception {
        String encoded = URLEncoder.encode("Hello World!", "UTF-8");
        System.out.println("Encoded: " + encoded);
        System.out.println("Decoded: " + URLDecoder.decode(encoded, "UTF-8"));
    }
}
