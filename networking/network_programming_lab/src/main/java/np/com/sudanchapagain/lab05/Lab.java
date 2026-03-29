package np.com.sudanchapagain.lab05;

import java.io.*;
import java.net.*;

public class Lab {
    public static void run() {
        String target = "http://example.com/";
        try {
            URL url = new URL(target);
            URLConnection conn = url.openConnection();
            conn.setRequestProperty("User-Agent", "Java");

            String contentType = conn.getContentType();
            String charset = null;

            if (contentType != null) {
                for (String part : contentType.split(";")) {
                    part = part.trim();
                    if (part.toLowerCase().startsWith("charset=")) charset = part.substring(8);
                }
            }

            InputStream in = conn.getInputStream();
            Reader r = (charset != null) ? new InputStreamReader(in, charset) : new InputStreamReader(in);
            BufferedReader br = new BufferedReader(r);
            String line = br.readLine();

            if (line != null) System.out.println("First line: " + line);
            br.close();
        } catch (Exception e) {
            System.out.println("Download error: " + e.getMessage());
        }
    }
}
