import javax.net.ssl.SSLSocketFactory;
import java.net.Socket;

public class SSLClient {
    public static void main(String[] args) throws Exception {
        SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
        Socket socket = factory.createSocket("google.com", 443);
        System.out.println("Connected to secure server!");
        socket.close();
    }
}
