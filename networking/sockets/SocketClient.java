import java.net.Socket;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class SocketClient {
    public static void main(String[] args) throws Exception {
        Socket socket = new Socket("localhost", 8080);
        PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        out.println("GET / HTTP/1.1\r\nHost: localhost\r\n\r\n");
        String response = in.readLine();
        System.out.println("Response: " + response);
        socket.close();
    }
}
