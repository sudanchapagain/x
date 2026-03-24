import java.nio.channels.SocketChannel;
import java.net.InetSocketAddress;

public class NonBlockingClient {
    public static void main(String[] args) throws Exception {
        SocketChannel channel = SocketChannel.open();
        channel.configureBlocking(false);
        channel.connect(new InetSocketAddress("google.com", 80));
        while (!channel.finishConnect()) {
            // Wait for connection
        }
        System.out.println("Connected!");
        channel.close();
    }
}
