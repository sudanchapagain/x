package np.com.sudanchapagain.common;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Set;

public class ChatServer {
    private static final String JOIN_MESSAGE = "__join__";

    private final int port;
    private volatile boolean running = false;
    private DatagramSocket serverSocket;
    private final Set<SocketAddress> clients = new HashSet<>();

    public ChatServer(int port) {
        this.port = port;
    }

    public void start() throws IOException {
        running = true;
        serverSocket = new DatagramSocket(port);
        byte[] buf = new byte[8192];
        DatagramPacket packet = new DatagramPacket(buf, buf.length);

        while (running) {
            serverSocket.receive(packet);
            String msg = new String(
                packet.getData(),
                packet.getOffset(),
                packet.getLength(),
                StandardCharsets.UTF_8
            );

            synchronized (clients) {
                clients.add(packet.getSocketAddress());
            }
            if (JOIN_MESSAGE.equals(msg)) {
                continue;
            }
            broadcast(msg);
        }
    }

    public void stop() {
        running = false;
        if (serverSocket != null && !serverSocket.isClosed()) {
            serverSocket.close();
        }
    }

    private void broadcast(String message) throws IOException {
        byte[] out = message.getBytes(StandardCharsets.UTF_8);
        synchronized (clients) {
            for (SocketAddress client : clients) {
                DatagramPacket outPacket = new DatagramPacket(out, out.length, client);
                serverSocket.send(outPacket);
            }
        }
    }
}
