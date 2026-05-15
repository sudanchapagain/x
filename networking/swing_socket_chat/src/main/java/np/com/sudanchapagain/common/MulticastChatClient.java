package np.com.sudanchapagain.common;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

public class MulticastChatClient {
    private static final String JOIN_MESSAGE = "__join__";

    private final String serverHost;
    private final int serverPort;
    private DatagramSocket socket;
    private InetAddress serverAddress;
    private final AtomicBoolean running = new AtomicBoolean(false);

    public MulticastChatClient(String serverHost, int serverPort) {
        this.serverHost = serverHost;
        this.serverPort = serverPort;
    }

    public void connect(Consumer<String> onMessage) throws IOException {
        serverAddress = InetAddress.getByName(serverHost);
        socket = new DatagramSocket();
        running.set(true);
        send(JOIN_MESSAGE);

        Thread listener = new Thread(() -> {
            byte[] buf = new byte[8192];
            DatagramPacket packet = new DatagramPacket(buf, buf.length);

            while (running.get()) {
                try {
                    socket.receive(packet);
                    String msg = new String(
                        packet.getData(),
                        packet.getOffset(),
                        packet.getLength(),
                        StandardCharsets.UTF_8
                    );

                    if (JOIN_MESSAGE.equals(msg)) {
                        continue;
                    }
                    onMessage.accept(msg);
                } catch (IOException e) {
                    if (running.get()) {
                        onMessage.accept(
                            "[multicast receive error: "
                            + e.getMessage()
                            + "]"
                        );
                    }
                    break;
                }
            }
        }, "multicast-listener");
        listener.setDaemon(true);
        listener.start();
    }

    public void send(String msg) throws IOException {
        if (serverAddress == null || socket == null)
            throw new IllegalStateException("Not connected");

        byte[] data = msg.getBytes(StandardCharsets.UTF_8);
        DatagramPacket packet = new DatagramPacket(
            data,
            data.length,
            serverAddress,
            serverPort
        );

        socket.send(packet);
    }

    public void close() {
        running.set(false);
        if (socket != null) {
            socket.close();
        }
    }
}
