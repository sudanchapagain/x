import java.net.DatagramSocket;
import java.net.DatagramPacket;

public class UDPServer {
    public static void main(String[] args) throws Exception {
        DatagramSocket socket = new DatagramSocket(9876);
        byte[] buffer = new byte[1024];
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
        socket.receive(packet);
        System.out.println("Received: " + new String(packet.getData()));
        socket.close();
    }
}
