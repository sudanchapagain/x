import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.InetAddress;

public class UDPClient {
    public static void main(String[] args) throws Exception {
        DatagramSocket socket = new DatagramSocket();
        byte[] data = "Hello UDP!".getBytes();
        DatagramPacket packet = new DatagramPacket(data, data.length, InetAddress.getByName("localhost"), 9876);
        socket.send(packet);
        socket.close();
    }
}
