package np.com.sudanchapagain.lab29;

import java.io.*;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;

public class Lab {
    public static void run() {
        Thread srv = new Thread(() -> {
            try (ServerSocketChannel ssc = ServerSocketChannel.open()) {
                ssc.bind(new InetSocketAddress(6200));
                ssc.configureBlocking(true);
                SocketChannel sc = ssc.accept();
                sc.write(ByteBuffer.wrap("NIO hello\n".getBytes()));
                sc.close();
            } catch (Exception ignored) {
            }
        });

        srv.setDaemon(true);
        srv.start();

        try (SocketChannel c = SocketChannel.open(new InetSocketAddress("localhost", 6200))) {
            ByteBuffer b = ByteBuffer.allocate(64);
            c.read(b);
            b.flip();
            System.out.println(new String(b.array(), 0, b.limit()).trim());
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
