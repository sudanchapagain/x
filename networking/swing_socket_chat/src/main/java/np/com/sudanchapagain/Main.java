package np.com.sudanchapagain;

import np.com.sudanchapagain.common.ChatServer;
import np.com.sudanchapagain.swing.SwingLauncher;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String host = "127.0.0.1";
        int port = 5000;
        int clients = 2;
        int maxClients = 5;

        if (args.length == 1) {
            try {
                clients = Integer.parseInt(args[0]);
            } catch (NumberFormatException e) {
                System.err.println("clients must be a number");
                System.exit(1);
            }
            if (clients < 1) clients = 1;
            if (clients > maxClients) clients = maxClients;
        }

        ChatServer server = new ChatServer(port);
        Thread serverThread = new Thread(() -> {
            try {
                server.start();
            } catch (IOException e) {
                System.err.println("Server error: " + e.getMessage());
            }
        }, "chat-server");
        serverThread.setDaemon(true);
        serverThread.start();

        Runtime.getRuntime().addShutdownHook(new Thread(server::stop));

        try {
            Thread.sleep(150);
        } catch (InterruptedException ignored) {
            Thread.currentThread().interrupt();
        }

        try {
            SwingLauncher.main(new String[] {
                host,
                String.valueOf(port),
                String.valueOf(clients)
            });
        } catch (Exception e) {
            System.err.println("Failed to start Swing client");
            System.exit(1);
        }
    }
}
