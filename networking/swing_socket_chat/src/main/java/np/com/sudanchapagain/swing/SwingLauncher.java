package np.com.sudanchapagain.swing;

import np.com.sudanchapagain.common.MulticastChatClient;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;

import javax.swing.*;

public class SwingLauncher {
    public static void main(String[] args) throws Exception {
        String host = "127.0.0.1";
        int port = 5000;
        int instances = 1;

        if (args.length >= 1) host = args[0];
        if (args.length >= 2) port = Integer.parseInt(args[1]);
        if (args.length >= 3) {
            try {
                instances = Integer.parseInt(args[2]);
            } catch (NumberFormatException ignored) {
            }
        }

        final String finalHost = host;
        final int finalPort = port;
        final int finalInstance = instances;

        SwingUtilities.invokeLater(() -> {
            try {
                flatleaf_look();
                for (int i = 0; i < finalInstance; i++) {
                    new SwingChat(finalHost, finalPort, i);
                }
            } catch (Throwable t) {
                System.err.println("Failed to start Swing UI");
            }
        });
    }

    private static void flatleaf_look() {
        try {
            Class<?> lafClass = Class
                .forName("com.formdev.flatlaf.FlatLightLaf");

            LookAndFeel laf = (LookAndFeel) lafClass
                .getDeclaredConstructor()
                .newInstance();

            UIManager.setLookAndFeel(laf);
        } catch (Exception ignored) {}
    }

    public static class SwingChat {
        private final MulticastChatClient mclient;
        private final JFrame frame = new JFrame("Socket chat");
        private final JTextArea area = new JTextArea();
        private final JTextField input = new JTextField();

        public SwingChat(String host, int port, int index) {
            mclient = new MulticastChatClient(host, port);
            area.setEditable(false);
            area.setLineWrap(true);
            area.setWrapStyleWord(true);

            frame.setLayout(new BorderLayout());
            frame.add(new JScrollPane(area), BorderLayout.CENTER);

            JPanel composer = new JPanel(new BorderLayout(8, 0));
            composer.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
            composer.add(input, BorderLayout.CENTER);

            JButton sendButton = new JButton("Send");
            composer.add(sendButton, BorderLayout.EAST);

            frame.add(composer, BorderLayout.SOUTH);
            frame.setMinimumSize(new Dimension(420, 280));
            frame.setSize(600, 400);
            frame.setLocationByPlatform(true);
            frame.setLocation(80 + (index * 40), 80 + (index * 40));
            frame.setVisible(true);
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

            frame.addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent e) {
                    mclient.close();
                }
            });

            try {
                mclient.connect(msg ->
                    SwingUtilities.invokeLater(() ->
                        area.append(msg + "\n")
                    )
                );
            } catch (IOException ex) {
                area.append(
                    "could not connect to server: "
                    + ex.getMessage()
                    + "\n"
                );
            }

            Action sendAction = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    String text = input.getText().trim();
                    if (text.isEmpty()) return;

                    if ("exit".equalsIgnoreCase(text)) {
                        mclient.close();
                        frame.dispose();
                        return;
                    }

                    try {
                        mclient.send(text);
                    } catch (IOException io) {
                        area.append("Send failed: " + io.getMessage() + "\n");
                    }
                    input.setText("");
                }
            };
            input.addActionListener(sendAction);
            sendButton.addActionListener(sendAction);
        }
    }
}
