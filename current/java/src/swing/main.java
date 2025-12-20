import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SwingExamples {

    public static void main(String[] args) {
        JFrame frame = new JFrame("Swing Examples Explorer");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(800, 600);

        JTabbedPane tabbedPane = new JTabbedPane();

        tabbedPane.addTab("basic controls", createBasicControlsPanel());
        tabbedPane.addTab("layouts", createLayoutsPanel());
        tabbedPane.addTab("event handling", createEventHandlingPanel());
        tabbedPane.addTab("custom painting", createCustomPaintingPanel());

        frame.add(tabbedPane);
        frame.setVisible(true);
    }

    private static JPanel createBasicControlsPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        JLabel label = new JLabel("Enter Your Name:");
        JTextField textField = new JTextField(15);
        JButton button = new JButton("Greet Me");
        JLabel greetingLabel = new JLabel();

        button.addActionListener(e -> greetingLabel.setText("Hello, " + textField.getText() + "!"));

        panel.add(label);
        panel.add(textField);
        panel.add(button);
        panel.add(greetingLabel);
        return panel;
    }

    private static JPanel createLayoutsPanel() {
        JPanel panel = new JPanel(new BorderLayout());

        JPanel topPanel = new JPanel();
        topPanel.setBackground(Color.LIGHT_GRAY);
        topPanel.add(new JLabel("Top Section"));

        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
        leftPanel.add(new JButton("Option 1"));
        leftPanel.add(new JButton("Option 2"));

        JPanel centerPanel = new JPanel(new GridLayout(2, 2, 5, 5));
        centerPanel.add(new JLabel("Grid Cell [0,0]"));
        centerPanel.add(new JLabel("Grid Cell [0,1]"));
        centerPanel.add(new JLabel("Grid Cell [1,0]"));
        centerPanel.add(new JLabel("Grid Cell [1,1]"));

        panel.add(topPanel, BorderLayout.NORTH);
        panel.add(leftPanel, BorderLayout.WEST);
        panel.add(centerPanel, BorderLayout.CENTER);

        return panel;
    }

    private static JPanel createEventHandlingPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout());

        JButton button = new JButton("Click Me");
        JLabel label = new JLabel("Button not clicked.");

        button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                label.setText("Button clicked!");
            }
        });

        panel.add(button);
        panel.add(label);
        return panel;
    }

    private static JPanel createCustomPaintingPanel() {
        JPanel panel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                g.setColor(Color.BLUE);
                g.fillRect(50, 50, 100, 100);
                g.setColor(Color.RED);
                g.fillOval(200, 50, 100, 100);
            }
        };

        panel.setPreferredSize(new Dimension(400, 400));
        return panel;
    }
}
