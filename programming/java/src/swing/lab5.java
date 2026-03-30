import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class lab5 extends JFrame {

    JLabel status;
    JTextField textField;
    JCheckBox checkBox;

    public lab5() {
        setTitle("");
        setSize(500, 400);
        setLayout(new FlowLayout());

        status = new JLabel("Status: ");
        add(status);

        textField = new JTextField(20);
        add(textField);

        checkBox = new JCheckBox("Check me");
        add(checkBox);

        addWindowListener(new WindowAdapter() {
            public void windowOpened(WindowEvent e) {
                status.setText("Window Opened");
            }

            public void windowClosing(WindowEvent e) {
                status.setText("Window Closing");
            }
        });

        checkBox.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (checkBox.isSelected())
                    status.setText("Checkbox Selected");
                else
                    status.setText("Checkbox Deselected");
            }
        });

        textField.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) {
                status.setText("TextField Focus Gained");
            }

            public void focusLost(FocusEvent e) {
                status.setText("TextField Focus Lost");
            }
        });

        addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                status.setText("Mouse Clicked at (" + e.getX() + ", " + e.getY() + ")");
            }
        });

        textField.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                status.setText("Key Pressed: " + e.getKeyChar());
            }
        });

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }

    public static void main(String[] args) {
        new lab5();
    }
}
