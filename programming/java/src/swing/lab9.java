import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;

public class lab9 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(400, 200);
        frame.setLayout(new FlowLayout());

        JButton fileBtn = new JButton("File Chooser");
        JButton colorBtn = new JButton("Color Chooser");

        JLabel label = new JLabel("Selected info will appear here");
        frame.add(fileBtn);
        frame.add(colorBtn);
        frame.add(label);

        fileBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileChooser = new JFileChooser();
                int result = fileChooser.showOpenDialog(frame);

                if (result == JFileChooser.APPROVE_OPTION) {
                    File file = fileChooser.getSelectedFile();
                    label.setText("File: " + file.getName());
                } else {
                    label.setText("No file selected");
                }
            }
        });

        colorBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Color color = JColorChooser.showDialog(frame, "Choose Color", Color.WHITE);

                if (color != null) {
                    label.setText("Color Selected");
                    frame.getContentPane().setBackground(color);
                }
            }
        });

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
