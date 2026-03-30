import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class lab8 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(400, 200);
        frame.setLayout(new FlowLayout());

        JButton modalBtn = new JButton("Modal Dialog");
        JButton modelessBtn = new JButton("Modeless Dialog");

        frame.add(modalBtn);
        frame.add(modelessBtn);

        modalBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JDialog modalDialog = new JDialog(frame, "Modal Dialog", true);
                modalDialog.setSize(250, 150);
                modalDialog.setLayout(new FlowLayout());
                modalDialog.add(new JLabel("This is a Modal Dialog"));
                modalDialog.add(new JButton("Close"));

                ((JButton) modalDialog.getContentPane().getComponent(1)).addActionListener(ev -> modalDialog.dispose());

                modalDialog.setLocationRelativeTo(frame);
                modalDialog.setVisible(true);
            }
        });

        modelessBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JDialog modelessDialog = new JDialog(frame, "Modeless Dialog", false);
                modelessDialog.setSize(250, 150);
                modelessDialog.setLayout(new FlowLayout());
                modelessDialog.add(new JLabel("This is a Modeless Dialog"));
                modelessDialog.add(new JButton("Close"));

                ((JButton) modelessDialog.getContentPane().getComponent(1)).addActionListener(ev -> modelessDialog.dispose());

                modelessDialog.setLocationRelativeTo(frame);
                modelessDialog.setVisible(true);
            }
        });

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
