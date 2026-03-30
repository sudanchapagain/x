import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class lab4 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(400, 300);
        frame.setLayout(new FlowLayout());

        JTextField num1 = new JTextField(10);
        JTextField num2 = new JTextField(10);

        frame.add(new JLabel("Number 1:"));
        frame.add(num1);
        frame.add(new JLabel("Number 2:"));
        frame.add(num2);

        JLabel resultLabel = new JLabel("Result: ");
        frame.add(resultLabel);

        ImageIcon addIcon = new ImageIcon("add.png");
        ImageIcon subIcon = new ImageIcon("sub.png");
        ImageIcon mulIcon = new ImageIcon("mul.png");

        JButton addBtn = new JButton(addIcon);
        JButton subBtn = new JButton(subIcon);
        JButton mulBtn = new JButton(mulIcon);

        frame.add(addBtn);
        frame.add(subBtn);
        frame.add(mulBtn);

        ActionListener action = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    double a = Double.parseDouble(num1.getText());
                    double b = Double.parseDouble(num2.getText());
                    double result = 0;

                    if (e.getSource() == addBtn) {
                        result = a + b;
                    } else if (e.getSource() == subBtn) {
                        result = a - b;
                    } else if (e.getSource() == mulBtn) {
                        result = a * b;
                    }

                    resultLabel.setText("Result: " + result);
                } catch (Exception ex) {
                    resultLabel.setText("Invalid input");
                }
            }
        };

        addBtn.addActionListener(action);
        subBtn.addActionListener(action);
        mulBtn.addActionListener(action);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}

