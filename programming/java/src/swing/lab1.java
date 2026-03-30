import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;

public class lab1 {
    public static void main(String[] args) {
        JFrame f = new JFrame("");

        f.setSize(500, 600);
        f.setLayout(new FlowLayout());

        ImageIcon i = new ImageIcon("icon.png");
        f.setIconImage(i.getImage());

        JLabel nl = new JLabel("Sudan Chapagain");
        nl.setFont(new Font("Serif", Font.BOLD, 20));
        nl.setForeground(Color.BLUE);
        f.add(nl);

        JLabel il = new JLabel("Icon Label", new ImageIcon("icon.png"), JLabel.LEFT);
        f.add(il);

        JTextField tf = new JTextField(20);
        tf.setBorder(new LineBorder(Color.RED, 2));
        tf.setToolTipText("Enter text here");
        f.add(tf);

        JTextArea ta = new JTextArea(5, 20);
        JScrollPane scrollPane = new JScrollPane(ta, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        f.add(scrollPane);

        JCheckBox cb = new JCheckBox("Accept Terms");
        f.add(cb);

        JPasswordField pf = new JPasswordField(20);
        f.add(pf);

        JRadioButton r1 = new JRadioButton("Male");
        JRadioButton r2 = new JRadioButton("Female");
        ButtonGroup bg = new ButtonGroup();
        bg.add(r1);
        bg.add(r2);
        f.add(r1);
        f.add(r2);

        String[] items = {"Select", "Option 1", "Option 2", "Option 3"};
        JComboBox<String> comboBox = new JComboBox<>(items);
        f.add(comboBox);

        JButton button = new JButton("Submit");
        f.add(button);

        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(f, "Form Submitted");
            }
        });

        f.setDefaultCloseOperation(f.EXIT_ON_CLOSE);
        f.setVisible(true);
    }
}
