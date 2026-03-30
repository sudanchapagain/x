import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

class ExternalHandler implements ActionListener {
    public void actionPerformed(ActionEvent e) {
        JOptionPane.showMessageDialog(null, "Handled by External Class");
    }
}

public class lab3 extends JFrame implements ActionListener {

    JButton btnSameClass, btnInnerClass, btnAnonymous, btnExternal;

    public lab3() {
        setSize(500, 400);
        setLayout(new FlowLayout());

        btnSameClass = new JButton("same class");
        btnSameClass.addActionListener(this);
        add(btnSameClass);

        btnInnerClass = new JButton("inner class");
        btnInnerClass.addActionListener(new InnerHandler());
        add(btnInnerClass);

        btnAnonymous = new JButton("anonymous");
        btnAnonymous.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(null, "Handled by anon inner class");
            }
        });
        add(btnAnonymous);

        btnExternal = new JButton("external class");
        btnExternal.addActionListener(new ExternalHandler());
        add(btnExternal);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == btnSameClass) {
            JOptionPane.showMessageDialog(this, "Handled within same class");
        }
    }

    class InnerHandler implements ActionListener {
        public void actionPerformed(ActionEvent e) {
            JOptionPane.showMessageDialog(null, "Handled by inner class");
        }
    }

    public static void main(String[] args) {
        new lab3();
    }
}
