import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class lab10 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(500, 400);
        frame.setLayout(new FlowLayout());

        JButton infoBtn = new JButton("Information");
        JButton questionBtn = new JButton("Question");
        JButton errorBtn = new JButton("Error");
        JButton warningBtn = new JButton("Warning");
        JButton confirmBtn = new JButton("Confirm");
        JButton inputBtn = new JButton("Input");
        JButton optionBtn = new JButton("Option");

        frame.add(infoBtn);
        frame.add(questionBtn);
        frame.add(errorBtn);
        frame.add(warningBtn);
        frame.add(confirmBtn);
        frame.add(inputBtn);
        frame.add(optionBtn);

        infoBtn.addActionListener(e -> JOptionPane.showMessageDialog(frame, "This is an Information Dialog", "Info", JOptionPane.INFORMATION_MESSAGE));

        questionBtn.addActionListener(e -> JOptionPane.showMessageDialog(frame, "Is this a question?", "Question", JOptionPane.QUESTION_MESSAGE));

        errorBtn.addActionListener(e -> JOptionPane.showMessageDialog(frame, "An error occurred!", "Error", JOptionPane.ERROR_MESSAGE));

        warningBtn.addActionListener(e -> JOptionPane.showMessageDialog(frame, "This is a warning!", "Warning", JOptionPane.WARNING_MESSAGE));

        confirmBtn.addActionListener(e -> {
            int res = JOptionPane.showConfirmDialog(frame, "Do you confirm?", "Confirm", JOptionPane.YES_NO_OPTION);
            JOptionPane.showMessageDialog(frame, "You selected: " + (res == JOptionPane.YES_OPTION ? "Yes" : "No"));
        });

        inputBtn.addActionListener(e -> {
            String input = JOptionPane.showInputDialog(frame, "Enter your name:");
            JOptionPane.showMessageDialog(frame, "You entered: " + input);
        });

        optionBtn.addActionListener(e -> {
            Object[] options = {"Option 1", "Option 2", "Option 3"};
            int choice = JOptionPane.showOptionDialog(frame, "Choose an option", "Option Dialog", JOptionPane.DEFAULT_OPTION, JOptionPane.INFORMATION_MESSAGE, null, options, options[0]);
            JOptionPane.showMessageDialog(frame, "You selected: " + options[choice]);
        });

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
