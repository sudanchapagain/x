import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.awt.*;

public class lab7 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(700, 600);
        frame.setLayout(new FlowLayout());

        JLabel status = new JLabel("Status: ");
        frame.add(status);

        JSlider slider = new JSlider(0, 100, 50);
        slider.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                status.setText("Slider Value: " + slider.getValue());
            }
        });
        frame.add(slider);

        String[] items = {"Item 1", "Item 2", "Item 3"};
        JList<String> list = new JList<>(items);
        list.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                status.setText("Selected: " + list.getSelectedValue());
            }
        });
        frame.add(new JScrollPane(list));

        String[][] data = {
            {"1", "A"},
            {"2", "B"},
            {"3", "C"}
        };
        String[] cols = {"ID", "Name"};
        JTable table = new JTable(data, cols);
        frame.add(new JScrollPane(table));

        JProgressBar progressBar = new JProgressBar(0, 100);
        progressBar.setValue(30);
        progressBar.setStringPainted(true);
        frame.add(progressBar);

        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Root");
        DefaultMutableTreeNode child1 = new DefaultMutableTreeNode("Child 1");
        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Child 2");
        root.add(child1);
        root.add(child2);

        JTree tree = new JTree(root);
        frame.add(new JScrollPane(tree));

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
