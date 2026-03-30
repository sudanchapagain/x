import javax.swing.*;
import java.awt.*;

public class lab2 {
    public static void main(String[] args) {
        JFrame frame = new JFrame("");
        frame.setSize(700, 700);
        frame.setLayout(new GridLayout(3, 2));

        JPanel flowPanel = new JPanel(new FlowLayout());
        flowPanel.setBorder(BorderFactory.createTitledBorder("Flow Layout"));
        flowPanel.add(new JButton("One"));
        flowPanel.add(new JButton("Two"));
        flowPanel.add(new JButton("Three"));
        frame.add(flowPanel);

        JPanel borderPanel = new JPanel(new BorderLayout());
        borderPanel.setBorder(BorderFactory.createTitledBorder("Border Layout"));
        borderPanel.add(new JButton("North"), BorderLayout.NORTH);
        borderPanel.add(new JButton("South"), BorderLayout.SOUTH);
        borderPanel.add(new JButton("East"), BorderLayout.EAST);
        borderPanel.add(new JButton("West"), BorderLayout.WEST);
        borderPanel.add(new JButton("Center"), BorderLayout.CENTER);
        frame.add(borderPanel);

        JPanel gridPanel = new JPanel(new GridLayout(2, 3));
        gridPanel.setBorder(BorderFactory.createTitledBorder("Grid Layout"));
        for (int i = 1; i <= 6; i++) {
            gridPanel.add(new JButton("Btn " + i));
        }
        frame.add(gridPanel);

        JPanel gridBagPanel = new JPanel(new GridBagLayout());
        gridBagPanel.setBorder(BorderFactory.createTitledBorder("GridBag Layout"));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gridBagPanel.add(new JButton("A"), gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gridBagPanel.add(new JButton("B"), gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gridBagPanel.add(new JButton("Wide Button"), gbc);
        frame.add(gridBagPanel);

        JPanel groupPanel = new JPanel();
        groupPanel.setBorder(BorderFactory.createTitledBorder("Group Layout"));
        GroupLayout layout = new GroupLayout(groupPanel);
        groupPanel.setLayout(layout);

        JButton g1 = new JButton("G1");
        JButton g2 = new JButton("G2");
        JButton g3 = new JButton("G3");

        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(true);

        layout.setHorizontalGroup(
            layout.createSequentialGroup()
                .addComponent(g1)
                .addComponent(g2)
                .addComponent(g3)
        );

        layout.setVerticalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                .addComponent(g1)
                .addComponent(g2)
                .addComponent(g3)
        );

        frame.add(groupPanel);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }
}
