import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class lab6 extends JFrame {
    int x = 150, y = 150;
    Image image;

    public lab6() {
        setTitle("");
        setSize(400, 400);

        image = new ImageIcon("img.png").getImage();

        addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                int key = e.getKeyCode();

                if (key == KeyEvent.VK_LEFT) {
                    x -= 10;
                } else if (key == KeyEvent.VK_RIGHT) {
                    x += 10;
                } else if (key == KeyEvent.VK_UP) {
                    y -= 10;
                } else if (key == KeyEvent.VK_DOWN) {
                    y += 10;
                }

                repaint();
            }
        });

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
        setFocusable(true);
    }

    public void paint(Graphics g) {
        super.paint(g);
        g.drawImage(image, x, y, 50, 50, this);
    }

    public static void main(String[] args) {
        new lab6();
    }
}
