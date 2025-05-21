#include <GL/glut.h>
#include <cmath>

const int WIDTH = 600;
const int HEIGHT = 600;

int xc = 300, yc = 300;
int rx = 150, ry = 100;

void drawSymmetricPoints(int x, int y) {
    glBegin(GL_POINTS);
    glVertex2i(xc + x, yc + y);
    glVertex2i(xc - x, yc + y);
    glVertex2i(xc + x, yc - y);
    glVertex2i(xc - x, yc - y);
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(1, 0, 0);
    glPointSize(2);
    int x = 0;
    int y = ry;
    float rx2 = rx * rx;
    float ry2 = ry * ry;
    float two_rx2 = 2 * rx2;
    float two_ry2 = 2 * ry2;
    float p1 = ry2 - (rx2 * ry) + (0.25f * rx2);
    float dx = two_ry2 * x;
    float dy = two_rx2 * y;
    while (dx < dy) {
        drawSymmetricPoints(x, y);
        x++;
        dx += two_ry2;
        if (p1 < 0) {
            p1 += ry2 + dx;
        } else {
            y--;
            dy -= two_rx2;
            p1 += ry2 + dx - dy;
        }
    }
    float p2 = ry2 * (x + 0.5f) * (x + 0.5f) + rx2 * (y - 1) * (y - 1) - rx2 * ry2;
    while (y >= 0) {
        drawSymmetricPoints(x, y);
        y--;
        dy -= two_rx2;
        if (p2 > 0) {
            p2 += rx2 - dy;
        } else {
            x++;
            dx += two_ry2;
            p2 += rx2 - dy + dx;
        }
    }
    glFlush();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(WIDTH, HEIGHT);
    glutCreateWindow("midpoint ellipse");
    glClearColor(0, 0, 0, 0);
    gluOrtho2D(0, WIDTH, 0, HEIGHT);
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}
