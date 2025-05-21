#include <GL/glut.h>
#include <cmath>

const int WIDTH = 500;
const int HEIGHT = 500;

int x_start = 50, y_start = 50;
int x_end = 400, y_end = 300;

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(1, 0, 0);
    glPointSize(2);
    int dx = x_end - x_start;
    int dy = y_end - y_start;
    int steps = abs(dx) > abs(dy) ? abs(dx) : abs(dy);
    float x_inc = dx / (float)steps;
    float y_inc = dy / (float)steps;
    float x = x_start;
    float y = y_start;
    for (int i = 0; i <= steps; i++) {
        glBegin(GL_POINTS);
        glVertex2i(round(x), round(y));
        glEnd();
        x += x_inc;
        y += y_inc;
    }
    glFlush();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(WIDTH, HEIGHT);
    glutCreateWindow("dda line");
    glClearColor(0, 0, 0, 0);
    gluOrtho2D(0, WIDTH, 0, HEIGHT);
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}
