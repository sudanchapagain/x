#include <GL/glut.h>

const int WIDTH = 600;
const int HEIGHT = 400;

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    float points[3][2] = {
        {100, 100},
        {150, 100},
        {125, 150}
    };
    glColor3f(0, 0, 1);
    glBegin(GL_TRIANGLES);
    for (int i = 0; i < 3; i++) {
        glVertex2f(points[i][0], points[i][1]);
    }
    glEnd();
    float pivot_x = 250.0f;
    glColor3f(1, 0, 0);
    glBegin(GL_TRIANGLES);
    for (int i = 0; i < 3; i++) {
        float x = points[i][0] - pivot_x;
        float y = points[i][1];
        float xr = -x;
        float yr = y;
        xr += pivot_x;
        glVertex2f(xr, yr);
    }
    glEnd();
    glFlush();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(WIDTH, HEIGHT);
    glutCreateWindow("2d reflection");
    glClearColor(0, 0, 0, 0);
    gluOrtho2D(0, WIDTH, 0, HEIGHT);
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}
