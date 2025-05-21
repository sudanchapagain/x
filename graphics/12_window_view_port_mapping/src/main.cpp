#include <GL/glut.h>
#include <vector>
#include <iostream>

const int WIN_WIDTH = 600;
const int WIN_HEIGHT = 400;
float win_xmin = 0.0f, win_ymin = 0.0f;
float win_xmax = 100.0f, win_ymax = 100.0f;
int vp_xmin = 50, vp_ymin = 50;
int vp_xmax = 350, vp_ymax = 250;

std::vector<std::pair<float, float>> window_points = {
    {10, 10},
    {90, 10},
    {90, 90},
    {10, 90}
};

void windowToViewport(float xw, float yw, int& xv, int& yv) {
    xv = vp_xmin + (xw - win_xmin) * (vp_xmax - vp_xmin) / (win_xmax - win_xmin);
    yv = vp_ymin + (yw - win_ymin) * (vp_ymax - vp_ymin) / (win_ymax - win_ymin);
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(0, 1, 0);
    glBegin(GL_LINE_LOOP);
    for (auto& p : window_points) {
        int xv, yv;
        windowToViewport(p.first, p.second, xv, yv);
        glVertex2i(xv, yv);
    }
    glEnd();
    glFlush();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(WIN_WIDTH, WIN_HEIGHT);
    glutCreateWindow("window to viewport mapping");
    glClearColor(0, 0, 0, 1);
    gluOrtho2D(0, WIN_WIDTH, 0, WIN_HEIGHT);
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}
