#include <GL/glut.h>
#include <cmath>

const int WIDTH = 500;
const int HEIGHT = 500;
const float PI = 3.14159265;

void display() {
  glClear(GL_COLOR_BUFFER_BIT);
  glPointSize(2);

  float x1 = 100, y1 = 100;
  float x2 = 200, y2 = 100;
  float x3 = 150, y3 = 200;

  glColor3f(1, 1, 1);
  glBegin(GL_LINE_LOOP);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  glVertex2f(x3, y3);
  glEnd();

  float angle_deg = 15;
  float angle_rad = angle_deg * PI / 180;
  float cos_theta = cos(angle_rad);
  float sin_theta = sin(angle_rad);
  float rx1 = x1 * cos_theta - y1 * sin_theta;
  float ry1 = x1 * sin_theta + y1 * cos_theta;
  float rx2 = x2 * cos_theta - y2 * sin_theta;
  float ry2 = x2 * sin_theta + y2 * cos_theta;
  float rx3 = x3 * cos_theta - y3 * sin_theta;
  float ry3 = x3 * sin_theta + y3 * cos_theta;

  glColor3f(1, 0, 0);
  glBegin(GL_LINE_LOOP);
  glVertex2f(rx1, ry1);
  glVertex2f(rx2, ry2);
  glVertex2f(rx3, ry3);
  glEnd();
  glFlush();
}

int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, HEIGHT);
  glutCreateWindow("2d rotation");
  glClearColor(0, 0, 0, 0);
  gluOrtho2D(0, WIDTH, 0, HEIGHT);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}
