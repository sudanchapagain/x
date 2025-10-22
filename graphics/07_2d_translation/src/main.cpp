#include <GL/glut.h>

const int WIDTH = 500;
const int HEIGHT = 500;

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

  float tx = 100;
  float ty = 50;

  x1 = x1 + tx;
  y1 = y1 + ty;
  x2 = x2 + tx;
  y2 = y2 + ty;
  x3 = x3 + tx;
  y3 = y3 + ty;

  glColor3f(1, 0, 0);
  glBegin(GL_LINE_LOOP);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  glVertex2f(x3, y3);

  glEnd();
  glFlush();
}

int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, HEIGHT);
  glutCreateWindow("2D Translation");
  glClearColor(0, 0, 0, 0);
  gluOrtho2D(0, WIDTH, 0, HEIGHT);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}
