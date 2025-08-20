#include <GL/glut.h>

const int WIDTH = 600;
const int HEIGHT = 400;

void display() {
  glClear(GL_COLOR_BUFFER_BIT);

  float points[3][2] = {{100, 100}, {150, 100}, {125, 150}};

  glColor3f(0, 0, 1);
  glBegin(GL_TRIANGLES);

  for (int i = 0; i < 3; i++) {
    glVertex2f(points[i][0], points[i][1]);
  }

  glEnd();
  float tx = 150.0f;
  float ty = 50.0f;
  float sx = 2.0f;
  float sy = 2.0f;

  glColor3f(1, 0, 0);
  glBegin(GL_TRIANGLES);

  for (int i = 0; i < 3; i++) {
    float x = points[i][0] * sx + tx;
    float y = points[i][1] * sy + ty;
    glVertex2f(x, y);
  }

  glEnd();
  glFlush();
}

int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, HEIGHT);
  glutCreateWindow("2d scaling");
  glClearColor(0, 0, 0, 0);
  gluOrtho2D(0, WIDTH, 0, HEIGHT);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}
