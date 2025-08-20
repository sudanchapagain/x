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

  int x = x_start;
  int y = y_start;
  int dx = abs(x_end - x_start);
  int dy = abs(y_end - y_start);
  int sx = x_start < x_end ? 1 : -1;
  int sy = y_start < y_end ? 1 : -1;
  bool is_steep = dy > dx;

  if (is_steep) {
    std::swap(dx, dy);
  }

  int p = 2 * dy - dx;

  for (int i = 0; i <= dx; i++) {
    glBegin(GL_POINTS);
    glVertex2i(x, y);
    glEnd();

    if (p >= 0) {
      if (is_steep) {
        x += sx;
      } else {
        y += sy;
      }
      p -= 2 * dx;
    }

    if (is_steep) {
      y += sy;
    } else {
      x += sx;
    }

    p += 2 * dy;
  }

  glFlush();
}

int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, HEIGHT);
  glutCreateWindow("bresenham line");
  glClearColor(0, 0, 0, 0);
  gluOrtho2D(0, WIDTH, 0, HEIGHT);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}
