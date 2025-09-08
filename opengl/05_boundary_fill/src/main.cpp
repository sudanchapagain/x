#include <GL/glut.h>

const int WIDTH = 500;
const int HEIGHT = 500;

GLubyte fillColor[3] = {255, 255, 0};
GLubyte boundaryColor[3] = {255, 0, 0};

void boundaryFill(int x, int y) {
  GLubyte pixel[3];

  glReadPixels(x, y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, pixel);

  bool isBoundary =
      (pixel[0] == boundaryColor[0] && pixel[1] == boundaryColor[1] &&
       pixel[2] == boundaryColor[2]);
  bool isFilled = (pixel[0] == fillColor[0] && pixel[1] == fillColor[1] &&
                   pixel[2] == fillColor[2]);

  if (!isBoundary && !isFilled) {
    glColor3ubv(fillColor);
    glBegin(GL_POINTS);
    glVertex2i(x, y);
    glEnd();
    glFlush();

    boundaryFill(x + 1, y);
    boundaryFill(x - 1, y);
    boundaryFill(x, y + 1);
    boundaryFill(x, y - 1);
  }
}

void display() {
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3ubv(boundaryColor);
  glBegin(GL_LINE_LOOP);

  glVertex2i(200, 200);
  glVertex2i(300, 200);
  glVertex2i(300, 300);
  glVertex2i(200, 300);

  glEnd();
  glFlush();
  boundaryFill(250, 250);
}

int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, HEIGHT);
  glutCreateWindow("boundary fill");
  glClearColor(0, 0, 0, 0);
  gluOrtho2D(0, WIDTH, 0, HEIGHT);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}
