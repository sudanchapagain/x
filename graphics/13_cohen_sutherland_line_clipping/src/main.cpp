#include <GL/glut.h>
#include <iostream>

const int INSIDE = 0; // 0000
const int LEFT = 1; // 0001
const int RIGHT = 2; // 0010
const int BOTTOM = 4; /// 0100
const int TOP = 8; // 1000
const int window_x_min = 100;
const int window_y_min = 100;
const int window_x_max = 400;
const int window_y_max = 400;
float line_x_start = 50, line_y_start = 50;
float line_x_end   = 450, line_y_end = 450;

int computeRegionCode(float x, float y) {
    int code = INSIDE;
    if (x < window_x_min) {
        code |= LEFT;
    } else if (x > window_x_max) {
        code |= RIGHT;
    }
    if (y < window_y_min) {
        code |= BOTTOM;
    } else if (y > window_y_max) {
        code |= TOP;
    }
    return code;
}

void drawClippingWindow() {
    glColor3f(1, 0, 0);
    glBegin(GL_LINE_LOOP);
    glVertex2i(window_x_min, window_y_min);
    glVertex2i(window_x_max, window_y_min);
    glVertex2i(window_x_max, window_y_max);
    glVertex2i(window_x_min, window_y_max);
    glEnd();
}

void drawLine(float x_start, float y_start, float x_end, float y_end, float r, float g, float b) {
    glColor3f(r, g, b);
    glBegin(GL_LINES);
    glVertex2f(x_start, y_start);
    glVertex2f(x_end, y_end);
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    drawClippingWindow();
    drawLine(line_x_start, line_y_start, line_x_end, line_y_end, 1.0, 0.0, 0.0);
    float x0 = line_x_start, y0 = line_y_start;
    float x1 = line_x_end, y1 = line_y_end;
    int code0 = computeRegionCode(x0, y0);
    int code1 = computeRegionCode(x1, y1);
    bool is_line_visible = false;
    while (true) {
        if ((code0 | code1) == 0) {
            is_line_visible = true;
            break;
        } else if ((code0 & code1) != 0) {
            break;
        } else {
            int region_code_out = code0 ? code0 : code1;
            float x, y;
            if (region_code_out & TOP) {
                x = x0 + (x1 - x0) * (window_y_max - y0) / (y1 - y0);
                y = window_y_max;
            } else if (region_code_out & BOTTOM) {
                x = x0 + (x1 - x0) * (window_y_min - y0) / (y1 - y0);
                y = window_y_min;
            } else if (region_code_out & RIGHT) {
                y = y0 + (y1 - y0) * (window_x_max - x0) / (x1 - x0);
                x = window_x_max;
            } else if (region_code_out & LEFT) {
                y = y0 + (y1 - y0) * (window_x_min - x0) / (x1 - x0);
                x = window_x_min;
            }
            if (region_code_out == code0) {
                x0 = x;
                y0 = y;
                code0 = computeRegionCode(x0, y0);
            } else {
                x1 = x;
                y1 = y;
                code1 = computeRegionCode(x1, y1);
            }
        }
    }
    if (is_line_visible) {
        drawLine(x0, y0, x1, y1, 0.0, 0.0, 1.0);
    }
    glFlush();
}

void initGraphics() {
    glClearColor(0, 0, 0, 1);
    gluOrtho2D(0, 500, 0, 500);
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutInitWindowSize(500, 500);
    glutCreateWindow("cohen sutherland line clipping");
    initGraphics();
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}

