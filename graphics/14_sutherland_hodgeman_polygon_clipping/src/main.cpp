#include <GL/glut.h>
#include <vector>

const int window_width = 500;
const int window_height = 500;
const int clip_x_min = 100;
const int clip_y_min = 100;
const int clip_x_max = 400;
const int clip_y_max = 400;

struct Point {
    float x, y;
};

std::vector<Point> original_polygon = {
    {150, 150},
    {300, 450},
    {450, 350},
    {350, 100},
    {200, 50}
};

std::vector<Point> clipped_polygon;

Point computeIntersection(Point p1, Point p2, int edge) {
    Point intersect;
    float m;

    if (p2.x != p1.x) {
        m = (p2.y - p1.y) / (p2.x - p1.x);
    } else {
        m = 1e5;
    }

    switch (edge) {
        case 0: // left
            intersect.x = clip_x_min;
            intersect.y = p1.y + m * (clip_x_min - p1.x);
            break;
        case 1:// right
            intersect.x = clip_x_max;
            intersect.y = p1.y + m * (clip_x_max - p1.x);
            break;
        case 2: // bottom
            intersect.y = clip_y_min;
            intersect.x = p1.x + (clip_y_min - p1.y) / m;
            break;
        case 3: // top
            intersect.y = clip_y_max;
            intersect.x = p1.x + (clip_y_max - p1.y) / m;
            break;
    }

    return intersect;
}

bool isInside(Point p, int edge) {
    switch (edge) {
        case 0: return p.x >= clip_x_min; // left
        case 1: return p.x <= clip_x_max; // right
        case 2: return p.y >= clip_y_min; // bottom
        case 3: return p.y <= clip_y_max; // tpp
    }
    return false;
}

void sutherlandHodgmanClip() {
    clipped_polygon = original_polygon;
    for (int edge = 0; edge < 4; ++edge) {
        std::vector<Point> input = clipped_polygon;
        clipped_polygon.clear();
        for (size_t i = 0; i < input.size(); ++i) {
            Point curr = input[i];
            Point prev = input[(i + input.size() - 1) % input.size()];
            bool curr_in = isInside(curr, edge);
            bool prev_in = isInside(prev, edge);
            if (prev_in && curr_in) {
                clipped_polygon.push_back(curr);
            } else if (prev_in && !curr_in) {
                Point inter = computeIntersection(prev, curr, edge);
                clipped_polygon.push_back(inter);
            } else if (!prev_in && curr_in) {
                Point inter = computeIntersection(prev, curr, edge);
                clipped_polygon.push_back(inter);
                clipped_polygon.push_back(curr);
            }
        }
    }
}

void drawPolygon(const std::vector<Point>& poly, float r, float g, float b) {
    glColor3f(r, g, b);
    glBegin(GL_POLYGON);
    for (auto& pt : poly) {
        glVertex2f(pt.x, pt.y);
    }
    glEnd();
}

void drawClippingWindow() {
    glColor3f(0.0, 1.0, 0.0);
    glBegin(GL_LINE_LOOP);
    glVertex2i(clip_x_min, clip_y_min);
    glVertex2i(clip_x_max, clip_y_min);
    glVertex2i(clip_x_max, clip_y_max);
    glVertex2i(clip_x_min, clip_y_max);
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT);
    drawClippingWindow();
    drawPolygon(original_polygon, 1.0, 0.0, 0.0);
    sutherlandHodgmanClip();
    drawPolygon(clipped_polygon, 0.0, 0.0, 1.0);
    glFlush();
}

void initGraphics() {
    glClearColor(0, 0, 0, 1);
    gluOrtho2D(0, window_width, 0, window_height);
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitWindowSize(window_width, window_height);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
    glutCreateWindow("sutherland hodgman polygon clipping");
    initGraphics();
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}
