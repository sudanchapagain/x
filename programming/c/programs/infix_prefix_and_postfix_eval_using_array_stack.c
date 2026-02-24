#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAX 200

int intStack[MAX];
int top = -1;

void pushInt(int x) {
    intStack[++top] = x;
}

int popInt() {
    return intStack[top--];
}

char charStack[MAX];
int top2 = -1;

void pushChar(char c) {
    charStack[++top2] = c;
}

char popChar() {
    return charStack[top2--];
}

char peekChar() {
    return charStack[top2];
}

int isEmptyChar() {
    return top2 == -1;
}

int precedence(char op) {
    if (op == '*' || op == '/') return 2;
    if (op == '+' || op == '-') return 1;
    return 0;
}

int applyOp(int a, int b, char op) {
    switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/': return a / b;
    }
    return 0;
}

int evalPostfix(char expr[]) {
    top = -1;

    for (int i = 0; expr[i]; i++) {
        if (isspace(expr[i])) {
            continue;
        }

        if (isdigit(expr[i])) {
            int num = 0;

            while (isdigit(expr[i])) {
                num = num * 10 + (expr[i] - '0');
                i++;
            }

            i--;
            pushInt(num);
        } else {
            int b = popInt();
            int a = popInt();
            pushInt(applyOp(a, b, expr[i]));
        }
    }
    return popInt();
}

int evalPrefix(char expr[]) {
    top = -1;
    int len = strlen(expr);

    for (int i = len - 1; i >= 0; i--) {
        if (isspace(expr[i])) {
            continue;
        }

        if (isdigit(expr[i])) {
            int num = 0, base = 1;

            while (i >= 0 && isdigit(expr[i])) {
                num = num + (expr[i] - '0') * base;
                base *= 10;
                i--;
            }

            i++;
            pushInt(num);
        } else {
            int a = popInt();
            int b = popInt();
            pushInt(applyOp(a, b, expr[i]));
        }
    }

    return popInt();
}

int evalInfix(char expr[]) {
    top = -1;
    top2 = -1;

    for (int i = 0; expr[i]; i++) {
        if (isspace(expr[i])) continue;

        if (isdigit(expr[i])) {
            int num = 0;

            while (isdigit(expr[i])) {
                num = num * 10 + (expr[i] - '0');
                i++;
            }

            i--;
            pushInt(num);
        } else if (expr[i] == '(') {
            pushChar('(');
        } else if (expr[i] == ')') {
            while (!isEmptyChar() && peekChar() != '(') {
                int b = popInt();
                int a = popInt();
                char op = popChar();
                pushInt(applyOp(a, b, op));
            }
            popChar();
        } else {
            while (
                !isEmptyChar() &&
                precedence(peekChar()) >= precedence(expr[i])
            ) {
                int b = popInt();
                int a = popInt();
                char op = popChar();
                pushInt(applyOp(a, b, op));
            }

            pushChar(expr[i]);
        }
    }

    while (!isEmptyChar()) {
        int b = popInt();
        int a = popInt();
        char op = popChar();
        pushInt(applyOp(a, b, op));
    }

    return popInt();
}

int main() {
    char infix[MAX], postfix[MAX], prefix[MAX];

    printf("enter infix   : ");
    fgets(infix, MAX, stdin);

    printf("enter postfix : ");
    fgets(postfix, MAX, stdin);

    printf("enter prefix  : ");
    fgets(prefix, MAX, stdin);

    printf("infix value   = %d\n", evalInfix(infix));
    printf("postfix value = %d\n", evalPostfix(postfix));
    printf("prefix value  = %d\n", evalPrefix(prefix));

    return 0;
}
