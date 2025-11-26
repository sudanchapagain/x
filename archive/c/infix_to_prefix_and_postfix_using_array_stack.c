#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX 200

char stack[MAX];
int top = -1;

void push(char x) {
    stack[++top] = x;
}

char pop() {
    if (top == -1) return '\0';
    return stack[top--];
}

int precedence(char c) {
    if (c == '^') return 3;
    if (c == '*' || c == '/') return 2;
    if (c == '+' || c == '-') return 1;
    return 0;
}

int is_empty() {
    return top == -1;
}

void reverse(char *s) {
    int i, j;
    char t;

    for (
        i = 0, j = strlen(s) - 1;
        i < j;
        i++, j--
    ) {
        t = s[i];
        s[i] = s[j];
        s[j] = t;
    }
}

void infix_to_postfix(char *infix, char *postfix) {
    int i = 0, j = 0;
    char c;

    top = -1;

    while (infix[i] != '\0') {
        c = infix[i];

        if (isspace(c)) {
            i++;
            continue;
        }

        if (isdigit(c)) {
            while (isdigit(infix[i])) {
                postfix[j++] = infix[i++];
            }

            postfix[j++] = ' ';
            continue;
        }

        if (c == '(') {
            push(c);
        } else if (c == ')') {
            while (!is_empty() && (c = pop()) != '(') {
                postfix[j++] = c, postfix[j++] = ' ';
            }
        } else {
            while (!is_empty() && precedence(stack[top]) >= precedence(c)) {
                postfix[j++] = pop();
                postfix[j++] = ' ';
            }
            push(c);
        }
        i++;
    }

    while (!is_empty()) {
        postfix[j++] = pop();
        postfix[j++] = ' ';
    }

    postfix[j] = '\0';
}

void infix_to_prefix(char *infix, char *prefix) {
    char temp[MAX];
    strcpy(temp, infix);

    reverse(temp);

    for (int i = 0; temp[i]; i++) {
        if (temp[i] == '(') {
            temp[i] = ')';
        } else if (temp[i] == ')') {
            temp[i] = '(';
        }
    }

    char postfix[MAX];
    infix_to_postfix(temp, postfix);

    strcpy(prefix, postfix);
    reverse(prefix);
}

int main() {
    char infix[MAX], postfix[MAX], prefix[MAX];

    printf("Enter infix expression: ");
    fgets(infix, MAX, stdin);
    infix[strcspn(infix, "\n")] = '\0';

    infix_to_postfix(infix, postfix);
    infix_to_prefix(infix, prefix);

    printf("Postfix : %s\n", postfix);
    printf("Prefix  : %s\n", prefix);

    return 0;
}
