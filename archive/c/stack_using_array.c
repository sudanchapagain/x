#include <stdio.h>

#define MAX 5

int top = -1;

void push(int num, int stack[]) {
    if (top == MAX - 1) {
        printf("\nStack Overflow");
        return;
    }

    top += 1;
    stack[top] = num;
}

void pop() {
    if (top == -1) {
        printf("\nStack underflow");
        return;
    }

    top -= 1;
}

int main() {
    int selected_event, num, run;
    int stack[MAX];

    while (1) {
        printf("\nSelect the option:\n1 to push\n2 to pop\n3 to display\n4 to exit\n");
        scanf("%d", &selected_event);

        switch (selected_event) {
            case 1:
                printf("\nEnter the number to push: ");
                scanf("%d", &num);
                push(num, stack);
                break;
            case 2:
                pop();
                break;
            case 3:
                printf("\nThe current items in stack are:\n");
                for (int i = 0; i <= top; i++) {
                    printf("%d\n", stack[i]);
                }
                break;
            case 4: return 1;
            default:
                printf("unrecognized command");
        }
    }
}
