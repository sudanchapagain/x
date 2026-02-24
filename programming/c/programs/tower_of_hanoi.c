#include <stdio.h>

void toh(int n, char from, char to, char aux) {
    if (n == 1) {
        printf("move disk 1 from rod %c to rod %c", from, to);
        return;
    }

    toh(n - 1, from, aux, to);
    printf("move disk %d from rod %c to rod %c", n, from, to);
    toh(n - 1, aux, to, from);
}


int main(void) {
    int num;
    printf("enter the number of disks");
    scanf("%d", &num);

    printf("the following stem move the disks from A to C: ");
    toh(num, 'A', 'C', 'B');

    return 0;
}
