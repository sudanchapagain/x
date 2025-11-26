#include <stdio.h>

int fib(int n) {
    if (n < 3) {
        return 1;
    }

    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    int n;
    printf("enter the term upto: ");
    scanf("%d", n);
    printf("\nfibonacci series: ");
    for (int i = 1; i <= n; i++) {
        printf("%d, ", fib(i));
    }

    return 0;
}
