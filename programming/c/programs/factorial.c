#include <stdio.h>

int fact(int n) {
    if (n < 2) {
        return 1;
    }

    return n * fact(n - 1);
}

int main(void) {
    int n;
    printf("enter a number: ");
    scanf("%d", &n);

    printf("\nfactorial of a number is %d", fact(n));
}
