#include "allocator.h"
#include <stdalign.h>
#include <stdio.h>

int
main (void)
{
    Allocator allocator;
    allocator_init (&allocator);

    int* a = (int*)allocator_allocate (&allocator, sizeof (int), alignof (int));
    *a     = 123;

    double* b = (double*)allocator_allocate (&allocator, sizeof (double), alignof (double));
    *b        = 3.14;

    printf ("%d, %f\n", *a, *b);

    allocator_reset (&allocator);
    return 0;
}
