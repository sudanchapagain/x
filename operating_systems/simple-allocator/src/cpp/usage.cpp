#include "allocator.hpp"
#include <iostream>

int
main ()
{
    Allocator allocator;
    int* a    = static_cast<int*> (allocator.allocate (sizeof (int)));
    *a        = 123;
    double* b = static_cast<double*> (allocator.allocate (sizeof (double)));
    *b        = 3.14;
    std::cout << *a << ", " << *b << "\n";
    allocator.reset ();
}
