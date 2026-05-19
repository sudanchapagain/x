#include "allocator.h"
#include <stdalign.h>
#include <stdio.h>

void
allocator_init (Allocator* allocator)
{
    allocator->offset = 0;
}

void*
allocator_allocate (Allocator* allocator, size_t size, size_t alignment)
{
    if (alignment == 0)
        alignment = alignof (max_align_t);

    uintptr_t base    = (uintptr_t)allocator->buffer;
    uintptr_t current = base + allocator->offset;

    uintptr_t aligned = (current + alignment - 1) & ~(alignment - 1);
    size_t padding    = (size_t)(aligned - current);

    if (allocator->offset + padding + size > ALLOCATOR_POOL_SIZE) {
        printf ("out of memory\n");
        return NULL;
    }

    allocator->offset += padding + size;
    return (void*)aligned;
}

void
allocator_deallocate (Allocator* allocator, void* ptr)
{
    (void)allocator;
    printf ("deallocated: %p\n", ptr);
}

void
allocator_reset (Allocator* allocator)
{
    allocator->offset = 0;
    printf ("mem reset\n");
}
