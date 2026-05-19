#pragma once

#include <stddef.h>
#include <stdint.h>

#define ALLOCATOR_POOL_SIZE 1024

typedef struct
{
    unsigned char buffer[ALLOCATOR_POOL_SIZE];
    size_t offset;
} Allocator;

void allocator_init(Allocator* allocator);
void* allocator_allocate(Allocator* allocator, size_t size, size_t alignment);
void allocator_deallocate(Allocator* allocator, void* ptr);
void allocator_reset(Allocator* allocator);

