#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>

class Allocator
{
   public:
    static constexpr std::size_t poolSize = 1024;  /// mem pool

    Allocator() : offset(0) {}  /// init 0s

    /// memory of size bytes with given alignment
    void* allocate(std::size_t size, std::size_t alignment = alignof(std::max_align_t))
    {
        std::uintptr_t base = reinterpret_cast<std::uintptr_t>(buffer);
        std::uintptr_t current = base + offset;

        /// align the current address
        /// <https://www.youtube.com/watch?v=E0QhZ6tNoRg>
        /// <https://codeinterstellar.medium.com/optimizing-memory-usage-in-c-a-deep-dive-into-data-alignment-and-padding-fd0ea3999aed>
        std::uintptr_t aligned = (current + alignment - 1) & ~(alignment - 1);
        std::size_t padding = aligned - current;

        /// space check
        if (offset + padding + size > poolSize)
        {
            std::cerr << "out of memory\n";
            return nullptr;
        }

        void* ptr = reinterpret_cast<void*>(aligned);  /// usable address
        offset += padding + size;                      /// move the offset

        return ptr;
    }

    void deallocate(void* ptr)
    {
        std::cout << "deallocated: " << ptr << "\n";  /// fake deallocate
    }

    /// reset allocator
    void reset()
    {
        offset = 0;
        std::cout << "mem reset\n";
    }

   private:
    alignas(std::max_align_t) char buffer[poolSize];  /// raw memory
    std::size_t offset;                               // the next allocation
};
