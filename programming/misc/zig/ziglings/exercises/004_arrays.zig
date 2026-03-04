const std = @import("std");

pub fn main() void {
    // (Problem 1)
    // This "const" is going to cause a problem later - can you see what it is?
    // How do we fix it?
    var some_primes = [_]u8{ 1, 3, 5, 7, 11, 13, 17, 19 };

    // Individual values can be set with '[]' notation.
    // Example: This line changes the first prime to 2 (which is correct):
    some_primes[0] = 2;

    // Individual values can also be accessed with '[]' notation.
    // Example: This line stores the first prime in "first":
    const first = some_primes[0];

    // (Problem 2)
    // Looks like we need to complete this expression. Use the example
    // above to set "fourth" to the fourth element of the some_primes array:
    const fourth = some_primes[3];

    // (Problem 3)
    // Use the len property to get the length of the array:
    const length = some_primes.len;

    std.debug.print("First: {}, Fourth: {}, Length: {}\n", .{
        first, fourth, length,
    });
}
