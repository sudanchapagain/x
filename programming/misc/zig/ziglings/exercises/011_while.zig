const std = @import("std");

pub fn main() void {
    var n: u32 = 2;

    // Please use a condition that is true UNTIL "n" reaches 1024:
    while (n < 1024) {
        // Print the current number
        std.debug.print("{} ", .{n});

        // Set n to n multiplied by 2
        n *= 2;
    }

    // Once the above is correct, this will print "n=1024"
    std.debug.print("n={}\n", .{n});
}
