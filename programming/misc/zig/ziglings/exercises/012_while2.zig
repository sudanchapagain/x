const std = @import("std");

pub fn main() void {
    var n: u32 = 2;

    // Please set the continue expression so that we get the desired
    // results in the print statement below.
    while (n < 1000) : (n *= 2) {
        // Print the current number
        std.debug.print("{} ", .{n});
    }

    // As in the last exercise, we want this to result in "n=1024"
    std.debug.print("n={}\n", .{n});
}
