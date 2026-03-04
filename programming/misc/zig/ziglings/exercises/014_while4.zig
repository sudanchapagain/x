const std = @import("std");

pub fn main() void {
    var n: u32 = 1;

    // Oh dear! This while loop will go forever?!
    // Please fix this so the print statement below gives the desired output.
    while (true) : (n += 1) {
        if (n == 4) break;
    }

    // Result: we want n=4
    std.debug.print("n={}\n", .{n});
}
