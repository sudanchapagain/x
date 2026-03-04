const std = @import("std");

pub fn main() void {
    const foo = 1;

    // Please fix this condition:
    if (foo == 1) {
        // We want our program to print this message!
        std.debug.print("Foo is 1!\n", .{});
    } else {
        std.debug.print("Foo is not 1!\n", .{});
    }
}
