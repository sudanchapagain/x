const std = @import("std");

pub fn main() void {
    std.debug.print("Standard Library.\n", .{});
}

// For the curious: Imports must be declared as constants because they
// can only be used at compile time rather than run time. Zig evaluates
// constant values at compile time. Don't worry, we'll cover imports
// in detail later.
// Also see this answer: https://stackoverflow.com/a/62567550/695615
