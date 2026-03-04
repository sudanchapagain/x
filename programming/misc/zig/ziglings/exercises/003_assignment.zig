const std = @import("std");

pub fn main() void {
    var n: u16 = 50;
    n = n + 5;

    const pi: u32 = 314159;

    const negative_eleven: i8 = -11;

    std.debug.print("{} {} {}\n", .{ n, pi, negative_eleven });
}
