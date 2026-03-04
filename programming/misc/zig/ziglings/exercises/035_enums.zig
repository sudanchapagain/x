const std = @import("std");

// Please complete the enum!
const Ops = enum { inc, pow, dec };

pub fn main() void {
    const operations = [_]Ops{
        Ops.inc,
        Ops.inc,
        Ops.inc,
        Ops.pow,
        Ops.dec,
        Ops.dec,
    };

    var current_value: u32 = 0;

    for (operations) |op| {
        switch (op) {
            Ops.inc => {
                current_value += 1;
            },
            Ops.dec => {
                current_value -= 1;
            },
            Ops.pow => {
                current_value *= current_value;
            },
            // No "else" needed! Why is that?
        }

        std.debug.print("{} ", .{current_value});
    }

    std.debug.print("\n", .{});
}
