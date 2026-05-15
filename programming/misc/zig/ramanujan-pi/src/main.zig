const std = @import("std");
const ramanujan_pi = @import("ramanujan_pi");

pub fn main() !void {
    const res = try ramanujan_pi.ramanujan_pi();
    std.debug.print("{}", .{res});
}
