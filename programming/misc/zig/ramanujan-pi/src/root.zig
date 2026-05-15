const std = @import("std");
const math = std.math;

fn fact(n: u64) f64 {
    var r: f64 = 1.0;
    var i: u64 = 1;
    while (i <= n) : (i += 1) {
        r *= @floatFromInt(i);
    }
    return r;
}

pub fn ramanujan_pi() !f64 {
    const one: f64 = 1.0;
    const pi_atan = 4.0 * math.atan(one);

    const k: u64 = 0;
    const numerator = fact(4 * k) * (1103.0 + 26390.0 * @as(f64, k));
    const denominator = math.pow(f64, fact(k), 4) * math.pow(f64, 396.0, 4.0 * @as(f64, k));

    const sum = numerator / denominator;
    const inv_pi = (2.0 * math.sqrt(2.0) / 9801.0) * sum;
    const ram_pi = 1.0 / inv_pi;

    std.debug.print("pi from atan = {}\n", .{pi_atan});

    std.debug.print("Ramanujan pi (k=0) = {}\n", .{ram_pi});

    std.debug.print("error = {}\n", .{pi_atan - ram_pi});

    return ram_pi;
}
