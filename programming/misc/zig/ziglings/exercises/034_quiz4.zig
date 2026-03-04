const std = @import("std");

const NumError = error{IllegalNumber};

pub fn main() !void {
    var stdout = std.fs.File.stdout().writer(&.{});

    const my_num: NumError!u32 = getNumber();

    try stdout.interface.print("my_num={!}\n", .{my_num});
}

// This function is obviously weird and non-functional. But you will not be changing it for this quiz.
fn getNumber() NumError!u32 {
    if (false) return NumError.IllegalNumber;
    return 42;
}
