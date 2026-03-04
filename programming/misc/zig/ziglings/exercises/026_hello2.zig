const std = @import("std");

pub fn main() !void {
    // We get a Writer for Standard Out so we can print() to it.
    var stdout = std.fs.File.stdout().writer(&.{});

    // Unlike std.debug.print(), the Standard Out writer can fail
    // with an error. We don't care _what_ the error is, we want
    // to be able to pass it up as a return value of main().
    //
    // We just learned of a single statement which can accomplish this.
    try stdout.interface.print("Hello world!\n", .{});
}
