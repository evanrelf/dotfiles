const std = @import("std");

pub fn main() anyerror!void {
    const name = "Evan";
    std.debug.print("Hello, {s}!\n", .{name});
}
