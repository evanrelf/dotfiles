const std = @import("std");

pub fn main() anyerror!void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();
    const allocator = &general_purpose_allocator.allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const name = if (args.len > 1) args[1] else "world";

    std.log.info("Hello, {s}!", .{name});
}
