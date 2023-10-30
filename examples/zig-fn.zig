//! Registering a Zig function to be called from Lua

const std = @import("std");
const luna = @import("luna");

// It can be convenient to store a short reference to the Lua struct when
// it is used multiple times throughout a file.
const Luna = luna.Luna;

// A Zig function called by Lua must accept a single *Lua parameter and must return an i32.
// This is the Zig equivalent of the lua_CFunction typedef int (*lua_CFunction) (lua_State *L) in the C API
fn adder(L: *Luna) i32 {
    const a = L.luna_tointeger(1) catch 0;
    const b = L.luna_tointeger(2) catch 0;
    L.luna_pushinteger(a + b);
    return 1;
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Initialize The Lua vm and get a reference to the main thread
    var L = try Luna.init(allocator);
    defer L.deinit();

    // Push the adder function to the Lua stack.
    // Here we use ziglua.wrap() to convert from a Zig function to the lua_CFunction required by Lua.
    // This could be done automatically by pushFunction(), but that would require the parameter to be comptime-known.
    // The call to ziglua.wrap() is slightly more verbose, but has the benefit of being more flexible.
    L.luna_pushcfunction(luna.wrap(adder));

    // Push the arguments onto the stack
    L.luna_pushinteger(10);
    L.luna_pushinteger(32);

    // Call the function. It accepts 2 arguments and returns 1 value
    // We use catch unreachable because we can verify this function call will not fail
    L.luna_pcall(2, 1, 0) catch unreachable;

    // The result of the function call is on the stack.
    // Use toInteger to read the integer at index 1
    std.debug.print("the result: {}\n", .{L.luna_tointeger(1) catch unreachable});

    // We can also register the function to a global and run from a Lua "program"
    L.luna_pushcfunction(luna.wrap(adder));
    L.luna_setglobal("add");

    // We need to open the base library so the global print() is available
    L.lunaL_openlibs();

    // Our "program" is an inline string
    L.lunaL_dostring(
        \\local sum = add(10, 32)
        \\print(sum)
    ) catch unreachable;
}
