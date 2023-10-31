const std = @import("std");
const luna = @import("luna.zig");
const testing = std.testing;

const LuaAlloc = luna.LuaAlloc;
const LuaDebugInfo = luna.LuaDebugInfo;
const LuaEvent = luna.LuaEvent;
const LuaInteger = luna.LuaInteger;
const LuaType = luna.LuaType;
const LuaNumber = luna.LuaNumber;
const LuaStatus = luna.LuaStatus;

const Luna = luna.Luna;
const LunaError = luna.LunaError;
const LunaBuffer = luna.LunaBuffer;

const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;
const expectError = testing.expectError;
const panic = std.debug.panic;

fn expectEqualStringsSentinel(expected: []const u8, actual: [*:0]const u8) !void {
    return expectEqualStrings(expected, std.mem.span(actual));
}

fn alloc(data: ?*anyopaque, ptr: ?*anyopaque, osize: usize, nsize: usize) callconv(.C) ?*anyopaque {
    _ = data;

    const alignment = @alignOf(std.c.max_align_t);
    if (@as(?[*]align(alignment) u8, @ptrCast(@alignCast(ptr)))) |prev_ptr| {
        const prev_slice = prev_ptr[0..osize];
        if (nsize == 0) {
            testing.allocator.free(prev_slice);
            return null;
        }
        const new_ptr = testing.allocator.realloc(prev_slice, nsize) catch return null;
        return new_ptr.ptr;
    } else if (nsize == 0) {
        return null;
    } else {
        const new_ptr = testing.allocator.alignedAlloc(u8, alignment, nsize) catch return null;
        return new_ptr.ptr;
    }
}

fn failing_alloc(data: ?*anyopaque, ptr: ?*anyopaque, osize: usize, nsize: usize) callconv(.C) ?*anyopaque {
    _ = data;
    _ = ptr;
    _ = osize;
    _ = nsize;
    return null;
}

test "initialization" {
    // initialize the Zig wrapper
    var lua = try Luna.init(testing.allocator);
    try expectEqual(LuaStatus.ok, lua.luna_status());
    lua.deinit();

    // attempt to initialize the Zig wrapper with no memory
    try expectError(error.Memory, Luna.init(testing.failing_allocator));

    // use the library directly
    lua = try Luna.luna_newstate(alloc, null);
    lua.luna_close();

    // use the library with a bad AllocFn
    try expectError(error.Memory, Luna.luna_newstate(failing_alloc, null));

    // use the auxiliary library (uses libc realloc and cannot be checked for leaks!)
    lua = try Luna.lunaL_newstate();
    lua.luna_close();
}

// until issue #1717 we need to use the struct workaround
const add = struct {
    fn addInner(l: *Luna) i32 {
        const a = l.luna_tointeger(1) catch 0;
        const b = l.luna_tointeger(2) catch 0;
        l.luna_pushinteger(a + b);
        return 1;
    }
}.addInner;

const sub = struct {
    fn subInner(l: *Luna) i32 {
        const a = l.luna_tointeger(1) catch 0;
        const b = l.luna_tointeger(2) catch 0;
        l.luna_pushinteger(a - b);
        return 1;
    }
}.subInner;

test "alloc functions" {
    var lua = try Luna.luna_newstate(alloc, null);
    defer lua.deinit();

    // get default allocator
    var data: *anyopaque = undefined;
    try expectEqual(@as(LuaAlloc, alloc), lua.luna_getallocf(&data));

    // set a bad allocator
    lua.luna_setallocf(failing_alloc, null);
    try expectEqual(@as(LuaAlloc, failing_alloc), lua.luna_getallocf(&data));

    // reset the good one
    lua.luna_setallocf(alloc, null);
}

test "luna_arith" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.luna_pushnumber(10);
    lua.luna_pushnumber(42);

    lua.luna_arith(.add);
    try expectEqual(@as(f64, 52), try lua.luna_tonumber(1));

    lua.luna_pushnumber(12);
    lua.luna_arith(.sub);
    try expectEqual(@as(f64, 40), try lua.luna_tonumber(1));

    lua.luna_pushnumber(2);
    lua.luna_arith(.mul);
    try expectEqual(@as(f64, 80), try lua.luna_tonumber(1));

    lua.luna_pushnumber(8);
    lua.luna_arith(.div);
    try expectEqual(@as(f64, 10), try lua.luna_tonumber(1));

    // prep for idiv
    lua.luna_pushnumber(1);
    lua.luna_arith(.add);
    lua.luna_pushnumber(2);
    lua.luna_arith(.int_div);
    try expectEqual(@as(f64, 5), try lua.luna_tonumber(1));

    lua.luna_pushnumber(2);
    lua.luna_arith(.mod);
    try expectEqual(@as(f64, 1), try lua.luna_tonumber(1));

    lua.luna_arith(.negate);
    try expectEqual(@as(f64, -1), try lua.luna_tonumber(1));

    lua.luna_arith(.negate);
    lua.luna_pushnumber(2);
    lua.luna_arith(.shl);
    try expectEqual(@as(i64, 4), try lua.luna_tointeger(1));

    lua.luna_pushnumber(1);
    lua.luna_arith(.shr);
    try expectEqual(@as(i64, 2), try lua.luna_tointeger(1));

    lua.luna_pushnumber(4);
    lua.luna_arith(.bor);
    try expectEqual(@as(i64, 6), try lua.luna_tointeger(1));

    lua.luna_pushnumber(1);
    lua.luna_arith(.band);
    try expectEqual(@as(i64, 0), try lua.luna_tointeger(1));

    lua.luna_pushnumber(1);
    lua.luna_arith(.bxor);
    try expectEqual(@as(i64, 1), try lua.luna_tointeger(1));

    lua.luna_arith(.bnot); // 0xFFFFFFFFFFFFFFFE which is -2
    try expectEqual(@as(i64, -2), try lua.luna_tointeger(1));

    lua.luna_pushnumber(3);
    lua.luna_arith(.pow);
    try expectEqual(@as(i64, -8), try lua.luna_tointeger(1));
}

test "luna_compare" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.luna_pushnumber(1);
    lua.luna_pushnumber(2);
    try expect(!lua.luna_compare(-2, -1, .eq));
    try expect(!lua.luna_compare(-1, -2, .le));
    try expect(!lua.luna_compare(-1, -2, .lt));
    try expect(lua.luna_compare(-2, -1, .le));
    try expect(lua.luna_compare(-2, -1, .lt));

    try expect(!lua.luna_rawequal(-1, -2));
    lua.luna_pushnumber(2);
    try expect(lua.luna_rawequal(-1, -2));
}

test "type of and getting values" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    var value: i32 = 0;

    lua.luna_pushnil();
    try expect(lua.luna_isnil(1));
    try expect(lua.luna_isnoneornil(1));
    try expect(lua.luna_isnoneornil(2));
    try expect(lua.luna_isnone(2));
    lua.luna_pop(1);

    lua.luna_pushboolean(true);
    lua.luna_pushglobaltable();
    lua.luna_pushinteger(1);
    lua.luna_pushlightuserdata(&value);
    lua.luna_pushnil();
    lua.luna_pushnumber(0.1);
    _ = lua.luna_pushthread();
    try expectEqualStrings(
        "all your codebase are belong to us",
        lua.luna_pushstring("all your codebase are belong to us"),
    );

    lua.luna_pushcfunction(luna.wrap(add));

    try expectEqualStrings("hello world", lua.luna_pushlstring("hello world"));
    _ = lua.luna_pushfstring("%s %s %d", .{ "hello", "world", @as(i32, 10) });
    lua.luna_pushvalue(1);

    // test both typeof and is functions
    try expectEqual(LuaType.boolean, lua.luna_type(1));
    try expectEqual(LuaType.table, lua.luna_type(2));
    try expectEqual(LuaType.number, lua.luna_type(3));
    try expectEqual(LuaType.light_userdata, lua.luna_type(4));
    try expectEqual(LuaType.nil, lua.luna_type(5));
    try expectEqual(LuaType.number, lua.luna_type(6));
    try expectEqual(LuaType.thread, lua.luna_type(7));
    try expectEqual(LuaType.string, lua.luna_type(8));
    try expectEqual(LuaType.function, lua.luna_type(9));
    try expectEqual(LuaType.string, lua.luna_type(10));
    try expectEqual(LuaType.string, lua.luna_type(11));
    try expectEqual(LuaType.boolean, lua.luna_type(12));

    try expect(lua.luna_isboolean(1));
    try expect(lua.luna_istable(2));
    try expect(lua.luna_isnumber(3));
    try expect(lua.luna_islightuserdata(4));
    try expect(lua.luna_isuserdata(4));
    try expect(lua.luna_isnil(5));
    try expect(lua.luna_isnumber(6));
    try expect(lua.luna_isthread(7));
    try expect(lua.luna_isstring(8));
    try expect(lua.luna_iscfunction(9));
    try expect(lua.luna_isfunction(9));
    try expect(lua.luna_isstring(10));
    try expect(lua.luna_isstring(11));
    try expect(lua.luna_isboolean(12));

    try expectEqualStrings("hello world 10", std.mem.span(try lua.luna_tostring(11)));

    // the created thread should equal the main thread (but created thread has no allocator ref)
    try expectEqual(lua.state, (try lua.luna_tothread(7)).state);
    try expectEqual(@as(luna.LuaCFunction, luna.wrap(add)), try lua.luna_tocfunction(9));

    try expectEqual(@as(LuaNumber, 0.1), try lua.luna_tonumber(6));
    try expectEqual(@as(LuaInteger, 1), try lua.luna_tointeger(3));

    try expectEqualStrings("number", lua.lunaL_typename(3));
}

test "typenames" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try expectEqualStrings("no value", lua.luna_typename(.none));
    try expectEqualStrings("nil", lua.luna_typename(.nil));
    try expectEqualStrings("boolean", lua.luna_typename(.boolean));
    try expectEqualStrings("userdata", lua.luna_typename(.light_userdata));
    try expectEqualStrings("number", lua.luna_typename(.number));
    try expectEqualStrings("string", lua.luna_typename(.string));
    try expectEqualStrings("table", lua.luna_typename(.table));
    try expectEqualStrings("function", lua.luna_typename(.function));
    try expectEqualStrings("userdata", lua.luna_typename(.userdata));
    try expectEqualStrings("thread", lua.luna_typename(.thread));
}

test "executing string contents" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();
    lua.lunaL_openlibs();

    try lua.lunaL_loadstring("f = function(x) return x + 10 end");
    try lua.luna_pcall(0, 0, 0);
    try lua.lunaL_loadstring("a = f(2)");
    try lua.luna_pcall(0, 0, 0);

    try expectEqual(LuaType.function, try lua.luna_getglobal("f"));
    lua.luna_pop(1);
    try expectEqual(LuaType.number, try lua.luna_getglobal("a"));
    try expectEqual(@as(i64, 12), try lua.luna_tointeger(1));

    try expectError(error.Syntax, lua.lunaL_loadstring("bad syntax"));
    try lua.lunaL_loadstring("a = g()");
    try expectError(error.Runtime, lua.luna_pcall(0, 0, 0));
}

test "filling and checking the stack" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try expectEqual(@as(i32, 0), lua.luna_gettop());

    // We want to push 30 values onto the stack
    // this should work without fail
    try lua.luna_checkstack(30);

    var count: i32 = 0;
    while (count < 30) : (count += 1) {
        lua.luna_pushnil();
    }

    try expectEqual(@as(i32, 30), lua.luna_gettop());

    // this should fail (beyond max stack size)
    try expectError(error.Fail, lua.luna_checkstack(1_000_000));

    // this is small enough it won't fail (would raise an error if it did)
    lua.lunaL_checkstack(40, null);

    while (count < 40) : (count += 1) {
        lua.luna_pushnil();
    }

    try expectEqual(@as(i32, 40), lua.luna_gettop());
}

test "stack manipulation" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // add some numbers to manipulate
    var num: i32 = 1;
    while (num <= 10) : (num += 1) {
        lua.luna_pushinteger(num);
    }
    try expectEqual(@as(i32, 10), lua.luna_gettop());

    lua.luna_settop(12);
    try expectEqual(@as(i32, 12), lua.luna_gettop());
    try expect(lua.luna_isnil(-1));

    // rotate the two nils to the bottom of the stack
    lua.luna_rotate(1, 2);
    try expect(lua.luna_isnil(1));
    try expect(lua.luna_isnil(2));

    lua.luna_remove(2);
    try expect(lua.luna_isnil(1));
    try expect(lua.luna_isinteger(2));

    lua.luna_insert(1);
    try expect(lua.luna_isinteger(1));
    try expect(lua.luna_isnil(2));

    lua.luna_replace(2);
    try expect(lua.luna_isinteger(2));
    try expectEqual(@as(i32, 10), lua.luna_gettop());

    lua.luna_copy(1, 2);
    try expectEqual(@as(i64, 10), try lua.luna_tointeger(1));
    try expectEqual(@as(i64, 10), try lua.luna_tointeger(2));
    try expectEqual(@as(i64, 1), try lua.luna_tointeger(3));
    try expectEqual(@as(i64, 8), try lua.luna_tointeger(-1));

    lua.luna_settop(0);
    try expectEqual(@as(i32, 0), lua.luna_gettop());
}

test "calling a function" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.luna_register("luna_add", luna.wrap(add));
    _ = try lua.luna_getglobal("luna_add");

    lua.luna_pushinteger(10);
    lua.luna_pushinteger(32);

    // luna_pcall is safer, but we might as well exercise call when
    // we know it should be safe
    lua.luna_call(2, 1);

    try expectEqual(@as(i64, 42), try lua.luna_tointeger(1));
}

test "version" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try expectEqual(@as(f64, 504), lua.luna_version());
    lua.lunaL_checkversion();
}

test "string buffers" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    var buffer: LunaBuffer = undefined;
    buffer.lunaL_buffinit(lua);
    try expectEqual(@as(usize, 0), buffer.lunaL_bufflen());

    buffer.lunaL_addchar('z');
    buffer.lunaL_addchar('i');
    buffer.lunaL_addchar('g');
    buffer.lunaL_addstring("lua");
    try expectEqual(@as(usize, 6), buffer.lunaL_bufflen());

    buffer.lunaL_buffsub(3);
    try expectEqual(@as(usize, 3), buffer.lunaL_bufflen());

    var str = buffer.lunaL_prepbuffsize(3);
    str[0] = 'l';
    str[1] = 'u';
    str[2] = 'a';
    try expectEqual(@as(usize, 3), buffer.lunaL_bufflen());
    buffer.lunaL_addsize(3);
    try expectEqual(@as(usize, 6), buffer.lunaL_bufflen());
    try expectEqualStrings("ziglua", buffer.lunaL_buffaddr());

    buffer.lunaL_addlstring(" api ");
    try expectEqualStrings("ziglua api ", buffer.lunaL_buffaddr());

    lua.luna_pushnumber(5.4);
    buffer.lunaL_addvalue();
    try expectEqual(@as(usize, 14), buffer.lunaL_bufflen());
    try expectEqualStrings("ziglua api 5.4", buffer.lunaL_buffaddr());

    buffer.lunaL_buffsub(4);
    try expectEqualStrings("ziglua api", buffer.lunaL_buffaddr());

    buffer.lunaL_addgsub(" some string here", "string", "text");
    try expectEqualStrings("ziglua api some text here", buffer.lunaL_buffaddr());

    buffer.lunaL_pushresult();
    try expectEqualStrings("ziglua api some text here", try lua.luna_tolstring(-1));

    // now test a small buffer
    buffer = undefined;
    var b = buffer.lunaL_buffinitsize(lua, 3);
    b[0] = 'a';
    b[1] = 'b';
    b[2] = 'c';
    buffer.lunaL_addsize(3);
    b = buffer.lunaL_prepbuffer();
    std.mem.copy(u8, b, "defghijklmnopqrstuvwxyz");
    buffer.lunaL_pushresultsize(23);
    try expectEqualStrings("abcdefghijklmnopqrstuvwxyz", lua.lunaL_tolstring(-1));
    lua.luna_pop(1);

    lua.luna_len(-1);
    try expectEqual(@as(LuaInteger, 26), try lua.luna_tointeger(-1));
}

test "global table" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.lunaL_openlibs();
    lua.luna_pushglobaltable();

    // find the print function
    _ = lua.luna_pushstring("print");
    try expectEqual(LuaType.function, lua.luna_gettable(-2));

    // index the global table in the global table
    try expectEqual(LuaType.table, lua.luna_getfield(-2, "_G"));

    // find pi in the math table
    try expectEqual(LuaType.table, lua.luna_getfield(-1, "math"));
    try expectEqual(LuaType.number, lua.luna_getfield(-1, "pi"));

    // but the string table should be nil
    lua.luna_pop(2);
    try expectEqual(LuaType.nil, lua.luna_getfield(-1, "string"));
}

test "function registration" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // register all functions as part of a table
    const funcs = [_]luna.LuaReg{
        .{ .name = "add", .func = luna.wrap(add) },
        .{ .name = "sub", .func = luna.wrap(sub) },
        .{ .name = "placeholder", .func = null },
    };
    lua.luna_newtable();
    lua.lunaL_setfuncs(&funcs, 0);

    try expectEqual(LuaType.boolean, lua.luna_getfield(-1, "placeholder"));
    lua.luna_pop(1);
    try expectEqual(LuaType.function, lua.luna_getfield(-1, "add"));
    lua.luna_pop(1);
    try expectEqual(LuaType.function, lua.luna_getfield(-1, "sub"));

    // also try calling the sub function sub(42, 40)
    lua.luna_pushinteger(42);
    lua.luna_pushinteger(40);
    try lua.luna_pcall(2, 1, 0);
    try expectEqual(@as(LuaInteger, 2), try lua.luna_tointeger(-1));

    // now test the newlib variation to build a library from functions
    // indirectly tests newLibTable
    lua.lunaL_newlib(&funcs);
    // add functions to the global table under "funcs"
    lua.luna_setglobal("funcs");

    try lua.lunaL_dostring("funcs.add(10, 20)");
    try lua.lunaL_loadstring("funcs.sub('10', 20)");
    try expectError(error.Runtime, lua.lunaL_loadstring("funcs.placeholder()"));
}

test "panic fn" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // just test setting up the panic function
    // it uses longjmp so cannot return here to test
    const panicFn = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            _ = l;
            return 0;
        }
    }.inner);
    try expectEqual(@as(?luna.LuaCFunction, null), lua.luna_atpanic(panicFn));
}

test "warn fn" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.luna_warning("this message is going to the void", false);

    const warnFn = luna.wrap(struct {
        fn inner(data: ?*anyopaque, msg: []const u8, to_cont: bool) void {
            _ = data;
            _ = to_cont;
            if (!std.mem.eql(u8, msg, "this will be caught by the warnFn")) panic("test failed", .{});
        }
    }.inner);

    lua.luna_setwarnf(warnFn, null);
    lua.luna_warning("this will be caught by the warnFn", false);
}

test "concat" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    _ = lua.luna_pushstring("hello ");
    lua.luna_pushnumber(10);
    _ = lua.luna_pushstring(" wow!");
    lua.luna_concat(3);

    try expectEqualStrings("hello 10.0 wow!", try lua.luna_tolstring(-1));
}

test "garbage collector" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // because the garbage collector is an opaque, unmanaged
    // thing, it is hard to test, so just run each function
    lua.luna_gc_stop();
    lua.luna_gc_collect();
    lua.luna_gc_restart();
    lua.luna_gc_step(10);
    _ = lua.luna_gc_count();
    _ = lua.luna_gc_countb();
    _ = lua.luna_gc_is_running();

    try expect(lua.luna_gc_set_generational(0, 10));
    try expect(lua.luna_gc_set_incremental(0, 0, 0));
    try expect(!lua.luna_gc_set_incremental(0, 0, 0));
}

test "extra space" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    var space: *align(1) usize = @ptrCast(lua.luna_getextraspace().ptr);
    space.* = 1024;
    // each new thread is initialized with a copy of the extra space from the main thread
    var thread = lua.luna_newthread();
    try expectEqual(@as(usize, 1024), @as(*align(1) usize, @ptrCast(thread.luna_getextraspace())).*);
}

test "table access" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring("a = { [1] = 'first', key = 'value', ['other one'] = 1234 }");
    _ = try lua.luna_getglobal("a");

    try expectEqual(LuaType.string, lua.luna_geti(1, 1));
    try expectEqualStrings("first", try lua.luna_tolstring(-1));

    try expectEqual(LuaType.string, lua.luna_rawgeti(1, 1));
    try expectEqualStrings("first", try lua.luna_tolstring(-1));

    _ = lua.luna_pushstring("key");
    try expectEqual(LuaType.string, lua.luna_gettable(1));
    try expectEqualStrings("value", try lua.luna_tolstring(-1));

    _ = lua.luna_pushstring("other one");
    try expectEqual(LuaType.number, lua.luna_rawget(1));
    try expectEqual(@as(LuaInteger, 1234), try lua.luna_tointeger(-1));

    // a.name = "ziglua"
    _ = lua.luna_pushstring("name");
    _ = lua.luna_pushstring("ziglua");
    lua.luna_settable(1);

    // a.lang = "zig"
    _ = lua.luna_pushstring("lang");
    _ = lua.luna_pushstring("zig");
    lua.luna_rawset(1);

    try expectError(error.Fail, lua.luna_getmetatable(1));

    // create a metatable (it isn't a useful one)
    lua.luna_newtable();
    lua.luna_pushcfunction(luna.wrap(add));
    lua.luna_setfield(-2, "__len");
    lua.luna_setmetatable(1);

    try lua.luna_getmetatable(1);
    _ = try lua.lunaL_getmetafield(1, "__len");
    try expectError(error.Fail, lua.lunaL_getmetafield(1, "__index"));

    lua.luna_pushboolean(true);
    lua.luna_setfield(1, "bool");

    try lua.lunaL_dostring("b = a.bool");
    try expectEqual(LuaType.boolean, try lua.luna_getglobal("b"));
    try expect(lua.luna_toboolean(-1));

    // create array [1, 2, 3, 4, 5]
    lua.luna_createtable(0, 0);
    var index: LuaInteger = 1;
    while (index <= 5) : (index += 1) {
        lua.luna_pushinteger(index);
        lua.luna_seti(-2, index);
    }
    try expectEqual(@as(luna.LuaUnsigned, 5), lua.luna_rawlen(-1));
    try expectEqual(@as(LuaInteger, 5), lua.lunaL_len(-1));

    // add a few more
    while (index <= 10) : (index += 1) {
        lua.luna_pushinteger(index);
        lua.luna_rawseti(-2, index);
    }
    try expectEqual(@as(luna.LuaUnsigned, 10), lua.luna_rawlen(-1));
}

test "conversions" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // number conversion
    var value: LuaInteger = undefined;
    try lua.luna_numbertointeger(3.14, &value);
    try expectEqual(@as(LuaInteger, 3), value);
    try expectError(error.Fail, lua.luna_numbertointeger(@as(LuaNumber, @floatFromInt(luna.LuaMaxInteger)) + 10, &value));

    // string conversion
    try lua.luna_stringtonumber("1");
    try expect(lua.luna_isinteger(-1));
    try expectEqual(@as(LuaInteger, 1), try lua.luna_tointeger(1));

    try lua.luna_stringtonumber("  1.0  ");
    try expect(lua.luna_isnumber(-1));
    try expectEqual(@as(LuaNumber, 1.0), try lua.luna_tonumber(-1));

    try expectError(error.Fail, lua.luna_stringtonumber("a"));
    try expectError(error.Fail, lua.luna_stringtonumber("1.a"));
    try expectError(error.Fail, lua.luna_stringtonumber(""));

    // index conversion
    try expectEqual(@as(i32, 2), lua.luna_absindex(-1));
    try expectEqual(@as(i32, 1), lua.luna_absindex(-2));
}

test "threads" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    var new_thread = lua.luna_newthread();
    try expectEqual(@as(i32, 1), lua.luna_gettop());
    try expectEqual(@as(i32, 0), new_thread.luna_gettop());

    lua.luna_pushinteger(10);
    lua.luna_pushnil();

    lua.luna_xmove(new_thread, 2);
    try expectEqual(@as(i32, 2), new_thread.luna_gettop());
}

test "userdata and uservalues" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const Data = struct {
        val: i32,
        code: [4]u8,
    };

    // create a Lua-owned pointer to a Data with 2 associated user values
    var data = lua.luna_newuserdata(Data, 2);
    data.val = 1;
    std.mem.copy(u8, &data.code, "abcd");

    // assign the user values
    lua.luna_pushnumber(1234.56);
    try lua.luna_setiuservalue(1, 1);

    _ = lua.luna_pushstring("test string");
    try lua.luna_setiuservalue(1, 2);

    try expectEqual(LuaType.number, try lua.luna_getiuservalue(1, 1));
    try expectEqual(@as(LuaNumber, 1234.56), try lua.luna_tonumber(-1));
    try expectEqual(LuaType.string, try lua.luna_getiuservalue(1, 2));
    try expectEqualStrings("test string", try lua.luna_tolstring(-1));

    try expectError(error.Fail, lua.luna_setiuservalue(1, 3));
    try expectError(error.Fail, lua.luna_getiuservalue(1, 3));

    try expectEqual(data, try lua.luna_touserdata(Data, 1));
    try expectEqual(@as(*const anyopaque, @ptrCast(data)), try lua.luna_topointer(1));
}

test "upvalues" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // counter from PIL
    const counter = struct {
        fn inner(l: *Luna) i32 {
            var counter = l.luna_tointeger(l.luna_upvalueindex(1)) catch 0;
            counter += 1;
            l.luna_pushinteger(counter);
            l.luna_copy(-1, l.luna_upvalueindex(1));
            return 1;
        }
    }.inner;

    // Initialize the counter at 0
    lua.luna_pushinteger(0);
    lua.luna_pushcclosure(luna.wrap(counter), 1);
    lua.luna_setglobal("counter");

    // call the function repeatedly, each time ensuring the result increases by one
    var expected: LuaInteger = 1;
    while (expected <= 10) : (expected += 1) {
        _ = try lua.luna_getglobal("counter");
        lua.luna_call(0, 1);
        try expectEqual(expected, try lua.luna_tointeger(-1));
        lua.luna_pop(1);
    }
}

test "table traversal" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring("t = { key = 'value', second = true, third = 1 }");
    _ = try lua.luna_getglobal("t");

    lua.luna_pushnil();

    while (lua.luna_next(1)) {
        switch (lua.luna_type(-1)) {
            .string => {
                try expectEqualStrings("key", try lua.luna_tolstring(-2));
                try expectEqualStrings("value", try lua.luna_tolstring(-1));
            },
            .boolean => {
                try expectEqualStrings("second", try lua.luna_tolstring(-2));
                try expectEqual(true, lua.luna_toboolean(-1));
            },
            .number => {
                try expectEqualStrings("third", try lua.luna_tolstring(-2));
                try expectEqual(@as(LuaInteger, 1), try lua.luna_tointeger(-1));
            },
            else => unreachable,
        }
        lua.luna_pop(1);
    }
}

test "registry" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const key = "mykey";

    // store a string in the registry
    _ = lua.luna_pushstring("hello there");
    lua.luna_rawsetp(luna.LuaRegistryIndex, key);

    // get key from the registry
    _ = lua.luna_rawgetp(luna.LuaRegistryIndex, key);
    try expectEqualStrings("hello there", try lua.luna_tolstring(-1));
}

test "closing vars" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.lunaL_openlibs();

    // do setup in Lua for ease
    try lua.lunaL_dostring(
        \\closed_vars = 0
        \\mt = { __close = function() closed_vars = closed_vars + 1 end }
    );

    lua.luna_newtable();
    _ = try lua.luna_getglobal("mt");
    lua.luna_setmetatable(-2);
    lua.luna_toclose(-1);
    lua.luna_closeslot(-1);
    lua.luna_pop(1);

    lua.luna_newtable();
    _ = try lua.luna_getglobal("mt");
    lua.luna_setmetatable(-2);
    lua.luna_toclose(-1);
    lua.luna_closeslot(-1);
    lua.luna_pop(1);

    // this should have incremented "closed_vars" to 2
    _ = try lua.luna_getglobal("closed_vars");
    try expectEqual(@as(LuaNumber, 2), try lua.luna_tonumber(-1));
}

test "raise error" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const makeError = struct {
        fn inner(l: *Luna) i32 {
            _ = l.luna_pushstring("makeError made an error");
            l.luna_error();
            return 0;
        }
    }.inner;

    lua.luna_pushcfunction(luna.wrap(makeError));
    try expectError(error.Runtime, lua.luna_pcall(0, 0, 0));
    try expectEqualStrings("makeError made an error", try lua.luna_tolstring(-1));
}

fn continuation(l: *Luna, status: luna.LuaStatus, ctx: isize) i32 {
    _ = status;

    if (ctx == 5) {
        _ = l.luna_pushstring("done");
        return 1;
    } else {
        // yield the current context value
        l.luna_pushinteger(ctx);
        return l.luna_yieldk(1, ctx + 1, luna.wrap(continuation));
    }
}

test "yielding" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    // here we create some zig functions that will run 5 times, continutally
    // yielding a count until it finally returns the string "done"
    const willYield = struct {
        fn inner(l: *Luna) i32 {
            return continuation(l, .ok, 0);
        }
    }.inner;

    var thread = lua.luna_newthread();
    thread.luna_pushcfunction(luna.wrap(willYield));

    try expect(!lua.luna_isyieldable());
    try expect(thread.luna_isyieldable());

    var results: i32 = undefined;
    var i: i32 = 0;
    while (i < 5) : (i += 1) {
        try expectEqual(luna.LuaThreadResumeStatus.yield, try thread.luna_resume(lua, 0, &results));
        try expectEqual(@as(LuaInteger, i), try thread.luna_tointeger(-1));
        thread.luna_pop(results);
    }
    try expectEqual(luna.LuaThreadResumeStatus.ok, try thread.luna_resume(lua, 0, &results));
    try expectEqualStrings("done", try thread.luna_tolstring(-1));
}

test "debug interface" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring(
        \\f = function(x)
        \\  local y = x * 2
        \\  y = y + 2
        \\  return x + y
        \\end
    );
    _ = try lua.luna_getglobal("f");

    var info: LuaDebugInfo = undefined;
    lua.luna_getinfo(.{
        .@">" = true,
        .l = true,
        .S = true,
        .n = true,
        .u = true,
        .t = true,
    }, &info);

    // get information about the function
    try expectEqual(LuaDebugInfo.FnType.lua, info.what);
    try expectEqual(LuaDebugInfo.NameType.other, info.name_what);
    const len = std.mem.len(@as([*:0]u8, @ptrCast(&info.short_src)));
    try expectEqualStrings("[string \"f = function(x)...\"]", info.short_src[0..len]);
    try expectEqual(@as(?i32, 1), info.first_line_defined);
    try expectEqual(@as(?i32, 5), info.last_line_defined);
    try expectEqual(@as(u8, 1), info.num_params);
    try expectEqual(@as(u8, 0), info.num_upvalues);
    try expect(!info.is_tail_call);
    try expectEqual(@as(?i32, null), info.current_line);

    // create a hook
    const hook = struct {
        fn inner(l: *Luna, event: LuaEvent, i: *LuaDebugInfo) void {
            switch (event) {
                .call => {
                    l.luna_getinfo(.{ .l = true, .r = true }, i);
                    if (i.current_line.? != 2) panic("Expected line to be 2", .{});
                    _ = l.luna_getlocal(i, i.first_transfer) catch unreachable;
                    if ((l.luna_tonumber(-1) catch unreachable) != 3) panic("Expected x to equal 3", .{});
                },
                .line => if (i.current_line.? == 4) {
                    // modify the value of y to be 0 right before returning
                    l.luna_pushnumber(0);
                    _ = l.luna_setlocal(i, 2) catch unreachable;
                },
                .ret => {
                    l.luna_getinfo(.{ .l = true, .r = true }, i);
                    if (i.current_line.? != 4) panic("Expected line to be 4", .{});
                    _ = l.luna_getlocal(i, i.first_transfer) catch unreachable;
                    if ((l.luna_tonumber(-1) catch unreachable) != 3) panic("Expected result to equal 3", .{});
                },
                else => unreachable,
            }
        }
    }.inner;

    // run the hook when a function is called
    try expectEqual(@as(?luna.LuaHook, null), lua.luna_gethook());
    try expectEqual(luna.LuaHookMask{}, lua.luna_gethookmask());
    try expectEqual(@as(i32, 0), lua.luna_gethookcount());

    lua.luna_sethook(luna.wrap(hook), .{ .call = true, .line = true, .ret = true }, 0);
    try expectEqual(@as(?luna.LuaHook, luna.wrap(hook)), lua.luna_gethook());
    try expectEqual(luna.LuaHookMask{ .call = true, .line = true, .ret = true }, lua.luna_gethookmask());

    _ = try lua.luna_getglobal("f");
    lua.luna_pushnumber(3);
    try lua.luna_pcall(1, 1, 0);
}

test "debug upvalues" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring(
        \\f = function(x)
        \\  return function(y)
        \\    return x + y
        \\  end
        \\end
        \\addone = f(1)
    );
    _ = try lua.luna_getglobal("addone");

    // index doesn't exist
    try expectError(error.Fail, lua.luna_getupvalue(1, 2));

    // inspect the upvalue (should be x)
    try expectEqualStrings("x", try lua.luna_getupvalue(-1, 1));
    try expectEqual(@as(LuaNumber, 1), try lua.luna_tonumber(-1));
    lua.luna_pop(1);

    // now make the function an "add five" function
    lua.luna_pushnumber(5);
    _ = try lua.luna_setupvalue(-2, 1);

    // test a bad index (the valid one's result is unpredicable)
    try expectError(error.Fail, lua.luna_upvalueid(-1, 2));

    // call the new function (should return 7)
    lua.luna_pushnumber(2);
    try lua.luna_pcall(1, 1, 0);
    try expectEqual(@as(LuaNumber, 7), try lua.luna_tonumber(-1));
    lua.luna_pop(1);

    try lua.lunaL_dostring(
        \\addthree = f(3)
    );

    _ = try lua.luna_getglobal("addone");
    _ = try lua.luna_getglobal("addthree");

    // now addone and addthree share the same upvalue
    lua.luna_upvaluejoin(-2, 1, -1, 1);
    try expect((try lua.luna_upvalueid(-2, 1)) == try lua.luna_upvalueid(-1, 1));
}

test "getstack" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try expectError(error.Fail, lua.luna_getstack(1));

    const function = struct {
        fn inner(l: *Luna) i32 {
            // get info about calling lua function
            var info = l.luna_getstack(1) catch unreachable;
            l.luna_getinfo(.{ .n = true }, &info);
            expectEqualStrings("g", info.name.?) catch unreachable;
            return 0;
        }
    }.inner;

    lua.luna_pushcfunction(luna.wrap(function));
    lua.luna_setglobal("f");

    try lua.lunaL_dostring(
        \\g = function()
        \\  f()
        \\end
        \\g()
    );
}

test "aux check functions" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const function = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_checkany(1);
            _ = l.lunaL_checkinteger(2);
            _ = l.lunaL_checklstring(3);
            _ = l.lunaL_checknumber(4);
            _ = l.lunaL_checkstring(5);
            l.lunaL_checktype(6, .boolean);
            return 0;
        }
    }.inner);

    lua.luna_pushcfunction(function);
    lua.luna_pcall(0, 0, 0) catch {
        try expectEqualStrings("bad argument #1 to '?' (value expected)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    lua.luna_pushnil();
    lua.luna_pcall(1, 0, 0) catch {
        try expectEqualStrings("bad argument #2 to '?' (number expected, got no value)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    lua.luna_pushnil();
    lua.luna_pushinteger(3);
    lua.luna_pcall(2, 0, 0) catch {
        try expectEqualStrings("bad argument #3 to '?' (string expected, got no value)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    lua.luna_pushnil();
    lua.luna_pushinteger(3);
    _ = lua.luna_pushlstring("hello world");
    lua.luna_pcall(3, 0, 0) catch {
        try expectEqualStrings("bad argument #4 to '?' (number expected, got no value)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    lua.luna_pushnil();
    lua.luna_pushinteger(3);
    _ = lua.luna_pushlstring("hello world");
    lua.luna_pushnumber(4);
    lua.luna_pcall(4, 0, 0) catch {
        try expectEqualStrings("bad argument #5 to '?' (string expected, got no value)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    lua.luna_pushnil();
    lua.luna_pushinteger(3);
    _ = lua.luna_pushlstring("hello world");
    lua.luna_pushnumber(4);
    _ = lua.luna_pushstring("hello world");
    lua.luna_pcall(5, 0, 0) catch {
        try expectEqualStrings("bad argument #6 to '?' (boolean expected, got no value)", try lua.luna_tolstring(-1));
        lua.luna_pop(-1);
    };

    lua.luna_pushcfunction(function);
    // test lunaL_pushfail here (currently acts the same as luna_pushnil)
    lua.lunaL_pushfail();
    lua.luna_pushinteger(3);
    _ = lua.luna_pushlstring("hello world");
    lua.luna_pushnumber(4);
    _ = lua.luna_pushstring("hello world");
    lua.luna_pushboolean(true);
    try lua.luna_pcall(6, 0, 0);
}

test "get global fail" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try expectError(error.Fail, lua.luna_getglobal("foo"));
}

test "metatables" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring("f = function() return 10 end");

    try lua.lunaL_newmetatable("mt");
    _ = lua.lunaL_getmetatable("mt");
    try expect(lua.luna_compare(1, 2, .eq));
    lua.luna_pop(1);

    // set the len metamethod to the function f
    _ = try lua.luna_getglobal("f");
    lua.luna_setfield(1, "__len");

    lua.luna_newtable();
    lua.lunaL_setmetatable("mt");

    try lua.lunaL_callmeta(-1, "__len");
    try expectEqual(@as(LuaNumber, 10), try lua.luna_tonumber(-1));
}

test "aux opt functions" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const function = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            expectEqual(@as(LuaInteger, 10), l.lunaL_optinteger(1, 10)) catch unreachable;
            expectEqualStrings("zig", l.lunaL_optlstring(2, "zig")) catch unreachable;
            expectEqual(@as(LuaNumber, 1.23), l.lunaL_optnumber(3, 1.23)) catch unreachable;
            expectEqualStringsSentinel("lang", l.lunaL_optstring(4, "lang")) catch unreachable;
            return 0;
        }
    }.inner);

    lua.luna_pushcfunction(function);
    try lua.luna_pcall(0, 0, 0);

    lua.luna_pushcfunction(function);
    lua.luna_pushinteger(10);
    _ = lua.luna_pushlstring("zig");
    lua.luna_pushnumber(1.23);
    _ = lua.luna_pushstring("lang");
    try lua.luna_pcall(4, 0, 0);
}

test "checkOption" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const Variant = enum {
        one,
        two,
        three,
    };

    const function = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            const option = l.lunaL_checkoption(Variant, 1, .one);
            l.luna_pushinteger(switch (option) {
                .one => 1,
                .two => 2,
                .three => 3,
            });
            return 1;
        }
    }.inner);

    lua.luna_pushcfunction(function);
    _ = lua.luna_pushstring("one");
    try lua.luna_pcall(1, 1, 0);
    try expectEqual(@as(LuaInteger, 1), try lua.luna_tointeger(-1));
    lua.luna_pop(1);

    lua.luna_pushcfunction(function);
    _ = lua.luna_pushstring("two");
    try lua.luna_pcall(1, 1, 0);
    try expectEqual(@as(LuaInteger, 2), try lua.luna_tointeger(-1));
    lua.luna_pop(1);

    lua.luna_pushcfunction(function);
    _ = lua.luna_pushstring("three");
    try lua.luna_pcall(1, 1, 0);
    try expectEqual(@as(LuaInteger, 3), try lua.luna_tointeger(-1));
    lua.luna_pop(1);

    // try the default now
    lua.luna_pushcfunction(function);
    try lua.luna_pcall(0, 1, 0);
    try expectEqual(@as(LuaInteger, 1), try lua.luna_tointeger(-1));
    lua.luna_pop(1);

    // check the raised error
    lua.luna_pushcfunction(function);
    _ = lua.luna_pushstring("unknown");
    try expectError(error.Runtime, lua.luna_pcall(1, 1, 0));
    try expectEqualStrings("bad argument #1 to '?' (invalid option 'unknown')", try lua.luna_tolstring(-1));
}

test "globalSub" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    _ = lua.lunaL_gsub("-gity -!", "-", "zig");
    try expectEqualStrings("ziggity zig!", try lua.luna_tolstring(-1));
}

test "loadBuffer" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    _ = try lua.lunaL_loadbufferx("global = 10", "chunkname", .text);
    try lua.luna_pcall(0, luna.LuaMultRet, 0);
    _ = try lua.luna_getglobal("global");
    try expectEqual(@as(LuaInteger, 10), try lua.luna_tointeger(-1));
}

test "where" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const whereFn = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_where(1);
            return 1;
        }
    }.inner);

    lua.luna_pushcfunction(whereFn);
    lua.luna_setglobal("whereFn");

    try lua.lunaL_dostring(
        \\
        \\ret = whereFn()
    );

    _ = try lua.luna_getglobal("ret");
    try expectEqualStrings("[string \"...\"]:2: ", try lua.luna_tolstring(-1));
}

test "ref" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.luna_pushnil();
    try expectError(error.Fail, lua.lunaL_ref(luna.LuaRegistryIndex));
    try expectEqual(@as(LuaInteger, 0), lua.luna_gettop());

    _ = lua.luna_pushlstring("Hello there");
    const ref = try lua.lunaL_ref(luna.LuaRegistryIndex);

    _ = lua.luna_rawgeti(luna.LuaRegistryIndex, ref);
    try expectEqualStrings("Hello there", try lua.luna_tolstring(-1));

    lua.lunaL_unref(luna.LuaRegistryIndex, ref);
}

test "args and errors" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const argCheck = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_argcheck(false, 1, "error!");
            return 0;
        }
    }.inner);

    lua.luna_pushcfunction(argCheck);
    try expectError(error.Runtime, lua.luna_pcall(0, 0, 0));

    const argExpected = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_argexpected(true, 1, "string");
            return 0;
        }
    }.inner);

    lua.luna_pushcfunction(argExpected);
    try expectError(error.Runtime, lua.luna_pcall(0, 0, 0));

    const raisesError = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_error("some error %s!", .{"zig"});
            unreachable;
        }
    }.inner);

    lua.luna_pushcfunction(raisesError);
    try expectError(error.Runtime, lua.luna_pcall(0, 0, 0));
    try expectEqualStrings("some error zig!", try lua.luna_tolstring(-1));
}

test "traceback" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const tracebackFn = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            l.lunaL_traceback(l, "", 1);
            return 1;
        }
    }.inner);

    lua.luna_pushcfunction(tracebackFn);
    lua.luna_setglobal("tracebackFn");
    try lua.lunaL_dostring("res = tracebackFn()");

    _ = try lua.luna_getglobal("res");
    try expectEqualStrings("\nstack traceback:\n\t[string \"res = tracebackFn()\"]:1: in main chunk", try lua.luna_tolstring(-1));
}

test "getSubtable" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_dostring(
        \\a = {
        \\  b = {},
        \\}
    );
    _ = try lua.luna_getglobal("a");

    // get the subtable a.b
    try lua.lunaL_getsubtable(-1, "b");

    // fail to get the subtable a.c (but it is created)
    try expectError(error.Fail, lua.lunaL_getsubtable(-2, "c"));

    // now a.c will pass
    try lua.lunaL_getsubtable(-3, "b");
}

test "userdata" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    const Type = struct { a: i32, b: f32 };
    try lua.lunaL_newmetatable("Type");

    var t = lua.luna_newuserdata(Type, 0);
    lua.lunaL_setmetatable("Type");
    t.a = 1234;
    t.b = 3.14;

    const checkUdata = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            const ptr = l.lunaL_checkudata(Type, 1, "Type");
            if (ptr.a != 1234) {
                _ = l.luna_pushlstring("error!");
                l.luna_error();
            }
            if (ptr.b != 3.14) {
                _ = l.luna_pushlstring("error!");
                l.luna_error();
            }
            return 1;
        }
    }.inner);

    lua.luna_pushcfunction(checkUdata);
    lua.luna_rotate(-2, 1);

    // call checkUdata asserting that the udata passed in with the
    // correct metatable and values
    try lua.luna_pcall(1, 1, 0);

    const testUdata = luna.wrap(struct {
        fn inner(l: *Luna) i32 {
            const ptr = l.lunaL_testudata(Type, 1, "Type") catch {
                _ = l.luna_pushlstring("error!");
                l.luna_error();
            };
            if (ptr.a != 1234) {
                _ = l.luna_pushlstring("error!");
                l.luna_error();
            }
            if (ptr.b != 3.14) {
                _ = l.luna_pushlstring("error!");
                l.luna_error();
            }
            return 0;
        }
    }.inner);

    lua.luna_pushcfunction(testUdata);
    lua.luna_rotate(-2, 1);

    // call checkUdata asserting that the udata passed in with the
    // correct metatable and values
    try lua.luna_pcall(1, 0, 0);
}

test "userdatauv" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    try lua.lunaL_newmetatable("FixedArray");

    // create an array of 10
    const slice = lua.luna_newuserdatauv(LuaInteger, 10, 0);
    lua.lunaL_setmetatable("FixedArray");
    for (slice, 1..) |*item, index| {
        item.* = @intCast(index);
    }

    const udataFn = struct {
        fn inner(l: *Luna) i32 {
            _ = l.lunaL_checkudatauv(LuaInteger, 1, "FixedArray");
            _ = l.lunaL_testudatauv(LuaInteger, 1, "FixedArray") catch unreachable;
            const arr = l.luna_touserdatauv(LuaInteger, 1) catch unreachable;
            for (arr, 1..) |item, index| {
                if (item != index) l.lunaL_error("something broke!", .{});
            }

            return 0;
        }
    }.inner;

    lua.luna_pushcfunction(luna.wrap(udataFn));
    lua.luna_rotate(-2, 1);

    try lua.luna_pcall(1, 0, 0);
}

test "skynet" {
    var lua = try Luna.init(testing.allocator);
    defer lua.deinit();

    lua.lunaL_initcodecache();

    lua.luna_open_cache();

    lua.luna_open_clonefunc();
}
