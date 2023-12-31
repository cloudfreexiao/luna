const std = @import("std");

const c = @cImport({
    @cInclude("minilua.h");
});

/// The type of the opaque structure that points to a thread and the state of a Lua interpreter
pub const LuaState = c.lua_State;
/// The type of integers in Lua. By default this is `i64`.
pub const LuaInteger = c.lua_Integer;
/// Type of floats in Lua (typically an f64)
pub const LuaNumber = c.lua_Number;
/// The unsigned version of Integer
pub const LuaUnsigned = c.lua_Unsigned;

/// The maximum integer value that `Integer` can store
pub const LuaMaxInteger = c.LUA_MAXINTEGER;

/// The minimum integer value that `Integer` can store
pub const LuaMinInteger = c.LUA_MININTEGER;

/// Index of the regsitry in the stack (pseudo-index)
pub const LuaRegistryIndex = c.LUA_REGISTRYINDEX;

/// Index of globals in the registry
pub const LuaRidxGlobals = c.LUA_RIDX_GLOBALS;

/// Index of the main thread in the registry
pub const LuaRidxMainThread = c.LUA_RIDX_MAINTHREAD;

/// Option for multiple returns in `Lua.protectedCall()` and `Lua.call()`
pub const LuaMultRet = c.LUA_MULTRET;

// Lua constants and types are declared below in alphabetical order
// For constants that have a logical grouping (like Operators), Zig enums are used for type safety

/// The type of function that Lua uses for all internal allocations and frees
/// `data` is an opaque pointer to any data (the allocator), `ptr` is a pointer to the block being alloced/realloced/freed
/// `osize` is the original size or a code, and `nsize` is the new size
///
/// See https://www.lua.org/manual/5.4/manual.html#lua_Alloc for more details
pub const LuaAlloc = *const fn (data: ?*anyopaque, ptr: ?*anyopaque, osize: usize, nsize: usize) callconv(.C) ?*anyopaque;

/// Type for C functions
/// See https://www.lua.org/manual/5.4/manual.html#lua_CFunction for the protocol
pub const LuaCFunction = *const fn (L: ?*LuaState) callconv(.C) c_int;

/// Type for continuation-function contexts (usually isize)
pub const LuaKContext = isize;

/// Type for continuation functions
pub const LuaKFunction = *const fn (state: ?*LuaState, status: c_int, ctx: LuaKContext) callconv(.C) c_int;

/// Type for functions that read/write blocks when loading/dumping Lua chunks
pub const LuaReader = *const fn (state: ?*LuaState, data: ?*anyopaque, size: [*c]usize) callconv(.C) [*c]const u8;
/// The type of the writer function used by `Lua.dump()`
pub const LuaWriter = *const fn (state: ?*LuaState, buf: ?*const anyopaque, size: usize, data: ?*anyopaque) callconv(.C) c_int;
/// The type of warning functions used by Lua to emit warnings
pub const LuaWarnFunction = *const fn (data: ?*anyopaque, msg: [*c]const u8, to_cont: c_int) callconv(.C) void;

/// Type for arrays of functions to be registered
pub const LuaReg = struct {
    name: [:0]const u8,
    func: ?LuaCFunction,
};

/// Lua types
/// Must be a signed integer because LuaType.none is -1
pub const LuaType = enum(i5) {
    none = c.LUA_TNONE,
    nil = c.LUA_TNIL,
    boolean = c.LUA_TBOOLEAN,
    light_userdata = c.LUA_TLIGHTUSERDATA,
    number = c.LUA_TNUMBER,
    string = c.LUA_TSTRING,
    table = c.LUA_TTABLE,
    function = c.LUA_TFUNCTION,
    userdata = c.LUA_TUSERDATA,
    thread = c.LUA_TTHREAD,
};

// Only used in loadFileX, so no need to group with Status
pub const lua_errfile = c.LUA_ERRFILE;

/// the status of a thread.
const LuaThreadStatus = struct {
    pub const ok = c.LUA_OK;
    pub const yield = c.LUA_YIELD;
    pub const err_runtime = c.LUA_ERRRUN;
    pub const err_syntax = c.LUA_ERRSYNTAX;
    pub const err_memory = c.LUA_ERRMEM;
    pub const err_error = c.LUA_ERRERR;
};

/// The possible status of a call to `Lua.resumeThread`
pub const LuaThreadResumeStatus = enum(u1) {
    ok = LuaThreadStatus.ok,
    yield = LuaThreadStatus.yield,
};

/// Status that a thread can be in
/// Usually errors are reported by a Zig error rather than a status enum value
pub const LuaStatus = enum(u3) {
    ok = LuaThreadStatus.ok,
    yield = LuaThreadStatus.yield,
    err_runtime = LuaThreadStatus.err_runtime,
    err_syntax = LuaThreadStatus.err_syntax,
    err_memory = LuaThreadStatus.err_memory,
    err_error = LuaThreadStatus.err_error,
};

/// Operations supported by `Lua.arith()`
pub const LuaArithOperator = enum(u4) {
    add = c.LUA_OPADD,
    sub = c.LUA_OPSUB,
    mul = c.LUA_OPMUL,
    div = c.LUA_OPDIV,
    int_div = c.LUA_OPIDIV,
    mod = c.LUA_OPMOD,
    pow = c.LUA_OPPOW,
    negate = c.LUA_OPUNM,
    bnot = c.LUA_OPBNOT,
    band = c.LUA_OPBAND,
    bor = c.LUA_OPBOR,
    bxor = c.LUA_OPBXOR,
    shl = c.LUA_OPSHL,
    shr = c.LUA_OPSHR,
};

/// Operations supported by `Lua.compare()`
pub const LuaCompareOperator = enum(u2) {
    eq = c.LUA_OPEQ,
    lt = c.LUA_OPLT,
    le = c.LUA_OPLE,
};

/// The internal Lua debug structure
const LuaDebug = c.lua_Debug;

/// The Lua debug interface structure
pub const LuaDebugInfo = struct {
    source: [:0]const u8 = undefined,
    src_len: usize = 0,
    short_src: [c.LUA_IDSIZE:0]u8 = undefined,

    name: ?[:0]const u8 = undefined,
    name_what: NameType = undefined,
    what: FnType = undefined,

    current_line: ?i32 = null,
    first_line_defined: ?i32 = null,
    last_line_defined: ?i32 = null,

    num_upvalues: u8 = 0,
    num_params: u8 = 0,

    is_vararg: bool = false,
    is_tail_call: bool = false,

    first_transfer: u16 = 0,
    num_transfer: u16 = 0,

    private: *anyopaque = undefined,

    pub const NameType = enum { global, local, method, field, upvalue, other };

    pub const FnType = enum { lua, c, main };

    pub const Options = packed struct {
        @">": bool = false,
        f: bool = false,
        l: bool = false,
        n: bool = false,
        r: bool = false,
        S: bool = false,
        t: bool = false,
        u: bool = false,
        L: bool = false,

        fn toString(options: Options) [10:0]u8 {
            var str = [_:0]u8{0} ** 10;
            var index: u8 = 0;

            inline for (std.meta.fields(Options)) |field| {
                if (@field(options, field.name)) {
                    str[index] = field.name[0];
                    index += 1;
                }
            }
            while (index < str.len) : (index += 1) str[index] = 0;

            return str;
        }
    };
};

/// Event masks
pub const mask_call = c.LUA_MASKCALL;
pub const mask_count = c.LUA_MASKCOUNT;
pub const mask_line = c.LUA_MASKLINE;
pub const mask_ret = c.LUA_MASKRET;
/// Reference constants
pub const ref_nil = c.LUA_REFNIL;
pub const ref_no = c.LUA_NOREF;

/// Type for debugging hook functions
/// https://www.lua.org/manual/5.4/manual.html#lua_Hook
pub const LuaHook = *const fn (L: ?*LuaState, ar: ?*LuaDebug) callconv(.C) void;

/// Specifies on which events the hook will be called
pub const LuaHookMask = packed struct {
    call: bool = false,
    ret: bool = false,
    line: bool = false,
    count: bool = false,

    /// Converts a HookMask to an integer bitmask
    pub fn toInt(mask: LuaHookMask) i32 {
        var bitmask: i32 = 0;
        if (mask.call) bitmask |= mask_call;
        if (mask.ret) bitmask |= mask_ret;
        if (mask.line) bitmask |= mask_line;
        if (mask.count) bitmask |= mask_count;
        return bitmask;
    }

    /// Converts an integer bitmask into a HookMask
    pub fn fromInt(mask: i32) LuaHookMask {
        return .{
            .call = (mask & mask_call) != 0,
            .ret = (mask & mask_ret) != 0,
            .line = (mask & mask_line) != 0,
            .count = (mask & mask_count) != 0,
        };
    }
};

/// The type of event that triggers a hook
pub const LuaEvent = enum(u3) {
    call = c.LUA_HOOKCALL,
    ret = c.LUA_HOOKRET,
    line = c.LUA_HOOKLINE,
    count = c.LUA_HOOKCOUNT,
    tail_call = c.LUA_HOOKTAILCALL,
};

/// Modes used for `Lua.load()`
pub const LuaMode = enum(u2) { binary, text, binary_text };

/// The superset of all errors returned from ziglua
pub const LunaError = error{
    /// A generic failure (used when a function can only fail in one way)
    Fail,
    /// A runtime error
    Runtime,
    /// A syntax error during precompilation
    Syntax,
    /// A memory allocation error
    Memory,
    /// An error while running the message handler
    MsgHandler,
    /// A file-releated error
    File,
};

// Helper functions to make the ziglua API easier to use

/// Casts the opaque pointer to a pointer of the given type with the proper alignment
/// Useful for casting pointers from the Lua API like userdata or other data
inline fn opaqueCast(comptime T: type, ptr: *anyopaque) *T {
    return @ptrCast(@alignCast(ptr));
}

pub const Luna = struct {
    allocator: ?*std.mem.Allocator = null,
    state: *LuaState,

    const Self = @This();
    const alignment = @alignOf(std.c.max_align_t);

    /// Allows Lua to allocate memory using a Zig allocator passed in via data.
    /// See https://www.lua.org/manual/5.4/manual.html#lua_Alloc for more details
    fn alloc(data: ?*anyopaque, ptr: ?*anyopaque, osize: usize, nsize: usize) callconv(.C) ?*align(alignment) anyopaque {
        // just like malloc() returns a pointer "which is suitably aligned for any built-in type",
        // the memory allocated by this function should also be aligned for any type that Lua may
        // desire to allocate. use the largest alignment for the target
        const allocator = opaqueCast(std.mem.Allocator, data.?);

        if (@as(?[*]align(alignment) u8, @ptrCast(@alignCast(ptr)))) |prev_ptr| {
            const prev_slice = prev_ptr[0..osize];

            // when nsize is zero the allocator must behave like free and return null
            if (nsize == 0) {
                allocator.free(prev_slice);
                return null;
            }

            // when nsize is not zero the allocator must behave like realloc
            const new_ptr = allocator.realloc(prev_slice, nsize) catch return null;
            return new_ptr.ptr;
        } else if (nsize == 0) {
            return null;
        } else {
            // ptr is null, allocate a new block of memory
            const new_ptr = allocator.alignedAlloc(u8, alignment, nsize) catch return null;
            return new_ptr.ptr;
        }
    }

    /// Initialize a Lua state with the given allocator
    pub fn init(allocator: std.mem.Allocator) !Self {
        // the userdata passed to alloc needs to be a pointer with a consistent address
        // so we allocate an Allocator struct to hold a copy of the allocator's data
        const allocator_ptr = allocator.create(std.mem.Allocator) catch return error.Memory;
        allocator_ptr.* = allocator;

        const state = c.lua_newstate(alloc, allocator_ptr) orelse return error.Memory;

        return .{
            .allocator = allocator_ptr,
            .state = state,
        };
    }

    /// Deinitialize a Lua state and free all memory
    pub fn deinit(self: *Self) void {
        self.luna_close();
        if (self.allocator) |a| {
            const allocator = a;
            allocator.destroy(a);
            self.allocator = null;
        }
    }

    /// Creates a new independent state and returns its main thread
    /// See https://www.lua.org/manual/5.4/manual.html#lua_newstate
    pub fn luna_newstate(alloc_fn: LuaAlloc, data: ?*anyopaque) !Self {
        const state = c.lua_newstate(alloc_fn, data) orelse return error.Memory;
        return .{ .state = state };
    }

    /// Release all Lua objects in the state and free all dynamic memory
    /// See https://www.lua.org/manual/5.4/manual.html#lua_close
    pub fn luna_close(self: *Self) void {
        c.lua_close(self.state);
    }

    /// Close the to-be-closed slot at the given index and set the value to nil
    /// The index must be the last index previously marked to be closed with toClose
    /// See https://www.lua.org/manual/5.4/manual.html#lua_closeslot
    pub fn luna_closeslot(self: *Self, index: i32) void {
        c.lua_closeslot(self.state, index);
    }

    /// Returns the acceptable index index converted into an equivalent absolute index
    /// See https://www.lua.org/manual/5.4/manual.html#lua_absindex
    pub fn luna_absindex(self: *Self, index: i32) i32 {
        return c.lua_absindex(self.state, index);
    }

    /// Performs an arithmetic or bitwise operation over the value(s) at the top of the stack,
    /// with the value at the top being the second operand. Pushes the result of the operation.
    /// This function follows the semantics of the corresponding Lua operator and may call metamethods
    /// See https://www.lua.org/manual/5.4/manual.html#lua_arith
    pub fn luna_arith(self: *Self, op: LuaArithOperator) void {
        c.lua_arith(self.state, @intFromEnum(op));
    }

    /// Sets a new panic function and returns the old one
    /// See https://www.lua.org/manual/5.4/manual.html#lua_atpanic
    pub fn luna_atpanic(self: *Self, panic_fn: LuaCFunction) ?LuaCFunction {
        return c.lua_atpanic(self.state, panic_fn);
    }

    /// Calls a function (or any callable value)
    /// First push the function to be called onto the stack. Then push any arguments onto the stack.
    /// Then call this function. All arguments and the function value are popped, and any results
    /// are pushed onto the stack.
    /// See https://www.lua.org/manual/5.4/manual.html#lua_call
    pub fn luna_call(self: *Self, nargs: i32, nresults: i32) void {
        c.lua_callk(self.state, nargs, nresults, 0, null);
    }

    /// Ensures that the stack has space for at least n extra arguments
    /// Returns an error if more stack space cannot be allocated
    /// Never shrinks the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_checkstack
    pub fn luna_checkstack(self: *Self, n: i32) !void {
        if (c.lua_checkstack(self.state, n) == 0) return error.Fail;
    }

    /// Concatenates the n values at the top of the stack, pops them, and leaves the result at the top
    /// If the number of values is 1, the result is a single value on the stack (nothing changes)
    /// If the number of values is 0, the result is the empty string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_concat
    pub fn luna_concat(self: *Self, n: i32) void {
        c.lua_concat(self.state, n);
    }

    /// Copies the element at fromidx to the valid index toidx, replacing the value at that position
    /// See https://www.lua.org/manual/5.4/manual.html#lua_copy
    pub fn copy(self: *Self, fromidx: i32, toidx: i32) void {
        c.lua_copy(self.state, fromidx, toidx);
    }

    /// Raises a Lua error using the value at the top of the stack as the error object
    /// Does a longjump and therefore never returns
    /// See https://www.lua.org/manual/5.4/manual.html#lua_error
    pub fn luna_error(self: *Self) noreturn {
        _ = c.lua_error(self.state);
        unreachable;
    }

    /// Stops the garbage collector
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_stop(self: *Self) void {
        _ = c.lua_gc(self.state, c.LUA_GCSTOP);
    }

    /// Restarts the garbage collector
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_restart(self: *Self) void {
        _ = c.lua_gc(self.state, c.LUA_GCRESTART);
    }

    /// Performs an incremental step of garbage collection corresponding to the allocation of step_size Kbytes
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_step(self: *Self, step_size: i32) void {
        _ = c.lua_gc(self.state, c.LUA_GCSTEP, step_size);
    }

    /// Changes the collector to generational mode
    /// Returns true if the previous mode was incremental
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_set_generational(self: *Self, minor_mul: i32, major_mul: i32) bool {
        return c.lua_gc(self.state, c.LUA_GCGEN, minor_mul, major_mul) == c.LUA_GCINC;
    }

    /// Changes the collector to incremental mode
    /// Returns true if the previous mode was generational
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_set_incremental(self: *Self, pause: i32, step_mul: i32, step_size: i32) bool {
        return c.lua_gc(self.state, c.LUA_GCINC, pause, step_mul, step_size) == c.LUA_GCGEN;
    }

    /// Returns a boolean that tells whether the garbage collector is running
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_is_running(self: *Self) bool {
        return c.lua_gc(self.state, c.LUA_GCISRUNNING) != 0;
    }

    /// Returns the current amount of memory (in Kbytes) in use by Lua
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_count(self: *Self) i32 {
        return c.lua_gc(self.state, c.LUA_GCCOUNT);
    }

    /// Returns the current amount of memory (in Kbytes) in use by Lua
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_countb(self: *Self) i32 {
        return c.lua_gc(self.state, c.LUA_GCCOUNTB);
    }

    /// Returns the current amount of memory (in Kbytes) in use by Lua
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gc
    pub fn luna_gc_collect(self: *Self) void {
        _ = c.lua_gc(self.state, c.LUA_GCCOLLECT);
    }

    /// Returns the memory allocation function of a given state
    /// If data is not null, it is set to the opaque pointer given when the allocator function was set
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getallocf
    pub fn luna_getallocf(self: *Self, data: ?**anyopaque) LuaAlloc {
        // Assert cannot be null because it is impossible (and not useful) to pass null
        // to the functions that set the allocator (setallocf and newstate)
        return c.lua_getallocf(self.state, @ptrCast(data)).?;
    }

    /// Returns a slice of a raw memory area associated with the given Lua state
    /// The application may use this area for any purpose; Lua does not use it for anything
    /// This area has a size of a pointer to void
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getextraspace
    pub fn luna_getextraspace(self: *Self) []u8 {
        return @as([*]u8, @ptrCast(c.lua_getextraspace(self.state).?))[0..@sizeOf(isize)];
    }

    /// Pushes onto the stack the value t[key] where t is the value at the given index
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getfield
    pub fn luna_getfield(self: *Self, index: i32, key: [:0]const u8) LuaType {
        return @enumFromInt(c.lua_getfield(self.state, index, key.ptr));
    }

    /// Pushes onto the stack the value of the global name and returns the type of that value
    /// Returns an error if the global does not exist (is nil)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getglobal
    pub fn luna_getglobal(self: *Self, name: [:0]const u8) !LuaType {
        const lua_type: LuaType = @enumFromInt(c.lua_getglobal(self.state, name.ptr));
        if (lua_type == .nil) return error.Fail;
        return lua_type;
    }

    /// Pushes onto the stack the value t[i] where t is the value at the given index
    /// Returns the type of the pushed value
    /// See https://www.lua.org/manual/5.4/manual.html#lua_geti
    pub fn luna_geti(self: *Self, index: i32, i: LuaInteger) LuaType {
        return @enumFromInt(c.lua_geti(self.state, index, i));
    }

    /// Pushes onto the stack the nth user value associated with the full userdata at the given index
    /// Returns the type of the pushed value or an error if the userdata does not have that value
    /// Pushes nil if the userdata does not have that value
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getiuservalue
    pub fn luna_getiuservalue(self: *Self, index: i32, n: i32) !LuaType {
        const val_type: LuaType = @enumFromInt(c.lua_getiuservalue(self.state, index, n));
        if (val_type == .none) return error.Fail;
        return val_type;
    }

    /// If the value at the given index has a metatable, the function pushes that metatable onto the stack
    /// Otherwise an error is returned
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getmetatable
    pub fn luna_getmetatable(self: *Self, index: i32) !void {
        if (c.lua_getmetatable(self.state, index) == 0) return error.Fail;
    }

    /// Pushes onto the stack the value t[k] where t is the value at the given index and k is the value on the top of the stack
    /// Returns the type of the pushed value
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gettable
    pub fn luna_gettable(self: *Self, index: i32) LuaType {
        return @enumFromInt(c.lua_gettable(self.state, index));
    }

    /// Returns the index of the top element in the stack
    /// Because indices start at 1, the result is also equal to the number of elements in the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gettop
    pub fn luna_gettop(self: *Self) i32 {
        return c.lua_gettop(self.state);
    }

    /// Moves the top element into the given valid `index` shifting up any elements to make room
    /// See https://www.lua.org/manual/5.4/manual.html#lua_insert
    pub fn luna_insert(self: *Self, index: i32) void {
        self.luna_rotate(index, 1);
    }

    /// Rotates the stack elements between the valid `index` and the top of the stack
    /// The elements are rotated `n` positions in the direction of the top for positive `n`,
    /// and `n` positions in the direction of the bottom for negative `n`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rotate
    pub fn luna_rotate(self: *Self, index: i32, n: i32) void {
        c.lua_rotate(self.state, index, n);
    }

    /// Returns true if the value at the given index is a boolean
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isboolean
    pub fn luna_isboolean(self: *Self, index: i32) bool {
        return c.lua_isboolean(self.state, index);
    }

    /// Returns true if the value at the given index is a CFn
    /// See https://www.lua.org/manual/5.4/manual.html#lua_iscfunction
    pub fn luna_iscfunction(self: *Self, index: i32) bool {
        return c.lua_iscfunction(self.state, index) != 0;
    }

    /// See https://www.lua.org/manual/5.4/manual.html#lua_isfunction
    pub fn luna_isfunction(self: *Self, index: i32) bool {
        return c.lua_isfunction(self.state, index);
    }

    /// Returns true if the value at the given index is an integer
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isinteger
    pub fn luna_isinteger(self: *Self, index: i32) bool {
        return c.lua_isinteger(self.state, index) != 0;
    }

    /// Returns true if the value at the given index is a light userdata
    /// See https://www.lua.org/manual/5.4/manual.html#lua_islightuserdata
    pub fn luna_islightuserdata(self: *Self, index: i32) bool {
        return c.lua_islightuserdata(self.state, index);
    }

    /// Returns true if the value at the given index is nil
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isnil
    pub fn luna_isnil(self: *Self, index: i32) bool {
        return c.lua_isnil(self.state, index);
    }

    /// Returns true if the given index is not valid
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isnone
    pub fn luna_isnone(self: *Self, index: i32) bool {
        return c.lua_isnone(self.state, index);
    }

    /// Returns true if the given index is not valid or if the value at the index is nil
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isnoneornil
    pub fn luna_isnoneornil(self: *Self, index: i32) bool {
        return c.lua_isnoneornil(self.state, index);
    }

    /// Returns true if the value at the given index is a number
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isnumber
    pub fn luna_isnumber(self: *Self, index: i32) bool {
        return c.lua_isnumber(self.state, index) != 0;
    }

    /// Returns true if the value at the given index is a string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isstring
    pub fn luna_isstring(self: *Self, index: i32) bool {
        return c.lua_isstring(self.state, index) != 0;
    }

    /// Returns true if the value at the given index is a table
    /// See https://www.lua.org/manual/5.4/manual.html#lua_istable
    pub fn luna_istable(self: *Self, index: i32) bool {
        return c.lua_istable(self.state, index);
    }

    /// Returns true if the value at the given index is a thread
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isthread
    pub fn luna_isthread(self: *Self, index: i32) bool {
        return c.lua_isthread(self.state, index);
    }

    /// Returns true if the value at the given index is a userdata (full or light)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isuserdata
    pub fn luna_isuserdata(self: *Self, index: i32) bool {
        return c.lua_isuserdata(self.state, index) != 0;
    }

    /// Returns true if the given coroutine can yield
    /// See https://www.lua.org/manual/5.4/manual.html#lua_isyieldable
    pub fn luna_isyieldable(self: *Self) bool {
        return c.lua_isyieldable(self.state) != 0;
    }

    /// Pushes the length of the value at the given index onto the stack
    /// Equivalent to the # operator in Lua
    /// See https://www.lua.org/manual/5.4/manual.html#lua_len
    pub fn luna_len(self: *Self, index: i32) void {
        c.lua_len(self.state, index);
    }

    /// Creates a new empty table and pushes onto the stack
    /// narr is a hint for how many elements the table will have as a sequence
    /// nrec is a hint for how many other elements the table will have
    /// Lua may preallocate memory for the table based on the hints
    /// See https://www.lua.org/manual/5.4/manual.html#lua_createtable
    pub fn luna_createtable(self: *Self, narr: i32, nrec: i32) void {
        c.lua_createtable(self.state, narr, nrec);
    }

    /// Creates a new empty table and pushes it onto the stack
    /// Equivalent to createTable(0, 0)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_newtable
    pub fn luna_newtable(self: *Self) void {
        c.lua_newtable(self.state);
    }

    /// Creates a new thread, pushes it on the stack, and returns a Lua state that represents the new thread
    /// The new thread shares the global environment but has a separate execution stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_newthread
    pub fn luna_newthread(self: *Self) Self {
        const state = c.lua_newthread(self.state).?;
        return .{ .state = state };
    }

    /// This function allocates a new userdata of the given type with user_values associated Lua values.
    /// Returns a pointer to the Lua-owned data
    /// See https://www.lua.org/manual/5.4/manual.html#lua_newuserdatauv
    pub fn luna_newuserdata(self: *Self, comptime T: type, user_values: i32) *T {
        // safe to .? because this function throws a Lua error on out of memory
        // so the returned pointer should never be null
        const ptr = c.lua_newuserdatauv(self.state, @sizeOf(T), user_values).?;
        return opaqueCast(T, ptr);
    }

    /// This function allocates a new userdata of the given type with user_values associated Lua values.
    /// Returns a pointer to the Lua-owned data
    /// See https://www.lua.org/manual/5.4/manual.html#lua_newuserdatauv
    pub fn luna_newuserdatauv(self: *Self, comptime T: type, size: usize, user_values: i32) []T {
        // safe to .? because this function throws a Lua error on out of memory
        const ptr = c.lua_newuserdatauv(self.state, @sizeOf(T) * size, user_values).?;
        return @as([*]T, @ptrCast(@alignCast(ptr)))[0..size];
    }

    /// Pops a key from the stack, and pushes a key-value pair from the table at the given index
    /// See https://www.lua.org/manual/5.4/manual.html#lua_next
    pub fn luna_next(self: *Self, index: i32) bool {
        return c.lua_next(self.state, index) != 0;
    }

    /// Tries to convert a Lua float into a Lua integer
    /// Returns an error if the conversion was unsuccessful
    /// See https://www.lua.org/manual/5.4/manual.html#lua_numbertointeger
    pub fn luna_numbertointeger(self: *Self, n: LuaNumber, i: *LuaInteger) !void {
        _ = self;
        const min_float: LuaNumber = @floatFromInt(LuaMinInteger);
        if (n >= min_float and n < -min_float) {
            i.* = @intFromFloat(n);
        } else return error.Fail;
    }

    /// Calls a function (or callable object) in protected mode
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pcall
    pub fn luna_pcall(self: *Self, num_args: i32, num_results: i32, msg_handler: i32) !void {
        // The translate-c version of lua_pcall does not type-check so we must rewrite it
        // (macros don't always translate well with translate-c)
        const ret = c.lua_pcallk(self.state, num_args, num_results, msg_handler, 0, null);
        switch (ret) {
            LuaThreadStatus.ok => return,
            LuaThreadStatus.err_runtime => return error.Runtime,
            LuaThreadStatus.err_memory => return error.Memory,
            LuaThreadStatus.err_error => return error.MsgHandler,
            else => unreachable,
        }
    }

    /// Pops `n` elements from the top of the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pop
    pub fn luna_pop(self: *Self, n: i32) void {
        self.luna_settop(-n - 1);
    }

    /// Sets the top of the stack to `index`
    /// If the new top is greater than the old, new elements are filled with nil
    /// If `index` is 0 all stack elements are removed
    /// See https://www.lua.org/manual/5.4/manual.html#lua_settop
    pub fn luna_settop(self: *Self, index: i32) void {
        c.lua_settop(self.state, index);
    }

    /// Pushes a boolean value with value `b` onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushboolean
    pub fn luna_pushboolean(self: *Self, b: bool) void {
        c.lua_pushboolean(self.state, @intFromBool(b));
    }

    /// Pushes a function onto the stack.
    /// Equivalent to pushClosure with no upvalues
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushcfunction
    pub fn luna_pushcfunction(self: *Self, c_fn: LuaCFunction) void {
        self.luna_pushcclosure(c_fn, 0);
    }

    /// Pushes a new Closure onto the stack
    /// `n` tells how many upvalues this function will have
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushcclosure
    pub fn luna_pushcclosure(self: *Self, c_fn: LuaCFunction, n: i32) void {
        c.lua_pushcclosure(self.state, c_fn, n);
    }

    /// Push a formatted string onto the stack and return a pointer to the string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushfstring
    pub fn luna_pushfstring(self: *Self, fmt: [:0]const u8, args: anytype) [*:0]const u8 {
        return @call(.auto, c.lua_pushfstring, .{ self.state, fmt.ptr } ++ args);
    }

    /// Pushes the global environment onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushglobaltable
    pub fn luna_pushglobaltable(self: *Self) void {
        _ = self.luna_rawgeti(LuaRegistryIndex, LuaRidxGlobals);
    }

    // Pushes an integer with value `n` onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushinteger
    pub fn luna_pushinteger(self: *Self, n: LuaInteger) void {
        c.lua_pushinteger(self.state, n);
    }

    /// Pushes a light userdata onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushlightuserdata
    pub fn luna_pushlightuserdata(self: *Self, ptr: *anyopaque) void {
        c.lua_pushlightuserdata(self.state, ptr);
    }

    /// Pushes the bytes onto the stack. Returns a slice pointing to Lua's internal copy of the string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushlstring
    pub fn luna_pushlstring(self: *Self, bytes: []const u8) []const u8 {
        return c.lua_pushlstring(self.state, bytes.ptr, bytes.len)[0..bytes.len];
    }

    /// Pushes a nil value onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushnil
    pub fn luna_pushnil(self: *Self) void {
        c.lua_pushnil(self.state);
    }

    /// Pushes a float with value `n` onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushnumber
    pub fn luna_pushnumber(self: *Self, n: LuaNumber) void {
        c.lua_pushnumber(self.state, n);
    }

    /// Pushes a zero-terminated string onto the stack
    /// Lua makes a copy of the string so `str` may be freed immediately after return
    /// Returns a pointer to the internal Lua string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushstring
    pub fn luna_pushstring(self: *Self, str: [:0]const u8) [:0]const u8 {
        return c.lua_pushstring(self.state, str.ptr)[0..str.len :0];
    }

    /// Pushes this thread onto the stack
    /// Returns true if this thread is the main thread of its state
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushthread
    pub fn luna_pushthread(self: *Self) bool {
        return c.lua_pushthread(self.state) != 0;
    }

    /// Pushes a copy of the element at the given index onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_pushvalue
    pub fn luna_pushvalue(self: *Self, index: i32) void {
        c.lua_pushvalue(self.state, index);
    }

    /// Returns true if the two values in indices `index1` and `index2` are primitively equal
    /// Bypasses __eq metamethods
    /// Returns false if not equal, or if any index is invalid
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawequal
    pub fn luna_rawequal(self: *Self, index1: i32, index2: i32) bool {
        return c.lua_rawequal(self.state, index1, index2) != 0;
    }

    /// Similar to `Lua.getTable()` but does a raw access (without metamethods)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawget
    pub fn luna_rawget(self: *Self, index: i32) LuaType {
        return @enumFromInt(c.lua_rawget(self.state, index));
    }

    /// Pushes onto the stack the value t[n], where `t` is the table at the given `index`
    /// Returns the `LuaType` of the pushed value
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawgeti
    pub fn luna_rawgeti(self: *Self, index: i32, n: LuaInteger) LuaType {
        return @enumFromInt(c.lua_rawgeti(self.state, index, n));
    }

    /// Pushes onto the stack the value t[k] where t is the table at the given `index` and
    /// k is the pointer `p` represented as a light userdata
    /// rawgetp
    pub fn luna_rawgetp(self: *Self, index: i32, p: *const anyopaque) LuaType {
        return @enumFromInt(c.lua_rawgetp(self.state, index, p));
    }

    /// Returns the raw length of the value at the given index
    /// For strings it is the length; for tables it is the result of the `#` operator
    /// For userdata it is the size of the block of memory
    /// For other values the call returns 0
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawlen
    pub fn luna_rawlen(self: *Self, index: i32) LuaUnsigned {
        return c.lua_rawlen(self.state, index);
    }

    /// Similar to `Lua.setTable()` but does a raw assignment (without metamethods)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawset
    pub fn luna_rawset(self: *Self, index: i32) void {
        c.lua_rawset(self.state, index);
    }

    /// Does the equivalent of t[`i`] = v where t is the table at the given `index`
    /// and v is the value at the top of the stack
    /// Pops the value from the stack. Does not use __newindex metavalue
    /// See https://www.lua.org/manual/5.4/manual.html#lua_rawseti
    pub fn luna_rawseti(self: *Self, index: i32, i: LuaInteger) void {
        c.lua_rawseti(self.state, index, i);
    }

    /// Does the equivalent of t[p] = v where t is the table at the given `index`
    /// `p` is encoded as a light user data, and v is the value at the top of the stack
    /// Pops the value from the stack. Does not use __newindex metavalue
    /// rawsetp
    pub fn luna_rawsetp(self: *Self, index: i32, p: *const anyopaque) void {
        c.lua_rawsetp(self.state, index, p);
    }

    /// Sets the C function f as the new value of global name
    /// See https://www.lua.org/manual/5.4/manual.html#lua_register
    pub fn luna_register(self: *Self, name: [:0]const u8, c_fn: LuaCFunction) void {
        c.lua_register(self.state, name.ptr, c_fn);
    }

    /// Removes the element at the given valid `index` shifting down elements to fill the gap
    /// See https://www.lua.org/manual/5.4/manual.html#lua_remove
    pub fn luna_remove(self: *Self, index: i32) void {
        self.luna_rotate(index, -1);
        self.luna_pop(1);
    }

    /// Moves the top element into the given valid `index` without shifting any elements,
    /// then pops the top element
    /// See https://www.lua.org/manual/5.4/manual.html#lua_replace
    pub fn luna_replace(self: *Self, index: i32) void {
        self.luna_copy(-1, index);
        self.luna_pop(1);
    }

    /// Copies the element at from_index to the valid index to_index, replacing the value at that position
    /// See https://www.lua.org/manual/5.4/manual.html#lua_copy
    pub fn luna_copy(self: *Self, from_index: i32, to_index: i32) void {
        c.lua_copy(self.state, from_index, to_index);
    }

    /// This function is deprecated; it is equivalent to closeThread() with from being null.
    /// See https://www.lua.org/manual/5.4/manual.html#lua_resetthread
    pub fn luna_resetthread(self: *Self) !void {
        return self.luna_closethread(null);
    }

    /// Resets a thread, cleaning its call stack and closing all pending to-be-closed variables.
    /// Returns a status code: LUA_OK for no errors in the thread, or an error status otherwise.
    /// In case of error, leaves the error object on the top of the stack.
    /// The parameter from represents the coroutine that is resetting L.
    /// If there is no such coroutine, this parameter can be NULL.
    /// (This function was introduced in release 5.4.6.)
    /// See https://www.lua.org/manual/5.4/manual.html#lua_closethread
    pub fn luna_closethread(self: *Self, from: ?Luna) !void {
        if (c.lua_closethread(self.state, if (from) |f| f.state else null) != LuaThreadStatus.ok) return error.Fail;
    }

    /// Compares two Lua values
    /// Returns true if the value at index1 satisisfies the comparison with the value at index2
    /// Returns false otherwise, or if any index is not valid
    /// See https://www.lua.org/manual/5.4/manual.html#lua_compare
    pub fn luna_compare(self: *Self, index1: i32, index2: i32, op: LuaCompareOperator) bool {
        return c.lua_compare(self.state, index1, index2, @intFromEnum(op)) != 0;
    }

    /// Starts and resumes a coroutine in the given thread
    /// See https://www.lua.org/manual/5.4/manual.html#lua_resume
    pub fn luna_resume(self: *Self, from: ?Luna, num_args: i32, num_results: *i32) !LuaThreadResumeStatus {
        const from_state = if (from) |from_val| from_val.state else null;
        const thread_status = c.lua_resume(self.state, from_state, num_args, num_results);
        switch (thread_status) {
            LuaThreadStatus.err_runtime => return error.Runtime,
            LuaThreadStatus.err_memory => return error.Memory,
            LuaThreadStatus.err_error => return error.MsgHandler,
            else => return @enumFromInt(thread_status),
        }
    }

    /// Changes the allocator function of a given state to `alloc_fn` with userdata `data`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setallocf
    pub fn luna_setallocf(self: *Self, alloc_fn: LuaAlloc, data: ?*anyopaque) void {
        c.lua_setallocf(self.state, alloc_fn, data);
    }

    /// Does the equivalent to t[`k`] = v where t is the value at the given `index`
    /// and v is the value on the top of the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setfield
    pub fn luna_setfield(self: *Self, index: i32, k: [:0]const u8) void {
        c.lua_setfield(self.state, index, k.ptr);
    }

    /// Pops a value from the stack and sets it as the new value of global `name`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setglobal
    pub fn luna_setglobal(self: *Self, name: [:0]const u8) void {
        c.lua_setglobal(self.state, name.ptr);
    }

    /// Does the equivalent to t[`n`] = v where t is the value at the given `index`
    /// and v is the value on the top of the stack. Pops the value from the stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_seti
    pub fn luna_seti(self: *Self, index: i32, n: LuaInteger) void {
        c.lua_seti(self.state, index, n);
    }

    /// Pops a value from the stack and sets it as the new `n`th user value associated to
    /// the full userdata at the given index
    /// Returns an error if the userdata does not have that value
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setiuservalue
    pub fn luna_setiuservalue(self: *Self, index: i32, n: i32) !void {
        if (c.lua_setiuservalue(self.state, index, n) == 0) return error.Fail;
    }

    /// Pops a table or nil from the stack and sets that value as the new metatable for the
    /// value at the given `index`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setmetatable
    pub fn luna_setmetatable(self: *Self, index: i32) void {
        // lua_setmetatable always returns 1 so is safe to ignore
        _ = c.lua_setmetatable(self.state, index);
    }

    /// Does the equivalent to t[k] = v, where t is the value at the given `index`
    /// v is the value on the top of the stack, and k is the value just below the top
    /// See https://www.lua.org/manual/5.4/manual.html#lua_settable
    pub fn luna_settable(self: *Self, index: i32) void {
        c.lua_settable(self.state, index);
    }

    /// Returns the status of this thread
    /// See https://www.lua.org/manual/5.4/manual.html#lua_status
    pub fn luna_status(self: *Self) LuaStatus {
        return @enumFromInt(c.lua_status(self.state));
    }

    /// Converts the zero-terminated string `str` to a number, pushes that number onto the stack,
    /// Returns an error if conversion failed
    /// See https://www.lua.org/manual/5.4/manual.html#lua_stringtonumber
    pub fn luna_stringtonumber(self: *Self, str: [:0]const u8) !void {
        const size = c.lua_stringtonumber(self.state, str.ptr);
        if (size == 0) return error.Fail;
    }

    /// Converts the Lua value at the given `index` into a boolean
    /// The Lua value at the index will be considered true unless it is false or nil
    /// See https://www.lua.org/manual/5.4/manual.html#lua_toboolean
    pub fn luna_toboolean(self: *Self, index: i32) bool {
        return c.lua_toboolean(self.state, index) != 0;
    }

    /// Converts a value at the given `index` into a CFn
    /// Returns an error if the value is not a CFn
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tocfunction
    pub fn luna_tocfunction(self: *Self, index: i32) !LuaCFunction {
        return c.lua_tocfunction(self.state, index) orelse return error.Fail;
    }

    /// Marks the given index in the stack as a to-be-closed slot
    /// See https://www.lua.org/manual/5.4/manual.html#lua_toclose
    pub fn luna_toclose(self: *Self, index: i32) void {
        c.lua_toclose(self.state, index);
    }

    /// Converts the Lua value at the given `index` to a signed integer
    /// The Lua value must be an integer, or a number, or a string convertible to an integer otherwise toIntegerX returns 0
    /// Returns an error if the conversion failed
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tointeger
    pub fn luna_tointeger(self: *Self, index: i32) !LuaInteger {
        var success: c_int = undefined;
        const result = c.lua_tointegerx(self.state, index, &success);
        if (success == 0) return error.Fail;
        return result;
    }

    /// Returns a slice of bytes at the given index
    /// If the value is not a string or number, returns an error
    /// If the value was a number the actual value in the stack will be changed to a string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tolstring
    pub fn luna_tolstring(self: *Self, index: i32) ![:0]const u8 {
        var length: usize = undefined;
        if (c.lua_tolstring(self.state, index, &length)) |ptr| return ptr[0..length :0];
        return error.Fail;
    }

    /// Converts the Lua value at the given `index` to a float
    /// The Lua value must be a number or a string convertible to a number otherwise toNumberX returns 0
    /// Returns an error if the conversion failed
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tonumber
    pub fn luna_tonumber(self: *Self, index: i32) !LuaNumber {
        var success: c_int = undefined;
        const result = c.lua_tonumberx(self.state, index, &success);
        if (success == 0) return error.Fail;
        return result;
    }

    /// Converts the value at the given `index` to an opaque pointer
    /// See https://www.lua.org/manual/5.4/manual.html#lua_topointer
    pub fn luna_topointer(self: *Self, index: i32) !*const anyopaque {
        if (c.lua_topointer(self.state, index)) |ptr| return ptr;
        return error.Fail;
    }

    /// Converts the Lua value at the given `index` to a zero-terminated many-itemed-pointer (string)
    /// Returns an error if the conversion failed
    /// If the value was a number the actual value in the stack will be changed to a string
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tostring
    pub fn luna_tostring(self: *Self, index: i32) ![*:0]const u8 {
        var length: usize = undefined;
        if (c.lua_tolstring(self.state, index, &length)) |str| return str;
        return error.Fail;
    }

    /// Converts the value at the given `index` to a Lua thread (wrapped with a `Lua` struct)
    /// The thread does _not_ contain an allocator because it is not the main thread and should therefore not be used with `deinit()`
    /// Returns an error if the value is not a thread
    /// See https://www.lua.org/manual/5.4/manual.html#lua_tothread
    pub fn luna_tothread(self: *Self, index: i32) !Self {
        const thread = c.lua_tothread(self.state, index);
        if (thread) |thread_ptr| return .{ .state = thread_ptr };
        return error.Fail;
    }

    /// Returns a Lua-owned userdata pointer of the given type at the given index.
    /// Works for both light and full userdata.
    /// Returns an error if the value is not a userdata.
    /// See https://www.lua.org/manual/5.4/manual.html#lua_touserdata
    pub fn luna_touserdata(self: *Self, comptime T: type, index: i32) !*T {
        if (c.lua_touserdata(self.state, index)) |ptr| return opaqueCast(T, ptr);
        return error.Fail;
    }

    /// Returns a Lua-owned userdata slice of the given type at the given index.
    /// Returns an error if the value is not a userdata.
    /// See https://www.lua.org/manual/5.4/manual.html#lua_touserdata
    pub fn luna_touserdatauv(self: *Self, comptime T: type, index: i32) ![]T {
        if (c.lua_touserdata(self.state, index)) |ptr| {
            const size = self.luna_rawlen(index) / @sizeOf(T);
            return @as([*]T, @ptrCast(@alignCast(ptr)))[0..size];
        }
        return error.Fail;
    }

    /// Returns the `LuaType` of the value at the given index
    /// Note that this is equivalent to lua_type but because type is a Zig primitive it is renamed to `typeOf`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_type
    pub fn luna_type(self: *Self, index: i32) LuaType {
        return @enumFromInt(c.lua_type(self.state, index));
    }

    /// Returns the name of the given `LuaType` as a null-terminated slice
    /// See https://www.lua.org/manual/5.4/manual.html#lua_typename
    pub fn luna_typename(self: *Self, t: LuaType) [:0]const u8 {
        return std.mem.span(c.lua_typename(self.state, @intFromEnum(t)));
    }

    /// Returns the pseudo-index that represents the `i`th upvalue of the running function
    /// See https://www.lua.org/manual/5.4/manual.html#lua_upvalueindex
    pub fn luna_upvalueindex(self: *Self, i: i32) i32 {
        _ = self;
        return c.lua_upvalueindex(i);
    }

    /// Returns the version number of this core
    /// See https://www.lua.org/manual/5.4/manual.html#lua_version
    pub fn luna_version(self: *Self) LuaNumber {
        return c.lua_version(self.state);
    }

    /// Emits a warning with the given `msg`
    /// A message with `to_cont` as true should be continued in a subsequent call to the function
    /// See https://www.lua.org/manual/5.4/manual.html#lua_warning
    pub fn luna_warning(self: *Self, msg: [:0]const u8, to_cont: bool) void {
        c.lua_warning(self.state, msg.ptr, @intFromBool(to_cont));
    }

    /// Sets the warning function to be used by Lua to emit warnings
    /// The `data` parameter sets the value `data` passed to the warning function
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setwarnf
    pub fn luna_setwarnf(self: *Self, warn_fn: LuaWarnFunction, data: ?*anyopaque) void {
        c.lua_setwarnf(self.state, warn_fn, data);
    }

    /// Pops `num` values from the current stack and pushes onto the stack of `to`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_xmove
    pub fn luna_xmove(self: *Self, to: Luna, num: i32) void {
        c.lua_xmove(self.state, to.state, num);
    }

    /// This function is equivalent to `Lua.yieldCont()` but has no continuation
    /// This function never returns
    /// See https://www.lua.org/manual/5.4/manual.html#lua_yield
    pub fn luna_yield(self: *Self, num_results: i32) noreturn {
        _ = c.lua_yieldk(self.state, num_results, 0, null);
        unreachable;
    }

    /// Yields this coroutine (thread)
    /// This function never returns
    /// See https://www.lua.org/manual/5.4/manual.html#lua_yieldk
    pub fn luna_yieldk(self: *Self, num_results: i32, ctx: LuaKContext, k: LuaKFunction) noreturn {
        _ = c.lua_yieldk(self.state, num_results, ctx, k);
        unreachable;
    }

    // Debug library functions
    //
    // The debug interface functions are included in alphabetical order
    // Each is kept similar to the original C API function while also making it easy to use from Zig

    /// Returns the current hook function
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gethook
    pub fn luna_gethook(self: *Self) ?LuaHook {
        return c.lua_gethook(self.state);
    }

    /// Returns the current hook count
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gethookcount
    pub fn luna_gethookcount(self: *Self) i32 {
        return c.lua_gethookcount(self.state);
    }

    /// Returns the current hook mask
    /// See https://www.lua.org/manual/5.4/manual.html#lua_gethookmask
    pub fn luna_gethookmask(self: *Self) LuaHookMask {
        return LuaHookMask.fromInt(c.lua_gethookmask(self.state));
    }

    /// Gets information about a specific function or function invocation
    /// Returns an error if an invalid option was given, but the valid options
    /// are still handled
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getinfo
    pub fn luna_getinfo(self: *Self, options: LuaDebugInfo.Options, info: *LuaDebugInfo) void {
        const str = options.toString();

        var ar: LuaDebug = undefined;
        ar.i_ci = @ptrCast(info.private);

        // should never fail because we are controlling options with the struct param
        _ = c.lua_getinfo(self.state, &str, &ar);
        // std.debug.assert( != 0);

        // copy data into a struct
        if (options.l) info.current_line = if (ar.currentline == -1) null else ar.currentline;
        if (options.n) {
            info.name = if (ar.name != null) std.mem.span(ar.name) else null;
            info.name_what = blk: {
                const what = std.mem.span(ar.namewhat);
                if (std.mem.eql(u8, "global", what)) break :blk .global;
                if (std.mem.eql(u8, "local", what)) break :blk .local;
                if (std.mem.eql(u8, "method", what)) break :blk .method;
                if (std.mem.eql(u8, "field", what)) break :blk .field;
                if (std.mem.eql(u8, "upvalue", what)) break :blk .upvalue;
                if (what.len == 0) break :blk .other;
                unreachable;
            };
        }
        if (options.r) {
            info.first_transfer = ar.ftransfer;
            info.num_transfer = ar.ntransfer;
        }
        if (options.S) {
            info.source = std.mem.span(ar.source);
            @memcpy(&info.short_src, &ar.short_src);
            info.first_line_defined = ar.linedefined;
            info.last_line_defined = ar.lastlinedefined;
            info.what = blk: {
                const what = std.mem.span(ar.what);
                if (std.mem.eql(u8, "Lua", what)) break :blk .lua;
                if (std.mem.eql(u8, "C", what)) break :blk .c;
                if (std.mem.eql(u8, "main", what)) break :blk .main;
                unreachable;
            };
        }
        if (options.t) info.is_tail_call = ar.istailcall != 0;
        if (options.u) {
            info.num_upvalues = ar.nups;
            info.num_params = ar.nparams;
            info.is_vararg = ar.isvararg != 0;
        }
    }

    /// Gets information about a local variable
    /// Returns the name of the local variable
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getlocal
    pub fn luna_getlocal(self: *Self, info: *LuaDebugInfo, n: i32) ![:0]const u8 {
        var ar: LuaDebug = undefined;
        ar.i_ci = @ptrCast(info.private);
        if (c.lua_getlocal(self.state, &ar, n)) |name| {
            return std.mem.span(name);
        }
        return error.Fail;
    }

    /// Gets information about the interpreter runtime stack
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getstack
    pub fn luna_getstack(self: *Self, level: i32) !LuaDebugInfo {
        var ar: LuaDebug = undefined;
        if (c.lua_getstack(self.state, level, &ar) == 0) return error.Fail;
        return LuaDebugInfo{ .private = ar.i_ci.? };
    }

    /// Gets information about the `n`th upvalue of the closure at index `func_index`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_getupvalue
    pub fn luna_getupvalue(self: *Self, func_index: i32, n: i32) ![:0]const u8 {
        if (c.lua_getupvalue(self.state, func_index, n)) |name| {
            return std.mem.span(name);
        }
        return error.Fail;
    }

    /// Sets the debugging hook function
    /// See https://www.lua.org/manual/5.4/manual.html#lua_sethook
    pub fn luna_sethook(self: *Self, hook_fn: LuaHook, mask: LuaHookMask, count: i32) void {
        const hook_mask = LuaHookMask.toInt(mask);
        c.lua_sethook(self.state, hook_fn, hook_mask, count);
    }

    /// Sets the value of a local variable
    /// Returns an error when the index is greater than the number of active locals
    /// Returns the name of the local variable
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setlocal
    pub fn luna_setlocal(self: *Self, info: *LuaDebugInfo, n: i32) ![:0]const u8 {
        var ar: LuaDebug = undefined;
        ar.i_ci = @ptrCast(info.private);
        if (c.lua_setlocal(self.state, &ar, n)) |name| {
            return std.mem.span(name);
        }
        return error.Fail;
    }

    /// Sets the value of a closure's upvalue
    /// Returns the name of the upvalue or an error if the upvalue does not exist
    /// See https://www.lua.org/manual/5.4/manual.html#lua_setupvalue
    pub fn luna_setupvalue(self: *Self, func_index: i32, n: i32) ![:0]const u8 {
        if (c.lua_setupvalue(self.state, func_index, n)) |name| {
            return std.mem.span(name);
        }
        return error.Fail;
    }

    /// Returns a unique identifier for the upvalue numbered `n` from the closure index `func_index`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_upvalueid
    pub fn luna_upvalueid(self: *Self, func_index: i32, n: i32) !*anyopaque {
        if (c.lua_upvalueid(self.state, func_index, n)) |ptr| return ptr;
        return error.Fail;
    }

    /// Make the `n1`th upvalue of the Lua closure at index `func_index1` refer to the `n2`th upvalue
    /// of the Lua closure at index `func_index2`
    /// See https://www.lua.org/manual/5.4/manual.html#lua_upvaluejoin
    pub fn luna_upvaluejoin(self: *Self, func_index1: i32, n1: i32, func_index2: i32, n2: i32) void {
        c.lua_upvaluejoin(self.state, func_index1, n1, func_index2, n2);
    }

    // Auxiliary library functions
    //
    // Auxiliary library functions are included in alphabetical order.
    // Each is kept similar to the original C API function while also making it easy to use from Zig

    /// Checks whether `cond` is true. Raises an error using `Lua.argError()` if not
    /// Possibly never returns
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_argcheck
    pub fn lunaL_argcheck(self: *Self, cond: bool, arg: i32, extra_msg: [:0]const u8) void {
        // translate-c failed
        if (!cond) self.lunaL_argerror(arg, extra_msg);
    }

    /// Raises an error reporting a problem with argument `arg` of the C function that called it
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_argerror
    pub fn lunaL_argerror(self: *Self, arg: i32, extra_msg: [*:0]const u8) noreturn {
        _ = c.luaL_argerror(self.state, arg, extra_msg);
        unreachable;
    }

    /// Checks whether `cond` is true. Raises an error using `Lua.typeError()` if not
    /// Possibly never returns
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_argexpected
    pub fn lunaL_argexpected(self: *Self, cond: bool, arg: i32, type_name: [:0]const u8) void {
        if (cond) self.lunaL_typeerror(arg, type_name);
    }

    /// Raises a type error for the argument `arg` of the C function that called it
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_typeerror
    pub fn lunaL_typeerror(self: *Self, arg: i32, type_name: [:0]const u8) noreturn {
        _ = c.luaL_typeerror(self.state, arg, type_name.ptr);
        unreachable;
    }

    /// Calls a metamethod
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_callmeta
    pub fn lunaL_callmeta(self: *Self, obj: i32, field: [:0]const u8) !void {
        if (c.luaL_callmeta(self.state, obj, field.ptr) == 0) return error.Fail;
    }

    /// Checks whether the function has an argument of any type at position `arg`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkany
    pub fn lunaL_checkany(self: *Self, arg: i32) void {
        c.luaL_checkany(self.state, arg);
    }

    /// Checks whether the function argument `arg` is an integer (or can be converted to an integer) and returns the integer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkinteger
    pub fn lunaL_checkinteger(self: *Self, arg: i32) LuaInteger {
        return c.luaL_checkinteger(self.state, arg);
    }

    /// Checks whether the function argument `arg` is a slice of bytes and returns the slice
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checklstring
    pub fn lunaL_checklstring(self: *Self, arg: i32) [:0]const u8 {
        var length: usize = 0;
        const str = c.luaL_checklstring(self.state, arg, &length);
        // luaL_checklstring never returns null (throws lua error)
        return str[0..length :0];
    }

    /// Checks whether the function argument `arg` is a number and returns the number
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checknumber
    pub fn lunaL_checknumber(self: *Self, arg: i32) LuaNumber {
        return c.luaL_checknumber(self.state, arg);
    }

    /// Checks whether the function argument `arg` is a string and searches for the enum value with the same name in `T`.
    /// `default` is used as a default value when not null
    /// Returns the enum value found
    /// Useful for mapping Lua strings to Zig enums
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkoption
    pub fn lunaL_checkoption(self: *Self, comptime T: type, arg: i32, default: ?T) T {
        const name = blk: {
            if (default) |defaultName| {
                break :blk self.lunaL_optlstring(arg, @tagName(defaultName));
            } else {
                break :blk self.lunaL_checklstring(arg);
            }
        };

        inline for (std.meta.fields(T)) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return @enumFromInt(field.value);
            }
        }

        return self.lunaL_argerror(arg, self.luna_pushfstring("invalid option '%s'", .{name.ptr}));
    }

    /// Grows the stack size to top + `size` elements, raising an error if the stack cannot grow to that size
    /// `msg` is an additional text to go into the error message
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkstack
    pub fn lunaL_checkstack(self: *Self, size: i32, msg: ?[*:0]const u8) void {
        c.luaL_checkstack(self.state, size, msg);
    }

    /// Checks whether the function argument `arg` is a string and returns the string
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkstring
    pub fn lunaL_checkstring(self: *Self, arg: i32) [*:0]const u8 {
        return c.luaL_checklstring(self.state, arg, null);
    }

    /// Checks whether the function argument `arg` has type `t`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checktype
    pub fn lunaL_checktype(self: *Self, arg: i32, t: LuaType) void {
        c.luaL_checktype(self.state, arg, @intFromEnum(t));
    }

    /// Checks whether the function argument `arg` is a userdata of the type `name`
    /// Returns a pointer to the userdata
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkudata
    pub fn lunaL_checkudata(self: *Self, comptime T: type, arg: i32, name: [:0]const u8) *T {
        // the returned pointer will not be null
        return opaqueCast(T, c.luaL_checkudata(self.state, arg, name.ptr).?);
    }

    /// Checks whether the function argument `arg` is a userdata of the type `name`
    /// Returns a Lua-owned userdata slice
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkudata
    pub fn lunaL_checkudatauv(self: *Self, comptime T: type, arg: i32, name: [:0]const u8) []T {
        // the returned pointer will not be null
        const ptr = c.luaL_checkudata(self.state, arg, name.ptr).?;
        const size = self.luna_rawlen(arg) / @sizeOf(T);
        return @as([*]T, @ptrCast(@alignCast(ptr)))[0..size];
    }

    /// Checks whether the code making the call and the Lua library being called are using
    /// the same version of Lua and the same numeric types.
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkversion
    pub fn lunaL_checkversion(self: *Self) void {
        return c.luaL_checkversion_(self.state, c.LUA_VERSION_NUM, c.LUAL_NUMSIZES);
    }

    /// Raises an error
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_error
    pub fn lunaL_error(self: *Self, fmt: [:0]const u8, args: anytype) noreturn {
        _ = @call(.auto, c.luaL_error, .{ self.state, fmt.ptr } ++ args);
        unreachable;
    }

    /// Loads and runs the given string
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_dostring
    pub fn lunaL_dostring(self: *Self, str: [:0]const u8) !void {
        try self.lunaL_loadstring(str);
        try self.luna_pcall(0, LuaMultRet, 0);
    }

    /// Pushes onto the stack the field `e` from the metatable of the object at index `obj`
    /// and returns the type of the pushed value
    /// TODO: possibly return an error if nil
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_getmetafield
    pub fn lunaL_getmetafield(self: *Self, obj: i32, field: [:0]const u8) !LuaType {
        const val_type: LuaType = @enumFromInt(c.luaL_getmetafield(self.state, obj, field.ptr));
        if (val_type == .nil) return error.Fail;
        return val_type;
    }

    /// Pushes onto the stack the metatable associated with the name `type_name` in the registry
    /// or nil if there is no metatable associated with that name. Returns the type of the pushed value
    /// TODO: return error when type is nil?
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_getmetatable
    pub fn lunaL_getmetatable(self: *Self, table_name: [:0]const u8) LuaType {
        return @enumFromInt(c.luaL_getmetatable(self.state, table_name.ptr));
    }

    /// Ensures that the value t[`field`], where t is the value at `index`, is a table, and pushes that table onto the stack.
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_getsubtable
    pub fn lunaL_getsubtable(self: *Self, index: i32, field: [:0]const u8) !void {
        if (c.luaL_getsubtable(self.state, index, field.ptr) == 0) return error.Fail;
    }

    /// Creates a copy of string `str`, replacing any occurrence of the string `pat` with the string `rep`
    /// Pushes the resulting string on the stack and returns it.
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_gsub
    pub fn lunaL_gsub(self: *Self, str: [:0]const u8, pat: [:0]const u8, rep: [:0]const u8) [:0]const u8 {
        return std.mem.span(c.luaL_gsub(self.state, str.ptr, pat.ptr, rep.ptr));
    }

    /// Returns the "length" of the value at the given index as a number
    /// it is equivalent to the '#' operator in Lua
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_len
    pub fn lunaL_len(self: *Self, index: i32) LuaInteger {
        return c.luaL_len(self.state, index);
    }

    /// Loads a buffer as a Lua chunk
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_loadbufferx
    pub fn lunaL_loadbufferx(self: *Self, buf: []const u8, name: [:0]const u8, mode: LuaMode) !void {
        const mode_str = switch (mode) {
            .binary => "b",
            .text => "t",
            .binary_text => "bt",
        };
        switch (c.luaL_loadbufferx(self.state, buf.ptr, buf.len, name.ptr, mode_str.ptr)) {
            LuaThreadStatus.ok => return,
            LuaThreadStatus.err_syntax => return error.Syntax,
            LuaThreadStatus.err_memory => return error.Memory,
            else => unreachable,
        }
    }

    /// Loads a file as a Lua chunk
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_loadfilex
    pub fn lunaL_loadfilex(self: *Self, file_name: [:0]const u8, mode: LuaMode) !void {
        const mode_str = switch (mode) {
            .binary => "b",
            .text => "t",
            .binary_text => "bt",
        };
        const ret = c.luaL_loadfilex(self.state, file_name.ptr, mode_str.ptr);
        switch (ret) {
            LuaThreadStatus.ok => return,
            LuaThreadStatus.err_syntax => return error.Syntax,
            LuaThreadStatus.err_memory => return error.Memory,
            lua_errfile => return error.File,
            // NOTE: the docs mention possible other return types, but I couldn't figure them out
            else => unreachable,
        }
    }

    /// Loads a string as a Lua chunk
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_loadstring
    pub fn lunaL_loadstring(self: *Self, str: [:0]const u8) !void {
        const ret = c.luaL_loadstring(self.state, str.ptr);
        switch (ret) {
            LuaThreadStatus.ok => return,
            LuaThreadStatus.err_syntax => return error.Syntax,
            LuaThreadStatus.err_memory => return error.Memory,
            // loadstring runs lua_load which runs pcall, so can also return any result of an pcall error
            LuaThreadStatus.err_runtime => return error.Runtime,
            LuaThreadStatus.err_error => return error.MsgHandler,
            else => unreachable,
        }
    }

    /// Creates a new table and registers there the functions in `list`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_newlib
    pub fn lunaL_newlib(self: *Self, list: []const LuaReg) void {
        self.lunaL_checkversion();
        self.lunaL_newlibtable(list);
        self.lunaL_setfuncs(list, 0);
    }

    /// Creates a new table with a size optimized to store all entries in the array `list`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_newlibtable
    pub fn lunaL_newlibtable(self: *Self, list: []const LuaReg) void {
        self.luna_createtable(0, @intCast(list.len));
    }

    /// Registers all functions in the array `fns` into the table on the top of the stack
    /// All functions are created with `num_upvalues` upvalues
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_setfuncs
    pub fn lunaL_setfuncs(self: *Self, funcs: []const LuaReg, num_upvalues: i32) void {
        self.lunaL_checkstack(num_upvalues, "too many upvalues");

        for (funcs) |f| {
            if (f.func) |func| {
                var i: i32 = 0;
                // copy upvalues to the top
                while (i < num_upvalues) : (i += 1) self.luna_pushvalue(-num_upvalues);
                self.luna_pushcclosure(func, num_upvalues);
            } else self.luna_pushboolean(false); // register a placeholder
            self.luna_setfield(-(num_upvalues + 2), f.name);
        }

        self.luna_pop(num_upvalues);
    }

    /// If the registry already has the key `key`, returns an error
    /// Otherwise, creates a new table to be used as a metatable for userdata
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_newmetatable
    pub fn lunaL_newmetatable(self: *Self, key: [:0]const u8) !void {
        if (c.luaL_newmetatable(self.state, key.ptr) == 0) return error.Fail;
    }

    /// Creates a new Lua state with an allocator using the default libc allocator
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_newstate
    pub fn lunaL_newstate() !Self {
        const state = c.luaL_newstate() orelse return error.Memory;
        return .{ .state = state };
    }

    /// If the function argument `arg` is an integer, returns the integer
    /// If the argument is absent or nil returns `default`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_optinteger
    pub fn lunaL_optinteger(self: *Self, arg: i32, default: LuaInteger) LuaInteger {
        return c.luaL_optinteger(self.state, arg, default);
    }

    /// If the function argument `arg` is a slice of bytes, returns the slice
    /// If the argument is absent or nil returns `default`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_optlstring
    pub fn lunaL_optlstring(self: *Self, arg: i32, default: [:0]const u8) [:0]const u8 {
        var length: usize = 0;
        // will never return null because default cannot be null
        const ret: [*]const u8 = c.luaL_optlstring(self.state, arg, default.ptr, &length);
        if (ret == default.ptr) return default;
        return ret[0..length :0];
    }

    /// If the function argument `arg` is a number, returns the number
    /// If the argument is absent or nil returns `default`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_optnumber
    pub fn lunaL_optnumber(self: *Self, arg: i32, default: LuaNumber) LuaNumber {
        return c.luaL_optnumber(self.state, arg, default);
    }

    /// If the function argument `arg` is a string, returns the string
    /// If the argment is absent or nil returns `default`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_optstring
    pub fn lunaL_optstring(self: *Self, arg: i32, default: [:0]const u8) [*:0]const u8 {
        return c.luaL_optlstring(self.state, arg, default.ptr, null);
    }

    /// Pushes the fail value onto the stack
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_pushfail
    pub fn lunaL_pushfail(self: *Self) void {
        c.luaL_pushfail(self.state);
    }

    /// Creates and returns a reference in the table at index `index` for the object on the top of the stack
    /// Pops the object
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_ref
    pub fn lunaL_ref(self: *Self, index: i32) !i32 {
        const ret = c.luaL_ref(self.state, index);
        return if (ret == ref_nil) error.Fail else ret;
    }

    /// If package.loaded[`mod_name`] is not true, calls the function `open_fn` with `mod_name`
    /// as an argument and sets the call result to package.loaded[`mod_name`]
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_requiref
    pub fn lunaL_requiref(self: *Self, mod_name: [:0]const u8, open_fn: LuaCFunction, global: bool) void {
        c.luaL_requiref(self.state, mod_name.ptr, open_fn, @intFromBool(global));
    }

    /// Sets the metatable of the object on the top of the stack as the metatable associated
    /// with `table_name` in the registry
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_setmetatable
    pub fn lunaL_setmetatable(self: *Self, table_name: [:0]const u8) void {
        c.luaL_setmetatable(self.state, table_name.ptr);
    }

    /// This function works like `Lua.checkUserdata()` except it returns a Zig error instead of raising a Lua error on fail
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_testudata
    pub fn lunaL_testudata(self: *Self, comptime T: type, arg: i32, name: [:0]const u8) !*T {
        if (c.luaL_testudata(self.state, arg, name.ptr)) |ptr| {
            return opaqueCast(T, ptr);
        } else return error.Fail;
    }

    /// This function works like `Lua.checkUserdataSlice()` except it returns a Zig error instead of raising a Lua error on fail
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_checkudata
    pub fn lunaL_testudatauv(self: *Self, comptime T: type, arg: i32, name: [:0]const u8) ![]T {
        if (c.luaL_testudata(self.state, arg, name.ptr)) |ptr| {
            const size = self.luna_rawlen(arg) / @sizeOf(T);
            return @as([*]T, @ptrCast(@alignCast(ptr)))[0..size];
        } else return error.Fail;
    }

    /// Converts any Lua value at the given index into a string in a reasonable format
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_tolstring
    pub fn lunaL_tolstring(self: *Self, index: i32) [:0]const u8 {
        var length: usize = undefined;
        const ptr = c.luaL_tolstring(self.state, index, &length);
        return ptr[0..length :0];
    }

    /// Creates and pushes a traceback of the stack of `other`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_traceback
    pub fn lunaL_traceback(self: *Self, other: *Luna, msg: [:0]const u8, level: i32) void {
        c.luaL_traceback(self.state, other.state, msg.ptr, level);
    }

    /// Returns the name of the type of the value at the given `index`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_typename
    pub fn lunaL_typename(self: *Self, index: i32) [:0]const u8 {
        return std.mem.span(c.luaL_typename(self.state, index));
    }

    /// Releases the reference `r` from the table at index `index`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_unref
    pub fn lunaL_unref(self: *Self, index: i32, r: i32) void {
        c.luaL_unref(self.state, index, r);
    }

    /// Pushes onto the stack a string identifying the current position of the control
    /// at the call stack `level`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_where
    pub fn lunaL_where(self: *Self, level: i32) void {
        c.luaL_where(self.state, level);
    }

    // Standard library loading functions

    pub fn lunaL_initcodecache(self: *Self) void {
        _ = self;
        c.luaL_initcodecache();
    }

    pub fn luna_open_cache(self: *Self) void {
        self.lunaL_requiref("skynet_codecache", c.luaopen_cache, true);
    }

    /// Open all standard libraries
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_openlibs
    pub fn lunaL_openlibs(self: *Self) void {
        c.luaL_openlibs(self.state);
    }
};

/// A string buffer allowing for Zig code to build Lua strings piecemeal
/// All LuaBuffer functions are wrapped in this struct to make the API more convenient to use
pub const LunaBuffer = struct {
    b: LuaBuffer = undefined,

    /// Internal Lua type for a string buffer
    pub const LuaBuffer = c.luaL_Buffer;

    pub const buffer_size = c.LUAL_BUFFERSIZE;

    const Self = @This();

    /// Initialize a Lua string buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_buffinit
    pub fn lunaL_buffinit(self: *Self, L: Luna) void {
        c.luaL_buffinit(L.state, &self.b);
    }

    /// Initialize a Lua string buffer with an initial size
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_buffinitsize
    pub fn lunaL_buffinitsize(self: *Self, L: Luna, size: usize) []u8 {
        return c.luaL_buffinitsize(L.state, &self.b, size)[0..size];
    }

    /// Adds `byte` to the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addchar
    pub fn lunaL_addchar(self: *Self, byte: u8) void {
        // could not be translated by translate-c
        var lua_buf = &self.b;
        if (lua_buf.n >= lua_buf.size) _ = self.lunaL_prepbuffsize(1);
        lua_buf.b[lua_buf.n] = byte;
        lua_buf.n += 1;
    }

    /// Adds a copy of the string `str` to the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addgsub
    pub fn lunaL_addgsub(self: *Self, str: [:0]const u8, pat: [:0]const u8, rep: [:0]const u8) void {
        c.luaL_addgsub(&self.b, str.ptr, pat.ptr, rep.ptr);
    }

    /// Adds the string to the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addlstring
    pub fn lunaL_addlstring(self: *Self, str: []const u8) void {
        c.luaL_addlstring(&self.b, str.ptr, str.len);
    }

    /// Adds to the buffer a string of `length` previously copied to the buffer area
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addsize
    pub fn lunaL_addsize(self: *Self, length: usize) void {
        // another function translate-c couldn't handle
        // c.luaL_addsize(&buf.b, length);
        var lua_buf = &self.b;
        lua_buf.n += length;
    }

    /// Adds the zero-terminated string pointed to by `str` to the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addstring
    pub fn lunaL_addstring(self: *Self, str: [:0]const u8) void {
        c.luaL_addstring(&self.b, str.ptr);
    }

    /// Adds the value on the top of the stack to the buffer
    /// Pops the value
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_addvalue
    pub fn lunaL_addvalue(self: *Self) void {
        c.luaL_addvalue(&self.b);
    }

    /// Returns a slice of the current content of the buffer
    /// Any changes to the buffer may invalidate this slice
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_buffaddr
    pub fn lunaL_buffaddr(self: *Self) []u8 {
        const length = self.b.n;
        return c.luaL_buffaddr(&self.b)[0..length];
    }

    /// Returns the length of the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_bufflen
    pub fn lunaL_bufflen(self: *Self) usize {
        return c.luaL_bufflen(&self.b);
    }

    /// Removes `num` bytes from the buffer
    /// TODO: perhaps error check?
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_buffsub
    pub fn lunaL_buffsub(self: *Self, num: usize) void {
        // Another bug with translate-c
        // c.luaL_buffsub(&buf.b, num);
        var lua_buf = &self.b;
        lua_buf.n -= num;
    }

    /// Equivalent to prepSize with a buffer size of Buffer.buffer_size
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_prepbuffer
    pub fn lunaL_prepbuffer(self: *Self) []u8 {
        return self.lunaL_prepbuffsize(buffer_size)[0..buffer_size];
    }

    /// Returns an address to a space of `size` where you can copy a string
    /// to be added to the buffer
    /// you must call `Buffer.addSize` to actually add it to the buffer
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_prepbuffsize
    pub fn lunaL_prepbuffsize(self: *Self, size: usize) []u8 {
        return c.luaL_prepbuffsize(&self.b, size)[0..size];
    }

    /// Finishes the use of the buffer leaving the final string on the top of the stack
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_pushresult
    pub fn lunaL_pushresult(self: *Self) void {
        c.luaL_pushresult(&self.b);
    }

    /// Equivalent to `Buffer.addSize()` followed by `Buffer.pushResult()`
    /// See https://www.lua.org/manual/5.4/manual.html#luaL_pushresultsize
    pub fn lunaL_pushresultsize(self: *Self, size: usize) void {
        c.luaL_pushresultsize(&self.b, size);
    }
};

pub const ZigFunction = fn (lua: *Luna) i32;
pub const ZigHook = fn (lua: *Luna, event: LuaEvent, info: *LuaDebugInfo) void;
pub const ZigKFunction = fn (lua: *Luna, status: LuaStatus, ctx: LuaKContext) i32;
pub const ZigReader = fn (lua: *Luna, data: *anyopaque) ?[]const u8;
pub const ZigWarnFunction = fn (data: ?*anyopaque, msg: []const u8, to_cont: bool) void;
pub const ZigWriter = fn (lua: *Luna, buf: []const u8, data: *anyopaque) bool;

fn TypeOfWrap(comptime T: type) type {
    return switch (T) {
        LuaState => Luna,
        ZigFunction => LuaCFunction,
        ZigHook => LuaHook,
        ZigKFunction => LuaKFunction,
        ZigReader => LuaReader,
        ZigWarnFunction => LuaWarnFunction,
        ZigWriter => LuaWriter,
        else => @compileError("unsupported type given to wrap: '" ++ @typeName(T) ++ "'"),
    };
}

/// Wraps the given value for use in the Lua API
/// Supports the following:
/// * `LuaState` => `Luna`
pub fn wrap(comptime value: anytype) TypeOfWrap(@TypeOf(value)) {
    const T = @TypeOf(value);
    return switch (T) {
        LuaState => Luna{ .state = value },
        ZigFunction => wrapCFunction(value),
        ZigHook => wrapZigHook(value),
        ZigKFunction => wrapZigKFunction(value),
        ZigReader => wrapZigReader(value),
        ZigWarnFunction => wrapZigWarnFunction(value),
        ZigWriter => wrapZigWriter(value),
        else => @compileError("unsupported type given to wrap: '" ++ @typeName(T) ++ "'"),
    };
}

fn wrapCFunction(comptime f: ZigFunction) LuaCFunction {
    return struct {
        fn inner(state: ?*LuaState) callconv(.C) c_int {
            // this is called by Lua, state should never be null
            var lua: Luna = .{ .state = state.? };
            return @call(.always_inline, f, .{&lua});
        }
    }.inner;
}

fn wrapZigHook(comptime f: ZigHook) LuaHook {
    return struct {
        fn inner(state: ?*LuaState, ar: ?*LuaDebug) callconv(.C) void {
            // this is called by Lua, state should never be null
            var lua: Luna = .{ .state = state.? };
            var info: LuaDebugInfo = .{
                .current_line = if (ar.?.currentline == -1) null else ar.?.currentline,
                .private = @ptrCast(ar.?.i_ci),
            };
            @call(.always_inline, f, .{ &lua, @as(LuaEvent, @enumFromInt(ar.?.event)), &info });
        }
    }.inner;
}

fn wrapZigKFunction(comptime f: ZigKFunction) LuaKFunction {
    return struct {
        fn inner(state: ?*LuaState, status: c_int, ctx: LuaKContext) callconv(.C) c_int {
            // this is called by Lua, state should never be null
            var lua: Luna = .{ .state = state.? };
            return @call(.always_inline, f, .{ &lua, @as(LuaStatus, @enumFromInt(status)), ctx });
        }
    }.inner;
}

fn wrapZigReader(comptime f: ZigReader) LuaReader {
    return struct {
        fn inner(state: ?*LuaState, data: ?*anyopaque, size: [*c]usize) callconv(.C) [*c]const u8 {
            var lua: Luna = .{ .state = state.? };
            if (@call(.always_inline, f, .{ &lua, data.? })) |buffer| {
                size.* = buffer.len;
                return buffer.ptr;
            } else {
                size.* = 0;
                return null;
            }
        }
    }.inner;
}

fn wrapZigWarnFunction(comptime f: ZigWarnFunction) LuaWarnFunction {
    return struct {
        fn inner(data: ?*anyopaque, msg: [*c]const u8, to_cont: c_int) callconv(.C) void {
            // warning messages emitted from Lua should be null-terminated for display
            const message = std.mem.span(@as([*:0]const u8, @ptrCast(msg)));
            @call(.always_inline, f, .{ data, message, to_cont != 0 });
        }
    }.inner;
}

fn wrapZigWriter(comptime f: ZigWriter) LuaWriter {
    return struct {
        fn inner(state: ?*LuaState, buf: ?*const anyopaque, size: usize, data: ?*anyopaque) callconv(.C) c_int {
            // this is called by Lua, state should never be null
            var lua: Luna = .{ .state = state.? };
            const buffer = @as([*]const u8, @ptrCast(buf))[0..size];
            const result = @call(.always_inline, f, .{ &lua, buffer, data.? });
            // it makes more sense for the inner writer function to return false for failure,
            // so negate the result here
            return @intFromBool(!result);
        }
    }.inner;
}

/// Export a Zig function to be used as a Zig (C) Module
pub fn zigopen(comptime name: []const u8, comptime func: ZigFunction) void {
    const declaration = wrap(func);
    @export(declaration, .{ .name = "luaopen_" ++ name, .linkage = .Strong });
}
