const std = @import("std");
const assert = std.debug.assert;

const c = @cImport({
    @cInclude("wslay.h");
});

pub const WslayEventContext = c.wslay_event_context;
pub const WslayEventCallBacks = c.wslay_event_callbacks;
pub const WslayFrameContext = c.wslay_frame_context;
pub const WslayFrameIocb = c.wslay_frame_iocb;
pub const WslayEventOnMsgRecvArg = c.wslay_event_on_msg_recv_arg;
pub const WslayEventOnFrameRecvStartArg = c.wslay_event_on_frame_recv_start_arg;
pub const WslayEventOnFrameRecvChunkArg = c.wslay_event_on_frame_recv_chunk_arg;
pub const WslayEventMsg = c.wslay_event_msg;
pub const WslayEventFragmentedMsg = c.wslay_event_fragmented_msg;

pub const WslayEventRecvCallBack = *const fn (ctx: ?*WslayFrameContext, buf: [*c]const u8, len: u32, flags: i32, user_data: ?*anyopaque) callconv(.C) c_long;

pub const WslayEventSendCallBack = *const fn (ctx: ?*WslayFrameContext, buf: [*c]const u8, len: u32, flags: i32, user_data: ?*anyopaque) callconv(.C) c_long;

pub const WslayEventGenMaskCallBack = *const fn (ctx: ?*WslayFrameContext, buf: [*c]const u8, len: u32, user_data: ?*anyopaque) callconv(.C) c_int;

pub const WslayEventOnFrameRecvStartCallBack = *const fn (ctx: ?*WslayFrameContext, arg: ?*WslayEventOnFrameRecvStartArg, user_data: ?*anyopaque) callconv(.C) void;

pub const WslayEventOnFrameRecvChunkCallBack = *const fn (ctx: ?*WslayFrameContext, arg: ?*WslayEventOnFrameRecvChunkArg, user_data: ?*anyopaque) callconv(.C) void;

pub const WslayEventOnFrameRecvEndCallBack = *const fn (ctx: ?*WslayFrameContext, user_data: ?*anyopaque) callconv(.C) void;

pub const WslayEventOnMsgRecvCallBack = *const fn (ctx: ?*WslayFrameContext, arg: ?*WslayEventOnMsgRecvArg, user_data: ?*anyopaque) callconv(.C) void;

// inline fn opaqueCast(comptime T: type, ptr: *anyopaque) *T {
//     return @ptrCast(@alignCast(ptr));
// }

pub const ZWslay = struct {
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(
        allocator: std.mem.Allocator,
        server: bool,
    ) !Self {
        var callbacks: WslayEventCallBacks = try allocator.create(WslayEventCallBacks);
        errdefer allocator.destroy(callbacks);

        // var ctx: WslayEventContext = try allocator.create(WslayEventContext);
        // errdefer allocator.destroy(ctx);

        if (server) {
            // c.wslay_event_context_server_init(ctx, callbacks);
        } else {}
        // assert(c.art_tree_init(&tree) == 0);
        // return .{
        //     .allocator = allocator,
        //     .tree = tree,
        // };
    }

    pub fn deinit(self: *Self) !void {
        _ = self;
        // assert(c.art_tree_destroy(self.tree) == 0);
        // self.allocator.destroy(self.tree);
    }
};
