const std = @import("std");
const assert = std.debug.assert;

const c = @cImport({
    @cInclude("art.h");
});

pub const ArtTree = c.art_tree;
pub const ArtLeaf = c.art_leaf;
pub const ArtCallBack = *const fn (data: ?*anyopaque, key: [*c]const u8, key_len: u32, value: ?*anyopaque) callconv(.C) c_int;

inline fn opaqueCast(comptime T: type, ptr: *anyopaque) *T {
    return @ptrCast(@alignCast(ptr));
}

pub const PicoArt = struct {
    allocator: std.mem.Allocator,
    tree: ?*ArtTree = null,

    const Self = @This();
    // const alignment = @alignOf(std.c.max_align_t);

    pub fn init(allocator: std.mem.Allocator) !Self {
        var tree: ArtTree = try allocator.create(ArtTree);
        errdefer allocator.destroy(tree);
        assert(c.art_tree_init(&tree) == 0);
        return .{
            .allocator = allocator,
            .tree = tree,
        };
    }

    pub fn deinit(self: *Self) !void {
        assert(c.art_tree_destroy(self.tree) == 0);
        self.allocator.destroy(self.tree);
    }

    pub fn size(self: *Self) u64 {
        return c.art_size(self.tree);
    }

    pub fn insert(self: *Self, key: [*c]const u8, key_len: u32, value: ?*anyopaque) *anyopaque {
        return c.art_insert(self.tree, key, key_len, value);
    }

    pub fn insert_no_replace(self: *Self, key: [*c]const u8, key_len: u32, value: ?*anyopaque) *anyopaque {
        return c.art_insert_no_replace(self.tree, key, key_len, value);
    }

    pub fn delete(self: *Self, key: [*c]const u8, key_len: u32) *anyopaque {
        return c.art_delete(self.tree, key, key_len);
    }

    pub fn search(self: *Self, key: [*c]const u8, key_len: u32) *anyopaque {
        return c.art_search(self.tree, key, key_len);
    }

    pub fn iter(self: *Self, cb: ?ArtCallBack, value: ?*anyopaque) i32 {
        return c.art_iter(self.tree, cb, value);
    }

    pub fn iter_prefix(self: *Self, prefix: [*c]const u8, prefix_len: u32, cb: ?ArtCallBack, value: ?*anyopaque) i32 {
        return c.art_iter_prefix(self.tree, prefix, prefix_len, cb, value);
    }

    pub fn minimum(self: *Self) *ArtLeaf {
        return c.art_minimum(self.tree);
    }

    pub fn maximum(self: *Self) *ArtLeaf {
        return c.art_maximum(self.tree);
    }
};
