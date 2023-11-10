const std = @import("std");

const assert = std.debug.assert;
const fmt = std.fmt;

const c = @cImport({
    @cInclude("picohttpparser.h");
});

pub const PicoHeader = struct {
    name: []const u8,
    value: []const u8,

    const Self = @This();

    pub fn isMultiline(self: *Self) bool {
        return @intFromPtr(self.name.ptr) == 0;
    }

    pub fn format(self: *Self, comptime layout: []const u8, opts: fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = layout;
        if (self.isMultiline()) {
            try fmt.format(writer, "{s}", .{self.value});
        } else {
            try fmt.format(writer, "{s}: {s}", .{ self.name, self.value });
        }
    }

    comptime {
        assert(@sizeOf(PicoHeader) == @sizeOf(c.phr_header));
        assert(@alignOf(PicoHeader) == @alignOf(c.phr_header));
    }
};

pub const PicoRequest = struct {
    method: []const u8,
    path: []const u8,
    minor_version: usize,
    headers: []const PicoHeader,

    const Self = @This();

    pub fn format(self: *Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        try fmt.format(writer, "{s} {s}\n", .{ self.method, self.path });
        for (self.headers) |header| {
            _ = try writer.write("\t");
            try fmt.format(writer, "{s}\n", .{header});
        }
    }

    pub fn parse(buf: []const u8, src: []PicoHeader) !Self {
        var method: []const u8 = undefined;
        var path: []const u8 = undefined;
        var minor_version: c_int = undefined;
        var num_headers: usize = src.len;

        const rc = c.phr_parse_request(
            buf.ptr,
            buf.len,
            @as([*c][*c]const u8, @ptrCast(&method.ptr)),
            &method.len,
            @as([*c][*c]const u8, @ptrCast(&path.ptr)),
            &path.len,
            &minor_version,
            @as([*c]c.phr_header, @ptrCast(src.ptr)),
            &num_headers,
            0,
        );

        // Leave a sentinel value, for JavaScriptCore support.
        if (rc > -1) @as([*]u8, @ptrFromInt(@intFromPtr(path.ptr)))[path.len] = 0;

        return switch (rc) {
            -1 => error.BadRequest,
            -2 => error.ShortRead,
            else => .{
                .method = method,
                .path = path,
                .minor_version = @as(usize, @intCast(minor_version)),
                .headers = src[0..num_headers],
            },
        };
    }
};

test "pico_http: parse request" {
    const REQ = "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n" ++
        "Host: www.kittyhell.com\r\n" ++
        "User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 " ++
        "Pathtraq/0.9\r\n" ++
        "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
        "Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n" ++
        "Accept-Encoding: gzip,deflate\r\n" ++
        "Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n" ++
        "Keep-Alive: 115\r\n" ++
        "Connection: keep-alive\r\n" ++
        "TestMultiline: Hello world\r\n" ++
        "   This is a second line in the header!\r\n" ++
        "Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; " ++
        "__utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; " ++
        "__utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n" ++
        "\r\n";

    var headers: [32]PicoHeader = undefined;

    const req = try PicoRequest.parse(REQ, &headers);

    std.debug.print("Method: {s}\n", .{req.method});
    std.debug.print("Path: {s}\n", .{req.path});
    std.debug.print("Minor Version: {}\n", .{req.minor_version});

    for (req.headers) |header| {
        std.debug.print("{}\n", .{header});
    }
}

pub const PicoResponse = struct {
    minor_version: usize,
    status_code: usize,
    status: []const u8,
    headers: []PicoHeader,
    bytes_read: c_int = 0,

    const Self = @This();

    pub fn format(self: *Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        try fmt.format(writer, "< {d} {s}\n", .{ self.status_code, self.status });
        for (self.headers) |header| {
            _ = try writer.write("< \t");
            try fmt.format(writer, "{s}\n", .{header});
        }
    }

    pub fn parseParts(buf: []const u8, src: []PicoHeader, offset: ?*usize) !Self {
        var minor_version: c_int = 1;
        var status_code: c_int = 0;
        var status: []const u8 = "";
        var num_headers: usize = src.len;

        const rc = c.phr_parse_response(
            buf.ptr,
            buf.len,
            &minor_version,
            &status_code,
            @as([*c][*c]const u8, @ptrCast(&status.ptr)),
            &status.len,
            @as([*c]c.phr_header, @ptrCast(src.ptr)),
            &num_headers,
            offset.?.*,
        );

        return switch (rc) {
            -1 => error.BadResponse,
            -2 => brk: {
                offset.?.* += buf.len;

                break :brk error.ShortRead;
            },
            else => .{
                .minor_version = @as(usize, @intCast(minor_version)),
                .status_code = @as(usize, @intCast(status_code)),
                .status = status,
                .headers = src[0..@min(num_headers, src.len)],
                .bytes_read = rc,
            },
        };
    }

    pub fn parse(buf: []const u8, src: []PicoHeader) !Self {
        var offset: usize = 0;
        const response = try parseParts(buf, src, &offset);
        return response;
    }
};

test "pico_http: parse response" {
    const RES = "HTTP/1.1 200 OK\r\n" ++
        "Date: Mon, 22 Mar 2021 08:15:54 GMT\r\n" ++
        "Content-Type: text/html; charset=utf-8\r\n" ++
        "Content-Length: 9593\r\n" ++
        "Connection: keep-alive\r\n" ++
        "Server: gunicorn/19.9.0\r\n" ++
        "Access-Control-Allow-Origin: *\r\n" ++
        "Access-Control-Allow-Credentials: true\r\n" ++
        "\r\n";

    var headers: [32]PicoHeader = undefined;

    const res = try PicoResponse.parse(RES, &headers);

    std.debug.print("Minor Version: {}\n", .{res.minor_version});
    std.debug.print("Status Code: {}\n", .{res.status_code});
    std.debug.print("Status: {s}\n", .{res.status});

    for (res.headers) |header| {
        std.debug.print("{}\n", .{header});
    }
}

pub const PicoHeaders = struct {
    headers: []const PicoHeader,

    const Self = @This();

    pub fn format(self: *Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        for (self.headers) |header| {
            try fmt.format(writer, "{s}: {s}\r\n", .{ header.name, header.value });
        }
    }

    pub fn parse(buf: []const u8, src: []PicoHeader) !Self {
        var num_headers: usize = src.len;

        const rc = c.phr_parse_headers(
            buf.ptr,
            buf.len,
            @as([*c]c.phr_header, @ptrCast(src.ptr)),
            @as([*c]usize, @ptrCast(&num_headers)),
            0,
        );

        return switch (rc) {
            -1 => error.BadHeaders,
            -2 => error.ShortRead,
            else => .{
                .headers = src[0..num_headers],
            },
        };
    }
};

test "pico_http: parse headers" {
    const HEADERS = "Date: Mon, 22 Mar 2021 08:15:54 GMT\r\n" ++
        "Content-Type: text/html; charset=utf-8\r\n" ++
        "Content-Length: 9593\r\n" ++
        "Connection: keep-alive\r\n" ++
        "Server: gunicorn/19.9.0\r\n" ++
        "Access-Control-Allow-Origin: *\r\n" ++
        "Access-Control-Allow-Credentials: true\r\n" ++
        "\r\n";

    var headers: [32]PicoHeader = undefined;

    const result = try PicoHeaders.parse(HEADERS, &headers);
    for (result.headers) |header| {
        std.debug.print("{}\n", .{header});
    }
}
