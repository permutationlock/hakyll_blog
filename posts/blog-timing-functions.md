---
title: Timing functions
author: A.
published: September 16, 2023
tags: c, zig
git: https://github.com/permutationlock/thierd
---

I have had a litte C snippet that I've used for function micro-benchmarks since
I saw one of my professors ([Orion Lawlor][1]) use it in University.

```C
#include <time.h>

double time_fn(void (*f)(void)) {
    int nruns = 1;
    int msec = 0;
    while(msec < 1000) {
        nruns *= 2;

        clock_t start = clock(), diff;
        for(int i = 0; i < nruns; ++i) {
            f();
        }
        diff = clock() - start;

        msec = diff * 1000 / (CLOCKS_PER_SEC);
    }
    return ((double)msec * 1000 * 1000) / nruns;
}
```

I recently ported this code to Zig and decided to take another look at how
time functions using the OS API.

# Clocks

On Linux clocks are read with the syscall [`clock_gettime`][2], and most
operating systems provide a corresponding function of the same name. The
function takes a `clock_id` parameter indicating which system clock to read
from. For performance benchmarks, there are three types of clocks that we
generally care about.

 - System time: the "real" elapsed time, sometimes called "wall time." Measured
   by `CLOCK_REALTIME` or `CLOCK_BOOTTIME`.
 - Process time: the time elapsed while the CPU
   was executing the current process. Measured by `CLOCK_PROCESS_CPUTIME_ID`.
 - Thread time: the time elapsed while the CPU
   was executing the current thread. Measured by `CLOCK_THREAD_CPUTIME_ID`.

When benchmarking a function, in most cases you want to read process time.
It turns out that the libc `clock` function returns process time, and
thus the C snippet from above is a great micro-benchmark.

There are
cases where you might wish to track system time, e.g. you want to
include the time that a function spends blocking on a syscall.

**Note:**  Clock types are OS dependent
and all the info above comes from Linux. On POSIX systems things might seem
almost identical, but similarly named clocks can be meaningfully different;
make sure to read the clock descriptions.

# Porting `timeFn` to Zig

While implementing some data structures in [Zig][3], I wanted to port my
benchmark snippet over. I decided to take advantage of Zig's `comptime`
functionality to allow me to time functions with arbitrary arguments and return
types.

```Zig
const std = @import("std");

pub fn timeFn(
    clock_id: i32,
    comptime Fn: type,
    function: Fn,
    args: std.meta.ArgsTuple(Fn)
) !f64 {
    var nruns: u64 = 1;
    var delta: u64 = 0;
    while (delta < 1000 * 1000 * 1000) {
        nruns *= 2;
        var last: std.os.timespec = undefined;
        std.os.clock_gettime(clock_id, &last) catch unreachable;
        var i: u32 = 0;
        while (i < nruns) {
            switch (@typeInfo(@typeInfo(Fn).Fn.return_type.?)) {
                .ErrorUnion => {
                    _ = @call(.never_inline, f, args) catch |err| {
                        std.debug.panic(
                            "error while timing {s}",
                            .{@errorName(err)}
                        );
                    };
                },
                .Void => @call(.never_inline, f, args),
                else => _ = @call(.never_inline, f, args),
            }
            i += 1;
        }
        var current: std.os.timespec = undefined;
        std.os.clock_gettime(clock_id, &current) catch unreachable;
        const seconds = @as(
            u64,
            @intCast(current.tv_sec - last.tv_sec)
        );
        const elapsed = (seconds * 1000 * 1000 * 1000)
            + @as(u32, @intCast(current.tv_nsec))
            - @as(u32, @intCast(last.tv_nsec));
        delta = elapsed;
    }
    return @as(f64, @floatFromInt(delta))
        / @as(f64, @floatFromInt(nruns));
}
```

The Zig funciton above is very close to the original C, but instead of taking
a function pointer it takes a function type alongside a
function and corresponding tuple of arguments. It also directly
uses `clock_gettime` rather than going through libc, and the clock
to read is selected at call time.

I initally used `std.time.Instant`, but that only measures
system time not process CPU time.

# Timing my Zig hash maps

I wrote several data structures in Zig to learn the langauge, including some
item pools and hash maps. Of course, I wanted to do some micro-benchmarks to
test these out (and compare them against the great Zig standard library hash
map implementation).

```Zig
const std = @import("std");
const heap = std.heap;

const timeFn = @import("include/timing.zig").timeFn;

const data_structures = @import("include/data_structures.zig");
const ListHashMap = data_structures.ListHashMap;
const OpenHashMap = data_structures.OpenHashMap;

const Key = [32]u8;
const Data = [64]u8;

fn equals(k1: Key, k2: Key) callconv(.Inline) bool {
    return std.mem.eql(u8, &k1, &k2);
}

fn hash(key: Key) u64 {
    return std.hash.Wyhash.hash(0, &key);
}

const num_inserts = 1024;
const ListMap = ListHashMap(
    Key, Data, equals, hash, num_inserts, 4 * num_inserts
);
const OpenMap = OpenHashMap(Key, Data, equals, hash, num_inserts);
const StdMap = std.hash_map.AutoHashMap(Key, Data);

fn testMap(comptime M: type, map: *M, keys: []Key, rkeys: []Key) !f64 {
    const data = [4]u8{1, 2, 3, 4} ** (@sizeOf(Data) / 4);
    const Funcs = struct {
        fn insertDelete(m: *M, ks: []Key, d: Data, rks: []Key) void {
            for (ks) |k| {
                m.putAssumeCapacity(k, d);
            }
            for (rks) |rk| {
                _ = m.remove(rk);
            }
        }
    };

    return try timeFn(
        std.os.CLOCK.PROCESS_CPUTIME_ID,
        @TypeOf(Funcs.insertDelete),
        Funcs.insertDelete,
        .{ map, keys, data, rkeys }
    ) / @as(f64, @floatFromInt(keys.len));
}

pub fn main() !void {
    const stdout = std.io.getStdOut();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const nkeys = num_inserts;
    var keys = try allocator.create([nkeys]Key);
    for (keys) |*k| {
        try std.os.getrandom(std.mem.asBytes(k));
    }
    var rkeys = try allocator.create([nkeys]Key);
    var i: usize = 0;
    while (i < keys.len){
        rkeys[i] = keys[i];
        i += 1;
    }
    var is = std.rand.Isaac64.init(0);
    is.random().shuffle(Key, rkeys);

    var list_map = try allocator.create(ListMap);
    list_map.init();
    try stdout.writer().print(
        "list map: {}ns\n",
        .{ try testMap(ListMap, list_map, keys, rkeys) }
    );

    var open_map = try allocator.create(OpenMap);
    open_map.init();
    try stdout.writer().print(
        "open map: {}ns\n",
        .{ try testMap(OpenMap, open_map, keys, rkeys) }
    );

    var std_map = StdMap.init(allocator);
    try std_map.ensureTotalCapacity(num_inserts);
    try stdout.writer().print(
        "std map: {}ns\n",
        .{ try testMap(StdMap, &std_map, keys, rkeys) }
    );
}
```

The core thing to focus on here is the function `testMap` which takes a comptime
type `M`, a pointer to a map of type `M`, and two arrays of keys. The first
array is a list of keys to insert into the map, and
the second array is a shuffled copy of the first array.

Inside `testMap` we generate a
function at compile time to insert the keys into the given map, and
then remove them in the order given by the second array. The Zig way to create a
function at compile time inside the scope of a function is to create a struct
with the function as a member.

[1]: http://lawlor.cs.uaf.edu/~olawlor/
[2]: https://www.man7.org/linux/man-pages/man3/clock_gettime.3.html
[3]: https://github.com/ziglang/
