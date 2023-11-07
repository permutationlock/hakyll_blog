---
title: Timing functions
author: A.
published: September 16, 2023
tags: c, zig
git: https://github.com/permutationlock/thierd
---

I have had a litte C snippet that I've used for function micro-benchmarks ever
since I saw one of my professors ([Orion Lawlor][1]) use it in University.

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

I recently ported this code to Zig and decided to take another look at how to
time functions using the OS API.

# Clocks

On Linux clocks are read with the syscall [`clock_gettime`][2], and most
operating systems provide a corresponding function of the same name[^1]. The
function takes a `clock_id` parameter indicating which system clock to read
from. For performance benchmarks, there are three types of clocks that we
generally care about.

 - System time: the "real" elapsed time, sometimes called "wall time," measured
   by `CLOCK_REALTIME` or `CLOCK_BOOTTIME`.
 - Process time: the time elapsed while the CPU
   was executing the current process, measured by `CLOCK_PROCESS_CPUTIME_ID`.
 - Thread time: the time elapsed while the CPU
   was executing the current thread, measured by `CLOCK_THREAD_CPUTIME_ID`.

When benchmarking a function, in most cases you want to read process time.
It turns out that the libc `clock` function returns process time, and
thus the C snippet from above is a great micro-benchmark.

There are
cases where you might wish to track system time, e.g. you want to
include the time that a function spends blocking on a syscall.

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

[1]: http://lawlor.cs.uaf.edu/~olawlor/
[2]: https://www.man7.org/linux/man-pages/man3/clock_gettime.3.html
[3]: https://github.com/ziglang/

[^1]: Clock types are OS dependent and similarly named clocks can be meaningfully different.
    Make sure to read the clock descriptions for the system that you are targeting.
