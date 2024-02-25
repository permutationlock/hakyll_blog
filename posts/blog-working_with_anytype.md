---
title: Working with Zig's anytype
author: A.
published: November 1, 2023
tags: zig
git: https://github.com/permutationlock/server_trait_example
---

Generics in Zig often get a bad wrap, particularly the `anytype` keyword.
Personally, while I enjoy working with the
robust type systems found in languages like Haskell and Rust, I have come
to really appreciate Zig's "`type` as a first class type" approach to generics.

This article will cover common patterns for working with `anytype`
parameters and document my adventure implementing a generic trait library.

## A situation

Suppose that we are writing a networking library and have created a
`Server` struct
that will accept and manage TCP connections. We would like `Server`
to exposes a `poll` function that will poll for events on the
`Server`'s internal collection of sockets.

There are three possible events that we want to handle: client connection,
message received, and client disconnection.

```Zig
pub const Server = struct {
    // ... 

    pub const Handle = u32;

    const Event = union(enum) {
        open: Handle,
        msg: struct { handle: Handle, bytes: []const u8, },
        close: Handle,
    };

    fn pollSockets(self: *Self) !void {
        // poll tracked sockets for any events
    }

    fn getEvent(self: *Self) ?Event {
        // get the next available event from the last call to pollSockets
    }

    pub fn listen(self: *Self, port: u16) !void {
        // begin listening for connections
    }

    // ...
}
```

Below I'll go over a few different ways that we could write a
`Server.poll` function that accepts a generic `handler` parameter
to respond to reported events. There are other ways to tackle the problem,
but for this article I am only interested in [static dispatch][6] with
compile time generics.

## Option 1: use duck typing

Zig allows functions to take `anytype` parameters. A
concrete version of the function will be generated for each different parameter
type that the generic function is called with.

At compile time, [duck typing][2] is
used to verify that each parameter type has the required fields and
declarations[^4].
It is not usually good practice to directly access fields on generic
parameters, so for the purposes of this article we will consider it
forbidden to do so.

A `poll` function for our `Server` struct using `anytype` is provided below.

```Zig
pub const Server = struct {
    // ... 

    pub fn poll(self: *Self, handler: anytype) !void {
        try self.pollSockets();
        while (self.getEvent()) |evt| {
            switch (evt) {
                .open => |handle| handler.onOpen(handle),
                .msg => |msg| handler.onMessage(msg.handle, msg.bytes),
                .close => |handle| handler.onClose(handle),
            }
        }
    }

    // ...
}
```

The biggest downside with duck typing
is that it will not be immediately clear
from the function signature of `poll` what is allowed to be passed
in for the `handler` parameter.
Someone using our server library would need to read the function body of `poll`,
track down the return type of `Server.getEvent`, and then find the definition of
`Server.Event`.

Below is a simple example server using a `handler` to log events.

```Zig
const std = @import("std");
const log = std.log.scoped(.log_server);

const my_server_lib = @import("my_server_lib");
const Server = my_server_lib.Server;
const Handle = Server.Handle;

pub const LogHandler = struct {
    count: usize = 0,

    pub fn onOpen(_: *Self, handle: Handle) void {
        log.info("connection {} opened", .{ handle });
    }

    pub fn onMessage(self: *Self, handle: Handle, msg: []const u8) void {
        log.info("{d}: client {d} sent '{s}'", .{ self.count, handle, msg });
        self.count += 1;
    }

    pub fn onClose(_: *Self, handle: Handle) void {
        log.info("connection {} closed", .{ handle });
    }
};

pub fn main() void {
    var server = Server{};
    var handler = LogHandler{};
    try server.listen(port);
    while (true) {
        try server.poll(&handler);
    }
}
```

## Option 2: use explicit function parameters 

A convention that will always provide clear requirements for
`anytype` parameters is to simply never rely on duck typing.
Everywhere that you would usually rely on the existence of a type declaration,
instead require the caller to pass an additional `comptime` parameter.

```Zig
// ...

pub fn poll(
    self: *Self,
    handler: anytype, 
    comptime onOpen: fn (@TypeOf(handler), Handle) void,
    comptime onMessage: fn (@TypeOf(handler), Handle, Message) void,
    comptime onClose: fn (@TypeOf(handler), Handle) void
) void {
    try self.pollSockets();
    while (self.getEvent()) |evt| {
        switch (evt) {
            .open => |handle| onOpen(handler, handle),
            .msg => |msg| onMessage(handler, msg.handle, msg.bytes),
            .close => |handle| onClose(handler, handle),
        }
    }
}

// ...
```

Unfortunately, while this does eliminate the issues with duck typing, it also
makes calling `Server.poll` quite a bit more verbose.

```Zig
// ...

var server = Server{};
var handler = LogHandler{};
try server.listen(port);
while (true) {
    try server.poll(
        &handler,
        LogHandler.onOpen,
        LogHandler.onMessage,
        LogHandler.onClose
    );
}

// ...
```

However, a second massive upside to passing each function explicitly is that
they can be defined separately from the type of the parameter. This provides
a lot of flexibility, and even allows for types that
don't support declarations, e.g. numeric types.

## Option 3: use traits for custom type checking

I created the [ztrait library][1] to explore whether it was possible to
implement Rust-style type traits in Zig.
Let's revisit the duck typing example in *Option 1*, but this time add our own
explicit type checking using traits.
I'll provide an explanation for each part of [ztrait][3] that we make use of,
but you may still want skim the [ztrait readme][3].

In [ztrait][1] a trait is 
function that takes a `type` and returns a `struct` containing only
`type` valued declarations[^1]. A type `Type` implements a trait `Trait` if 
for each declaration `Trait(Type).decl`, `Type.decl` exists
and `@TypeOf(Type.decl)` equals `Trait(Type).decl`.

We can define a `Handler` trait for the type of our `handler` parameter as follows.

```Zig
pub fn Handler(comptime Type: type) type {
    return struct {
        pub const onOpen = fn (*Type, Handle) void;
        pub const onMessage = fn (*Type, Handle, []const u8) void;
        pub const onClose = fn (*Type, Handle) void;
    };
}
```

Then we can simply add a trait verification line at the top of `Server.poll`.

```Zig
// ...

pub fn poll(self: *Self, handler: anytype) void {
    comptime where(PointerChild(@TypeOf(handler)), implements(Handler));

    try self.pollSockets();
    while (self.getEvent()) |evt| {
        switch (evt) {
            .open => |handle| handler.onOpen(handle),
            .msg => |msg| handler.onMessage(msg.handle, msg.bytes),
            .close => |handle| handler.onClose(handle),
        }
    }
}

// ...
```

If the reader is familiar with the trait convention, the type requirements are
now immediately clear: the type of `handler` must be a single item pointer to
a type that has the declarations defined by the `Handler` trait[^3].

The type requirements are still not in the function signature itself, or
easily accessible from an LSP, but traits provide clear and formal type
documentation.

## Option 4: use a comptime constructed interface

Trait verification relies on the library writer to
ensure that the traits are up to date with how generic parameters
are actually used. The [ztrait library][1] exposes the `interface`
function to generate an interface that only provides
access to the declarations of a type that match a given trait.

A version of `Server.poll` that constructs an interface for the `handler`
parameter is provided below. 

```Zig
// ...

pub fn poll(self: *Self, handler: anytype) void {
    const handler_ifc = interface(PointerChild(@TypeOf(handler)), Handler);

    try self.pollSockets();
    while (self.getEvent()) |evt| {
        switch (evt) {
            .open => |handle| handler_ifc.onOpen(handler, handle),
            .msg => |msg| handler_ifc.onMessage(handler, msg.handle, msg.bytes),
            .close => |handle| handler_ifc.onClose(handler, handle),
        }
    }
}

// ...
```

So long as the `handler` parameter is only used with the functions of
`handler_ifc`, the `Handler` trait is guaranteed to
define "necessary and sufficient" conditions for the type of `handler`.

## Option 5: take an interface as a separate parameter

Another way to use interfaces is take an interface struct as an explicit parameter
along the lines of *Option 2*. To understand how this works, we'll first need to
take a look inside how the `interface` function works[^5].

```Zig
pub fn interface(
    comptime Type: type,
    comptime Trait: fn (type) type
) Interface(Type, Trait) {
    comptime where(Type, implements(Trait));
    return .{};
}
```

A call to `interface(Type, Trait)` returns a default constructed struct
of  type `Interface(Type, Trait)`. The struct type contains one field
for each declaration of `Trait(Type)` with default value equal to the corresponding
declaration of `Type` if it exists and matches the type signature.
The call to `where` ensures that each field has a default value, and thus a
default instance can be constructed and returned.

We can instead decide to use `Interface(Type, Trait)` directly as the
type of a parameter[^2].

```Zig
// ...

pub fn poll(
    self: *Self,
    handler: anytype,
    handler_ifc: Interface(PointerChild(@TypeOf(handler)), Handler)
) void {
    try self.pollSockets();
    while (self.getEvent()) |evt| {
        switch (evt) {
            .open => |handle| handler_ifc.onOpen(handler, handle),
            .msg => |msg| handler_ifc.onMessage(handler, msg.handle, msg.bytes),
            .close => |handle| handler_ifc.onClose(handler, handle),
        }
    }
}

// ...
```

Calling `Server.poll` with our defined `LogHandler` type would now look like the
code below.


```Zig
// ...

var server = Server{};
var handler = LogHandler{};
try server.listen(port);
while (true) {
    try server.poll(&handler, .{});
}

// ...
```

In this way we allow the caller the
to define a custom interface at each call site, but
keep calls concise when implementations can be inferred from declarations of
the parameter type.

```Zig
server.poll(&handler, .{ onOpen = otherOnOpen });
```

We also now have type requirements directly in the function signature.

## Conclusion: `zimpl` is better

I believe that *Option 2* and *Option 5* are by far the best conventions to use in
most situations.

 - Both are easy to implement and understand (see [zimpl][5] below for a simple
   version of *Option 5*).
 - Both clearly define type requirements directly in the function
   signature.
 - Both closely resemble other common Zig patterns.
 - Both let the caller define a specific implementation at each
   call site.
 - Both generate errors at call sites, not in function
   signatures. As of `zig-0.12-dev` error messages
   originating
   from a generic function signature are usually poor.

For all the above reasons, I decided to make [zimpl][5]. The [zimple][5]
library is a tiny subset  of [ztrait][1] containing ~20 lines of code and
the one public declaration `zimpl.Impl` that is a simplified
`ztrait.Interface`.

A version of `Server.poll` using `zimpl` would go as follows.

```Zig
pub fn Handler(comptime Type: type) type {
    return struct {
        onOpen: fn (Type, Handle) void,
        onMessage: fn (Type, Handle, []const u8) void,
        onClose: fn (Type, Handle) void,
    };
}

pub fn poll(self: *Self, ctx: anytype, impl: Impl(Handler, @TypeOf(ctx))) void {
    try self.pollSockets();
    while (self.getEvent()) |evt| {
        switch (evt) {
            .open => |handle| impl.onOpen(ctx, handle),
            .msg => |msg| impl.onMessage(ctx, msg.handle, msg.bytes),
            .close => |handle| impl.onClose(ctx, handle),
        }
    }
}
```

The above version of `poll` is called in the same way as the
version of `poll` we wrote for *Option 5*.

### Source code

A full working implementation of every example found in the article is available on
[GitHub][4].

[1]: https://github.com/permutationlock/zig_type_traits
[2]: https://en.wikipedia.org/wiki/Duck_typing
[3]: https://github.com/permutationlock/zig_type_traits/blob/main/README.md
[4]: https://github.com/permutationlock/server_trait_example
[5]: https://github.com/permutationlock/zimpl
[6]: https://en.wikipedia.org/wiki/Static_dispatch

[^4]: In Zig a variable or function declared in the namespace
    of a type is called a declaration of that type.
[^2]: Using the `Interface` type directly as a parameter type does not work for traits
    with sub-traits or declarations that depend on one another.
[^3]: The `PointerChild` helper function verifies that a type is a single item
    pointer and then returns the child type. E.g. `PointerChild(*u8)` is `u8`, while
    `PointerChild(u8)` and `PointerChild([]u8)` will be compile errors.
[^5]: In reality both `interface` and `Interface` can also take a tuple of
    traits, but this simplified implementation covers the most common use case.
[^1]: More complicated traits with sub-traits exist as well, but
    they are beyond the scope of this article.
