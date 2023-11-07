---
title: In defense of Zig's anytype
author: A.
published: November 1, 2023
tags: zig
git: https://github.com/permutationlock/zig_type_traits
---

Generics in Zig often get a bad wrap, particularly the `anytype` keyword.
Personally, while I also love working with the
robust type systems found in languages like Haskell and Rust, I have come
to really appreciate Zig's "`type` as a first class type" approach to generics.

This article will cover a couple common patterns for working with `anytype`
parameters, and introduce my [trait library][1] as an option
for managing complicated generic types.

## An example situation

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
        open: void,
        event: bytes: []const u8,
        close: void,
    };

    fn recv(self: *Self, handle: Handle) Event { // ... } 
    fn pollHandles(self: *Self) []const Handle { // ... }

    pub fn listen(self: *Self, port: u16) void { // ... }

    // ...
}
```

Below I'll go over a few different ways that we could write a
`Server.poll` function that accepts a generic `handler` parameter
to respond to reported events[^1].

## Option 1: use duck typing

Zig allows functions to take `anytype` parameters. At compile time, a
concrete version of the function will be generated for each different parameter
type that the generic function is called with.

At compile time, [duck typing][2] is
used to verify that each parameter type has the required fields and
declarations[^4].
In general, it is not good practice to directly access fields on generic
parameters, so for the purposes of this article we will consider it
forbidden to do so.

A `poll` function for our `Server` struct using `anytype` is provided below.

```Zig
pub const Server = struct {
    // ... 

    pub fn poll(self: *Self, handler: anytype) void {
        const updated_handles = self.pollHandles();
        for (updated_handles) |handle| {
            const event = self.recv(handle);
            switch (event) {
                .open => handler.onOpen(handle),
                .msg => |msg| handler.onMessage(handle, msg),
                .close => handler.onClose(handle),
            }
        }
    }

    // ...
}
```

The biggest downside of duck typing
is that it will not be immediately clear
from the function signature of `poll` what is allowed to be passed
in for the `handler` parameter.
Someone using our library would need to read the function body of `poll`,
track down the return type of `Server.recv`, and then find the definition of
`Server.Event`.

Below is a simple example server using a `handler` to logs events.

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
    server.listen(port);
    while (true) {
        server.poll(&handler);
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
    const updated_handles = self.pollHandles();
    for (updated_handles) |handle| {
        const event = self.recv(handle);
        switch (event) {
            .open => onOpen(handler, handle),
            .msg => |msg| onMessage(handler, handle, msg),
            .close => onClose(handler, handle),
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
server.listen(port);
while (true) {
    server.poll(
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

## Option 3: use custom type checking

In this section I will assume that you have at least skimmed the readme of
my [Zig type trait library][1].

Let us revisit the duck typing example, but this time add our own
explicit type checking using traits[^3]. We'll first define a `Handler` trait as
follows.

```Zig
pub fn Handler(comptime Type: type) type {
    return struct {
        pub const onOpen = fn (*Type, Handle) void;
        pub const onMessage = fn (*Type, Handle, []const u8) void;
        pub const onClose = fn (*Type, Handle) void;
    };
}
```

Then we simply add a trait verification line at the top of `Server.poll`.

```Zig
// ...

pub fn poll(self: *Self, handler: anytype) void {
    comptime where(PointerChild(@TypeOf(handler)), implements(Handler));

    const updated_handles = self.pollHandles();
    for (updated_handles) |handle| {
        const event = self.recv(handle);
        switch (event) {
            .open => handler.onOpen(handle),
            .msg => |msg| handler.onMessage(handle, msg),
            .close => handler.onClose(handle),
        }
    }
}

// ...
```

If the reader is familiar with the trait convention, the type requirements are
now immediately clear: `handler` must be a single item pointer to an instance of
a type that implements the `Handler` trait.
Trait verification also
produces a nice compile error if the type of `handler` is invalid.

### Going a step further with interfaces

Explicit type checking relies on the library writer to
ensure that checks are up to date with how generic parameters
are actually used. My trait library provides the `comptime` function
`Interface` to generate an interface that only provides
access to the declarations of a type that match a given set of traits.

A version of `Server.poll` using interfaces is provided below.

```Zig
// ...

pub fn poll(self: *Self, handler: anytype) void {
    const HandlerIfc = Interface(PointerChild(@TypeOf(handler)), Handler);

    const updated_handles = self.pollHandles();
    for (updated_handles) |handle| {
        const event = self.recv(handle);
        switch (event) {
            .open => HandlerIfc.onOpen(handler, handle),
            .msg => |msg| HandlerIfc.onMessage(handler, handle, msg),
            .close => HandlerIfc.onClose(handler, handle),
        }
    }
}

// ...
```

So long as the `handler` parameter is only used with the functions of
`HandlerIfc`, the `Handler` trait is guaranteed to
define "necessary and sufficient" conditions for the type of `handler`.

[1]: https://github.com/permutationlock/zig_type_traits
[2]: https://en.wikipedia.org/wiki/Duck_typing

[^1]: There are other ways to tackle the problem, but for this article I am only interested in compile-time generics.
[^3]: There are many other ways to do explicit type checking if you aren't
    interested in my library.
[^4]: In Zig a variable or function declared in the namespace
    of a type is called a declaration of that type.
