---
title: In defense of Zig's anytype
author: A.
published: November 1, 2023
tags: zig
git: https://github.com/permutationlock/zig_type_traits
---

Generics in Zig often get a bad wrap, particularly `anytype`. While I love a
good robust type system like those found in Haskell or Rust, I have
come to really appreciate the simplicity of Zig generics.

This article will cover a couple common patterns for working with `anytype`
parameters, and then introduce my [trait library][1] as a potential
strategy for managing complicated generic types.

## An example situation

Suppose that we are writing a networking library and have created a
`Server` struct
that will accept and manage TCP connections. We would like `Server`
to exposes a `poll` function that will poll for events on the
server's internal collection of sockets.

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

    fn receive(self: *Self, handle: Handle) Event {
        // ...
    }
    
    fn pollInternal(self: *Self) []const Handle {
        // ...
    }

    pub fn listen(self: *Self, port: u16) void {
        // ...
    }

    // Our job is to write: pub fn poll(self: *Self, ...) void { ... }
}
```

Below I'll go over a few different ways that we could write a
`Server.poll` function that accepts a generic `handler` parameter
to respond to reported events.

**Note:** There are other ways to tackle the problem, but for this article I am
only interested in compile-time generics.

## Option 1: use duck typing

Zig allows generic functions to take `anytype` parameters, generating
a new concrete function at compile-time for each different parameter
type used. Compile time [duck typing][2] is used to verify that the passed
parameters have the required declarations and fields.

**Note:** In general, it is considered good practice to avoid directly accessing
the fields of generic parameters. Functions in this article will never
expect a generic type to have any specific fields.

A `poll` function for our `Server` struct using `anytype` is provided below.

```Zig
pub const Server = struct {
    // ... 

    pub fn poll(self: *Self, handler: anytype) void {
        const updated_handles = self.pollInternal();
        for (updated_handles) |handle| {
            const event = self.receive(handle);
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
is that it is not immediately clear
from the function signature of `poll` what can be passed in for the
`handler` parameter.
Someone using our library would need to read the function body of `poll`,
track down the return type of `Server.receive`, and then find the definition of
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

One way to provide clarity is to use `anytype` but never rely on duck typing.
Everytime you would have relied on the existence of a type declaration,
instead require the caller to pass an additional comptime parameter.

```Zig
// ...

pub fn poll(
    self: *Self,
    handler: anytype, 
    comptime onOpen: fn (@TypeOf(handler), Handle) void,
    comptime onMessage: fn (@TypeOf(handler), Handle, Message) void,
    comptime onClose: fn (@TypeOf(handler), Handle) void
) void {
    const updated_handles = self.pollInternal();
    for (updated_handles) |handle| {
        const event = self.receive(handle);
        switch (event) {
            .open => onOpen(handler, handle),
            .msg => |msg| onMessage(handler, handle, msg),
            .close => onClose(handler, handle),
        }
    }
}

// ...
```

While this does make type requirements explicit, it also makes calling
`Server.poll` a bit more verbose.

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

A massive upside to this style of generics is that the functions are no
longer tied directly to a type definition. This gives the library a lot of
flexibility, and also lets the generic function work with types that
don't support declarations, e.g. numeric types.

## Option 3: use custom type checking

In this section I will assume that you have at least skimmed the readme of
my [Zig type trait library][1].

Let us revisit the duck typing example, but this time add our own
explicit type checking using traits. We'll first define a `Handler` trait as
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

    const updated_handles = self.pollInternal();
    for (updated_handles) |handle| {
        const event = self.receive(handle);
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

**Note:** My trait library is obviously only one possible way to do
explicit type checking. You could do it from basics with `if`
and `switch` statements and the `@typeInfo` builtin, or define your
own conventions and helper functions.

### Going a step further with interfaces

Explicit type checking relies on the library writer to
ensure that checks are up to date with how generic parameters
are actually used. My trait library provides the comptime function
`Interface` to generate an interface that only provides
access to the declarations of a type that match a given set of traits.

A version of `Server.poll` using interfaces is provided below.

```Zig
// ...

pub fn poll(self: *Self, handler: anytype) void {
    const HandlerIfc = Interface(PointerChild(@TypeOf(handler)), Handler);

    const updated_handles = self.pollInternal();
    for (updated_handles) |handle| {
        const event = self.receive(handle);
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
