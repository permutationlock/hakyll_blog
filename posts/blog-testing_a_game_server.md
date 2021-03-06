---
title: Blog - testing a game server
author: A.
published: December 18, 2020
tags: blog, software
---

As alluded to in a few of my previous posts, I have been writing a C++ game
server library, which is
[available on GitHub](https://github.com/permutationlock/jwt_game_server).
The goal of this library is to provide a framework to easily develop
[JWT](https://jwt.io/) authenticated WebSocket backends for JavaScript games.

I'm familiar with the tenets of unit testing philosophy (if you're
interested, my favorite
overview is
[The Science of Unit Testing](https://www.youtube.com/watch?v=FjwayiHNI1w) by
Dave Steffen at CPP Con 2020), but testing networked code is always a tricky
task. Ideally one could construct a mock interface that mimics the network
interface and allows simple encapsulated unit testing just as with any other
function, but this can take a lot of work and most networking libraries are
not set up with this in mind. Thus, it often comes down to spinning up
asynchronous servers and clients and testing code through the local network
interface. This post discusses how I tested my library which uses the
[WebSocket++ library](https://github.com/zaphoyd/websocketpp) for networking.

For testing I went with the [doctest](https://github.com/onqtam/doctest)
library: it is very simple, lightweight, fast, popular, and I liked syntax
better than other libraries such as
[catch2](https://github.com/catchorg/Catch2),
[boost](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html),
etc.

All code discussed below can be found in the
[testing folder](https://github.com/permutationlock/jwt_game_server/tree/main/test/src)
of the git repo.

The first challenge I tackled was logging and exceptions. As the server is
designed to run continuously in production, errors are spit out in logs.
I use the [spdlog library](https://github.com/gabime/spdlog)
for logging, and it allows setting up various sinks for logs. Therefore in
doctest cases I simply setup a
[**std::ostringstream**](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
as a logging sync.

```C++
std::ostringstream oss;
auto ostream_sink = std::make_shared<spdlog::sinks::ostream_sink_mt> (oss);
auto logger = std::make_shared<spdlog::logger>("my_logger", ostream_sink);
spdlog::set_default_logger(logger);
spdlog::set_level(spdlog::level::err);
```

This could then be checked by doctest to verify any tests went off without
errors.

```C++
CHECK(oss.str().empty());
```

Obviously this is quite a rough hack, but errors appearing in logs are a very
infrequent occurrence and not a main focus of testing; we really want to test
that the server behaves correctly.

In order to do this I wrote a simple client class
[**jwt_game_server::base_client**](https://github.com/permutationlock/jwt_game_server/blob/main/include/jwt_game_server/base_client.hpp)
which I could use to spin up fake clients. In order to allow several servers to
be served on the same ports in quick succession I utilized the SO_REUSEADDR
flag, accessed via the websocketpp library's
[**endpoint.set_reuse_addr()**](https://docs.websocketpp.org/classwebsocketpp_1_1transport_1_1asio_1_1endpoint.html#a491d13d6e1ad0edc5843e41b06fa4e0c)
function. Tests can now spin up clients to log in to the server, and query it for
information. Meanwhile, the server's public interface can be separately
checked; for example, I check whether the correct number of players are logged
in.

Since these tests deal with threading and networking, it is unfortunately
necessary to build some waiting in between asynchronous function calls. Wait
times are system dependent, but there doesn't seem to be an easy way to get
around them. The best solution is to grossly overestimate times and make it
clear where the uncertainty lies in test cases.
