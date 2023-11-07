---
title: Cross-compiling C for the web
author: A.
published: February 16, 2023
tags: c, wasm
git: https://github.com/permutationlock/emscripten_c_examples
---

In this post we'll take a look at how to use [Clang][5] and
[Emscripten][6] to make C code that can cross-compile to
[WebAssembly][9] and run in a web page.

First, we will take a look at what [WebAssembly][9] is and how to
compile simple functions to target the web with [Clang][5].
Second, we will create a cross-platform "Hello, world!" app
with [Emscripten][6]. Then we'll get more general
file i/o working on the web. Finally, we will get a
[Raylib][1] app set up to cross-compile to the browser.

In order to follow along you will need the [Clang][5]
compiler and [lld][11].
Second, you will need to have
[Emscripten][6] installed. You will also need a web server to test
your web builds; I will use [Go][7] and provide a simple server, but
use whatever you are comfortable with. I'll also use [make][3] for
build scrips and [git][8] to pull source code.

All shell commands will be written for a Unix-like system,
I was on x86\_64 Linux.

## What is WebAssembly?

When targeting web platforms, old methods would often translate
native source code to JavaScript. In the past several years,
however,
a new option has become available: [WebAssembly][9]. WebAssembly
is a portable assembly language used to create binary executables
that run on a simple stack-based virtual machine. It strives to
provide performance far closer to native binaries when compared
with JavaScript.

Let us take our favorite fibonacci function written in C and
compile it to WASM so that it can be called from JavaScript
and used in a web app. We'll start by making a new project
directory and creating a
`fibonacci.c` file.

```C
// fibonacci.c

int fibonacci(int n) {
    if(n <= 1) {
        return n;
    }

    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

The `clang` compiler can target `wasm32` (32 bit WebAssembly)
right out of the box.

```bash
mkdir static
clang fibonacci.c -o static/fibonacci.wasm --target=wasm32 \
    --no-standard-libraries \
    -Wl,--export-all -Wl,--no-entry
```

The `--target=wasm32` flag tells the compiler to output 32 bit
WebAssembly. Next, `--no-standard-libraries` tells the compiler to
omit libc as clang does not come with a WASM compatible
implementation of libc; the libc on your local machine contains
a lot of code that won't naturally translate to WASM such as
operating system calls. Finally, `-Wl,--export-all` tells the linker
to export all
functions and `-Wl,--no-entry` tells the linker that we are not
making a runable binary.

Now that we have a `fibonacci.wasm` file containing our `fibonacci`
function, we need to write an HTML file and a JavaScript snippet to
load and run our WebAssembly.

```HTML
<!-- static/index.html -->

<!DOCTYPE html>
<html>
<script>
WebAssembly.instantiateStreaming(fetch("fibonacci.wasm"), {}).then(
        (obj) => console.log(
            "The 6th Fibonacci number is " +
            obj.instance.exports.fibonacci(6)
        )
    );
</script>
</html>
```

To test our web app we just need a web server to host it. I will be
using the following Go web server.

```Go
// server.go

package main

import (
    "log"
    "net/http"
)

func main() {
    http.HandleFunc(
        "/",
        func(w http.ResponseWriter, r *http.Request) {
            http.ServeFile(w, r, "static/"+r.URL.Path[1:])
        })

    log.Fatal(http.ListenAndServe(":8081", nil))
}
```

Run the webserver to host the `static/` directory.

```bash
go run server.go
```

Visiting [localhost:8081](http://localhost:8081) in a web browser
will produce... a blank screen. But, if we open the developer
console, we should see the expected output!

```default
The 6th Fibonacci number is 8
```

## Hello Emscripten

In the last section we saw how simple functions can be compiled to
WASM, but what if we want to make an entire application
cross-platform? Well, at minimum we need an implementation of the C
standard library that works in the browser (or I suppose you could
implement a cross-platform standard library of your own as a
replacement to libc).

Luckily, there is an available solution: [Emscripten][6]! Emscripten
provides the `emcc` compiler and linker which emulates POSIX operating
system features in the browser, including a web compatible libc and OpenGL.

Let's make the classic C intro program.

```C
// main.c

#include <stdio.h>

int main() {
    printf("Hello, emcc!\n");
    return 0;
}
```

Compiling and running this on our local system we should get the
expected print out.

```bash
clang main.c -o hello
./hello
Hello, emcc!
```

In order to get this working in Emscripten we need a little bit of
setup. First, let us make a static directory for our webserver to
host.

```bash
mkdir static
```

Now we can compile our code with `emcc`. The `emcc` compiler has three
different output targets:

 - WebAssembly: creates a raw `.wasm` file.
 - JavaScript: adds a `.js` file containing a Module object
   that binds native functionality to web features: e.g.
   stdout is mapped to `console.log.
 - HTML: uses a template to generate a `.html` file to load
   and run the application. The default HTML template
   provides several features for testing, such as a
   canvas for graphical windows and an in page console.

The best way to learn about how `emcc` works, in my opinion, is to
simply read through the generated Javascript bindings.
In this post I will outline how to create our own HTML file to
make use of the emcc generated WASM and JavaScript files.

```bash
emcc main.c -o static/hello.js
```

Let's add a bare bones `static/index.html` file to load our compiled web
app.

```HTML
<!-- static/index.html -->

<!DOCTYPE html>
<html>
<script src="hello.js"></script>
</html>
```

Looking in the static directory, we should now have our `index.html`
file, as well as two `emcc` generated files: `hello.js` and
`hello.wasm`.

To test our web app we can host it using the same `server.go` file
described earlier.

```bash
go run server.go
```

Visit [localhost:8081](http://localhost:8081), open the developer
console, and we should see the greating!

### Configuring the Emscripten Module

What if we wanted to change where stdout goes? Well, all we need to
do is take a look at `hello.js`. The comment at the top of the file
explains that the Emscripten bindings are controlled via a `Module`
object. The `Module` object can either be created and customized
externally
before the `hello.js` script is loaded, or a default Module will be
created. At the bottom of the file a function `run` is defined and
called (if `Module['noInitialRun']` is not `true`).

Currently we are using the defualt `Module`. Looking through
`hello.js` a
bit more we can find the spot where stdout and stderr
behaviour is defined.

```JavaScript
// hello.js | line 475

// ...

var out = Module['print'] || console.log.bind(console);
var err = Module['printErr'] || console.warn.bind(console);

// ...
```

If we want to set our own output function, we can simply define the
`Module` object and set `Module['print']` to whatever we want.

```HTML
<!-- static/index.html -->

<!DOCTYPE html>
<html>
<script>
var Module = {
    print: window.alert.bind(window),
};
</script>
<script src="hello.js"></script>
</html>
```

Now if we run the web server and open the page we get an annoying
alert box with our message. It is probably best to switch
back to `console.log` if you countinue using this project
as a base for the following examples.

## File I/O

When running a native application we have a local file system to
use for reading and writing data. Emscripten provides a way to
emulate this functionality.

Starting from the basic "hello emcc" project created above,
let us modify `main.c` to open a file and print some of its contents.

```C
// main.c

#include <stdio.h>

int main() {
    char buffer[32] = { 0 };
    FILE* f = fopen("static/hello.txt", "r");
    fread(buffer, sizeof(char), 31, f);

    printf("File contained: %s\n", buffer);

    return 0;
}
```

Next we'll create a `static/hello.txt` file to be read by our app.

```bash
mkdir files
echo "I am a file!" > files/hello.txt
```

Compiling and running on our local system, we get the expected
output.

```bash
clang main.c -o hello
./hello
File contained: I am a file!

```

However, if we compile and test our file on the web, the console
shows an error.

```default
Uncaught (in promise) RuntimeError: index out of bounds
    createExportWrapper http://localhost:8081/hello.js:903
    callMain http://localhost:8081/hello.js:4318
    doRun http://localhost:8081/hello.js:4371
    run http://localhost:8081/hello.js:4386
    runCaller http://localhost:8081/hello.js:4303
    removeRunDependency http://localhost:8081/hello.js:832
    receiveInstance http://localhost:8081/hello.js:991
    receiveInstantiationResult http://localhost:8081/hello.js:1009
```

We never told `emcc` about our `files/hello.txt` file, so of course the
WebAssembly app cannot find it. In order to package a file or
directory of files alongside our code, we need to use
a compiler flag to tell `emcc` to include them.

```bash
emcc main.c -o static/hello.js --preload-file files/
```

Looking in the static directory we now see that a third generated
file has appeard: `hello.data`. Reloading our web app we should now
see the same console output that our local binary produced.

## A resizable Raylib app

Porting command line applications to the web is not
terribly useful, so let's do something a bit more complicated.
The goal
of this section is to create a resizable window drawing some centered
text. On our local system this will be a standard window in whatever
window
manager is being used, e.g. [X][12]. On the web our "window" will
be a
canvas that fills the content area of the browser window and resizes
with it.

### Setup

Start once again with the hello emcc example from the first section.
For this project we will need to download the Raylib source code.

```bash
git clone https://github.com/raysan5/raylib.git
```

To compile Raylib for your local system you will need to follow the
instructions found on the [GitHub readme][1]. The easiest way to get
Raylib working is to use a package manager to install the library and
its dependencies. Local compilation examples below will assume that
the Raylib libraries are already installed.

Thankfully, compiling
Raylib for the web does not require
any dependencies other than the Raylib source and the `emcc`
compiler.
The Raylib library is written with Emscripten support
and the `Makefile` has an option to target web platforms. Lets
use `make` to build an Emscripten compatible library file.

```bash
cd raylib/src/
PLATFORM=PLATFORM_WEB make
cd ../../
mkdir libweb
cp raylib/src/libraylib.a libweb/
```

It will also be nice to grab the Raylib header file and put it in a
dedicated include directory for our project.

```bash
mkdir include
cp raylib/src/raylib.h include/
```

### Drawing a square window

Next let us modify main.c to use Raylib to create a 300 pixel square
window and fill it with a dark gray background.

```c
// main.c

#include <raylib.h>

int main() {
    InitWindow(300, 300, "hello, raylib!");

    while(!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(DARKGRAY);
        EndDrawing();
    }

    return 0;
}
```

Our build commands in this section will get fairly involved, so we
will head things off by making a [`Makefile`][3] (feel free to replace
this with your build script of choice).

```bash
# Makefile

LOAD = -lraylib
INCLUDE = -Iinclude

CC = clang
LIB =
FLAGS =

EMCC = EMCC
WLIB = -Llibweb
WFLAGS = -s USE_GLFW=3 -s ASYNCIFY

all: native web

native: main.c
	$(CC) main.c -o hello $(FLAGS) $(LIB) $(INCLUDE) $(LOAD) 

web: main.c
	$(EMCC) main.c -o static/hello.js $(WFLAGS) $(WLIB) $(INCLUDE) $(LOAD) 

clean:
	rm hello static/hello.js static/hello.wasm
```

Running `make` with the above `Makefile` will produce the following
build commands.

```bash
clang main.c -o hello -Iinclude -lraylib
emcc main.c -o static/hello.js -Iinclude -Llibweb \
    -lraylib -s USE_GLFW=3 -s ASYNCIFY
```

The `clang` build command assumes that Raylib is already
in the main build path, i.e. installed via a package manager. You
could modify the variables in `Makefile` to match your setup.

The `-s USE_GLFW=3` flag tells the compiler we will be using
[gflw][2] version 3. The `-s ASYNCIFY` tells the compiler
to modify our code to allow it to interact with asynchronous
JavaScript. Currently our code is an infinite loop, which would block
out all JavaScript events if taken literally. Both flags are
necessary in our case because the Raylib library file requires them.

You should now be able to run `make` to build for both native and
web. Testing on web we should now get an error.

```default
Uncaught (in promise) TypeError: Module.canvas is undefined
```

Emscripten requires an HTML canvas to draw a graphical window. If we
look through `staic/hello.js` we can find the relevant property is
`Module['canvas']`. Let us modify our HTML file to add a canvas
element.

```HTML
<!-- static/hello.js -->

<!DOCTYPE html>
<html>
    <body>
        <canvas id="canvas"></canvas>
    </body>
    <script>
        var Module = {
            canvas: document.getElementById('canvas'),
        };
    </script>
    <script src="hello.js"></script>
<html>
```

Refreshing the page, we now see a 300 pixel gray square.

### Using the Emscripten animation loop

Currently our code runs a simple infinite loop no matter the
platform.
On the web it is best practice to run an "asynchronous loop,"
that is, to define a function that contains the body of the loop and
then have the browser regularly call that function to request
animation frames.

```c
// main.c

#include <raylib.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

void update() {
    BeginDrawing();
    ClearBackground(DARKGRAY);
    EndDrawing();
}

int main() {
    InitWindow(300, 300, "hello, raylib!");

#ifdef __EMSCRIPTEN__
    emscripten_set_main_loop(update, 0, 1);
#else
    while(!WindowShouldClose()) {
        update();
    }
#endif

    return 0;
}
```

The `#ifdef __EMSCRIPTEN__` preprocessor condition allows us to check
whether the `emcc` compiler is being used. If the compiler is `emcc`,
we set `update` as the Emscripten main loop callback function.
Otherwise, we simply run the animation loop as normal.

### Making the window resizable

In order to make a window resizable in a native Raylib application,
we simply need to set a config flag before calling
`InitWindow`.

```C
// main.c

// ...

    SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    InitWindow(300, 300, "hello, raylib!");

// ...
```

Let us also make the `update` function draw some centered text so we
can ensure that window is re-drawing correctly.

```C
// main.c

// ...

void update() {
    char* text = "Hello, emcc!";

    BeginDrawing();

    ClearBackground(DARKGRAY);

    int width = GetScreenWidth();
    int height = GetScreenHeight();

    int twidth = MeasureText(text, 22);
    int theight = 10;

    DrawText(
        text,
        (width - twidth) / 2,
        (height - theight) / 2,
        22,
        RAYWHITE
    );

    EndDrawing();
}

// ...
```

Making and re-running both the native and web builds, we should see
that the native window is now resizable. Unfortunately, the web app
still doesn't resize.

The first
issue is that the "window" in the context of the
web application is the canvas, not the browser window. However,
simply making the canvas fill the browser window does not fix the
problem since
Emscripten does not emulate the canvas resizing as a
window resize.

The solution is to do things manually by taking advantage of another
feature of
Emscripten: [cwrap][4]. We will first need to create a C function
that should be called whenever the canvas is resized.

```C
// main.c

// ...

#ifdef __EMSCRIPTEN__
extern void on_resize(int width, int height) {
    SetWindowSize(width, height);
}
#endif

// ...
```

Then we will add the following compile flags to our `Makefile` to
tell `emcc` to export our new `on_resize` function in addition to
`main`.

```bash
# Makefile

# ...

WFLAGS = -s USE_GLFW=3 -s ASYNCIFY \
         -s EXPORTED_RUNTIME_METHODS=cwrap \
         -s EXPORTED_FUNCTIONS=_main,_on_resize

# ...
```

We also tell `emcc` to generate `cwrap` functionality which allows us
to wrap exported C functions as JavaScript functions.

If we call `make` with our updated `Makefile` and look through
`static/hello.js` we can find the defintion of `cwrap`.

```JavaScript
// static/hello.js | line 8451

// ...

/**
 * @param {string=} returnType
 * @param {Array=} argTypes
 * @param {Object=} opts
 */
function cwrap(ident, returnType, argTypes, opts) {
    return function() {
        return ccall(ident, returnType, argTypes, arguments, opts);
    }
}

// ...
```

Not delving too much into the inner workings,
`Module.cwrap` requires three
arguments: `ident` is the name of the exported C function to
be wrapped, `returnType` is the return type, and `argTypes` is an
array indicating the function arguments. In our case `ident` should
be `"on_resize`", `returnType` should be `null`, and `argTypes`
should be `[number, number]`.

To put our newly exported function into use with `cwrap` we need to
make some major modifications to `static/index.html`.

```HTML
<!-- static/index.html -->

<!DOCTYPE html>
<html>
    <body>
        <canvas id="canvas"></canvas>
    </body>
    <script>
        function on_load() {
            let canvas = document.getElementById(
                "canvas"
            );
            let on_resize = Module.cwrap(
                "on_resize",
                null,
                ["number", "number"]
            );
            let resize_handler = () => {
                const w = canvas.width
                    = window.innerWidth;
                const h = canvas.height
                    = window.innerHeight;
                on_resize(w, h);
            }
            window.addEventListener(
                "resize",
                resize_handler,
                true
            );
            resize_handler();
        }

        var Module = {
            postRun: [on_load],
            canvas: document.getElementById('canvas'),
        };
    </script>
    <script src="hello.js"></script>
</html>
```

The above changes make it so that
the canvas resizes with the browser window and the `on_resize`
function is called when the canvas is
resized. The `postRun` property of `Module` is an array of functions
to be called after the Emscripten module is running.
We need our `on_load` function to be called after `Module` has been
configured because we require the `Module.cwrap` function.

Recompiling, everything should work as expected and the window should
resize on web!

If we want to have the canvas completely fill the
browser window and eliminate any white edges, we can add a header
with some CSS styling to `static/index.html`.

```HTML
<!-- static/index.html -->

<!-- ... -->

    <head>
        <title>Hello, emcc!</title>
        <style>
            * {
                padding: 0;
                margin: 0;
            }
            body {
                overflow: hidden;
            }
        </style>
    </head>

<!-- ... -->
```

Finally, here is a complete look at the three files we wrote in
this section.

```C
// main.c

#include <raylib.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

#ifdef __EMSCRIPTEN__
extern void on_resize(int width, int height) {
    SetWindowSize(width, height);
}
#endif

void update() {
    char* text = "Hello, emcc!";

    BeginDrawing();

    ClearBackground(DARKGRAY);

    int width = GetScreenWidth();
    int height = GetScreenHeight();

    int twidth = MeasureText(text, 22);
    int theight = 10;

    DrawText(
        text,
        (width - twidth) / 2,
        (height - theight) / 2,
        22,
        RAYWHITE
    );

    EndDrawing();
}

int main() {
    SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    InitWindow(300, 300, "hello, raylib!");

#ifdef __EMSCRIPTEN__
    emscripten_set_main_loop(update, 0, 1);
#else
    while(!WindowShouldClose()) {
        update();
    }
#endif

    return 0;
}
```

```HTML
<!-- static/index.html -->

<!DOCTYPE html>
<html>
    <head>
        <title>Hello Emscripten</title>
        <style>
            * {
                padding: 0;
                margin: 0;
            }
            body {
                overflow: hidden;
            }
        </style>
    </head>
    <body>
        <canvas id="canvas"></canvas>
    </body>
    <script>
        function on_load() {
            let canvas = document.getElementById(
                "canvas"
            );
            let on_resize = Module.cwrap(
                "on_resize",
                null,
                ["number", "number"]
            );
            let resize_handler = () => {
                const w = canvas.width
                    = window.innerWidth;
                const h = canvas.height
                    = window.innerHeight;
                on_resize(w, h);
            }
            window.addEventListener(
                "resize",
                resize_handler,
                true
            );
            resize_handler();
        }

        var Module = {
            postRun: [on_load],
            canvas: document.getElementById('canvas'),
        };
    </script>
    <script src="hello.js"></script>
</html>
```

```bash
# Makefile

LOAD = -lraylib
INCLUDE = -Iinclude

CC = clang
LIB =
FLAGS =

EMCC = emcc
WLIB = -Llibweb
WFLAGS = -s USE_GLFW=3 -s ASYNCIFY \
         -s EXPORTED_RUNTIME_METHODS=cwrap \
         -s EXPORTED_FUNCTIONS=_main,_on_resize

all: native web

native: main.c
	$(CC) main.c -o hello $(FLAGS) $(LIB) $(INCLUDE) $(LOAD) 

web: main.c
	$(EMCC) main.c -o static/hello.js $(WFLAGS) $(WLIB) $(INCLUDE) $(LOAD) 

clean:
	rm hello static/hello.js static/hello.wasm
```

[1]: https://github.com/raysan5/raylib
[2]: https://www.glfw.org/
[3]: https://www.gnu.org/software/make/
[4]: https://emscripten.org/docs/porting/connecting_cpp_and_javascript/Interacting-with-code.html?highlight=cwrap
[5]: https://clang.llvm.org/
[6]: https://emscripten.org/
[7]: https://go.dev/
[8]: https://git-scm.com/
[9]: https://webassembly.org/
[10]: https://llvm.org/
[11]: https://lld.llvm.org/
[12]: https://en.wikipedia.org/wiki/X_Window_System
