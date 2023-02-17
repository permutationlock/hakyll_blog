---
title: Blog - C++ linting and completion
author: A.
published: October 13, 2020
tags: blog, software
---

Today I spent several hours attempting to set up a simple C++ linting system
neovim. I have used [ALE](https://github.com/dense-analysis/ale) quite successfully in the past, but for whatever
reason it would not behave today. I could not get even the most basic
functionality of setting the g:ale_linters variable to effect which linters
were enabled (setting g:ale_linters_explicit=1 would always result in no
linters enabled, and setting it 0 would always result in all linters enabled).
Moreover, ALE would not parse my makefile for flags no matter what I attempted.
Thus I had to manually enter compiler flags as a global variable. And to top it
all off, the header only library that I am working with currently,
[websocketpp](https://github.com/zaphoyd/websocketpp), takes so long to compile on my notebook that linting is almost
useless anyway.

After the debacle above, I foolishly decided to
spend another hour fiddling with [clang complete](https://github.com/xavierd/clang_complete). This I did manage to get
working, but again found that it was too slow parsing large libraries on my
notebook. So I'm back to pretty much raw nvim with syntax highlighting and I
don't think I will mess with such things for a long time, I prefer simple
systems anyway.
