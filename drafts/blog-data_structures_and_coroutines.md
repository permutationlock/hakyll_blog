---
title: Data structures and coroutines
author: A.
published: August 2, 2023
tags: c, algorithms
---

This article covers how implementing a left-leaning
red-black tree led me on an adventure to understanding coroutines and algorithm
visualization.

In my quest to create a multiplayer game server that I understood down to
the level operating system calls, I first needed to write a small collection of
data structures in C. At one point I needed a structure to map account IDs to
active sessions, both to allow re-connection and disallow duplicate sessions
from a single account. I decided to implement an ordered map via a balanced
binary search tree.

One of the most common ways to implement a balanced binary search tree is a
red-black tree.
A left leaning red-black tree is slightly less efficent than a standard
red-black tree (but has the same time complexity), but is very simple to
implement.

The idea behind a red-black tree is to simulate a 2-3-4 tree in a binary tree
