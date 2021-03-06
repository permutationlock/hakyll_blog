---
title: Blog - labrinth.io planning
author: A.
published: August 24, 2020
tags: blog, software, games
---

Around six years ago I had the idea of making a turn based competitive strategy
game designed around battles between parties of adventurers. The main
inspiration for this is my combined love of esports, tabletop strategy
games (chess, Warhammer, Heroscape, D&D Minatures), and turn based
tactical RPGs (Icewind Dale, Fire Emblem, and Divinity: Original Sin).

Recently I have been giving the idea more serious thought, and to that end I
have decided to do a fast as possible build of a simple turn based game with
ranked matchmaking. Having been a longtime fan of the board game The Amazing
Labrinth, and seeing no video games that immitate this style of turn based
game, the plan is to make labrinth.io.

For the graphics I will be using three.js, simply because I am used to it and
it gives me the option of using a 3D orthogonal style if I get the time. I will
be trying out [Colyseus](https://colyseus.io) for the networking, using a
Node.js game server and JavaScript client served by Apache. The other option
would be to serve the entire app through Node.js, but I'm used to Apache and at
the moment I don't see much benefit in switching.

Further updates on labrinth.io will be forthcoming, as well as design documents
for *Kier Tactics* the planned party based game.
