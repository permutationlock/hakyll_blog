---
title: Wasting time with power series
author: A.
published: June 9, 2023
tags: math
---

Yesterday I fiddled around remembering some basic mathematics from
my math school days, and both diversions happened to involve
some formal power series.

## Room codes and birthdays

The other day I was going through ideas for implementing game invites
in my C game server. If I were to use randomly generated room codes
that players could send to one another,
how likely would it be that a two room codes would be the same?
This is essentially the popular
[Birthday Problem][1], but I was
procrastinating other work and I decided to work it out anyway.

Let $b$ be the number of bytes and $n$ be the number of rooms.
There are $2^{8b}$ possible codes and the probability that the
$(i+1)-\text{th}$ code is unique from those that came before is
$1-i/2^{8b}$. Therefore
the probability that there are no collisions and all $n$ room codes
are unique is

$$P = \prod_{i=1}^{n-1}\left(1-\frac{i}{2^{8b}}\right).$$

If we want an approximation that is slighly easier to compute, we
can use the fact that
$1-x\approx e^{-x}$ for $x$ close to $0$. To verify
this approximation we have our first encounter with power
series. Rearranging the Maclaurin expansion we have

$$
\begin{align*}
e^{-x} &= \sum_{i=0}^\infty (-1)^{i}x^i\\
&= 1-x+ \sum_{i=1}^\infty \left(x^{2i} - x^{2i+1}\right).
\end{align*}
$$

Supposing that $0<x<1$ we have $x^{2i}>x^{2i+1}$. Therefore

$$
e^{-x}-(1-x) = \sum_{i=1}^\infty\left(x^{2i}- x^{2i+1}\right)
> 0.$$

So $1-x < e^{-x}$. Even better, however, is the fact that we can
rearrange again and similarly find

$$e^{-x}-(1-x+x^2) = \sum_{i=2}^\infty\left(-x^{2i-1}+x^{2i}\right)
<0.$$

Thus $1-x<e^{-x}<1-x+x^2$ when $0<x<1$. Therefore as
$x\longrightarrow
0$, $1-x$ becomes a very good approximation of $e^{-x}$.

Applying this back to our original quetion we have

$$
\begin{align*}
P &= \prod_{i=1}^{n-1}\left(1-\frac{i}{2^{8b}}\right)\\
&\approx\prod_{i=1}^{n-1}e^{-i/2^{8b}}\\
&=e^{\left(-1/2^{8b}\right)\sum_{i=1}^{n-1}i}\\
&=e^{-n(n-1)/2^{8b+1}}.
\end{align*}
$$

This gives us an easily computable approximation. Plugging in some
numbers we can see that
if room codes are 4 bytes then $P\approx 6.1\times 10^{-5}$ and
if they are 8 bytes then $P\approx 1.4\times 10^{-14}$.

**Note 1:** it's actually not too bad in our case to compute the
original probability directly, but I thought working out the
approximation
was interesting so I
still wrote it up.

**Note 2:** neither of the probabilities calculated above actually ended up
being useful to me: as it turns out I don't care if any two rooms
share a room code, I can just re-roll any collisions. What I
really care about is the likelyhood that a player guesses the code of
an already open room, which has the boring answer of
$1/\left(2^{8b} -n\right)$.

## Using the generating funciton sledge hammer on a binary tack

My next distraction stemmed from my befuzzled brain somehow being
unable to figure out how many nodes fit in a binary tree
with $n+1$ layers.

Naturally, I wrote out the recurrence relation
$a_n = 2a_{n-1}+1$ with $a_{0}=1$. And how does one solve a
recurrence relation? With generating functions of course! Any
self respecting former math grad student
would tell you the same.

An ordinary generating function is a way of expressing a sequence
$\{a_n\}$
as the
coefficients of a formal power series

$$A(x) = \sum_{i=1}^\infty a_ix^i.$$

Manipulating the power series
algebraically can sometimes produce a general formula for each
coefficient $a_n$ in terms of $n$. The
process I go through below could similarly be used to try and solve
other sequences.

The first step is to use the recurrence relation to write
following equation

$$
\sum_{i=0}^\infty a_ix^i = \sum_{i=1}^\infty
\left(2a_{i-1}+1\right)x^i + a_0.
$$

Our goal is now to attempt to rearrange this equation
into terms of only $A(x)$ and other fixed coefficient
power series unrelated to our sequence $a_i$.
We can do this via the following
manipulations:

$$
\begin{align*}
A(x) &= \sum_{i=1}^\infty \left(2a_{i-1}+1\right)
+ a_0\\
&=\sum_{i=1}^\infty 2a_{i-1}x^i + \sum_{i=1}^\infty x^i + 1\\
&=2x\sum_{i=1}^\infty a_{i-1}x^{i-1} + \sum_{i=0}^\infty x^i.\\
&=2x A(x) + \sum_{i=0}^\infty x^i.
\end{align*}
$$

We next need the fact that

$$\sum_{i=0}^\infty x^i = \frac{1}{1-x}.$$

One can verify
this in two ways. Using algebra, we can multiply the power series on
the left by $(1-x)$ and observe that the result is $1$. Using
calculus, we can compute the Maclaurin expansion of $1/(1-x)$ and
observe that the result is the power series on the left.

Therefore we have the equation

$$
\begin{align*}
A(x) &= 2xA(x) + \frac{1}{1-x}\\
2x^2A(x)-3xA(x)+A(x) &= 1\\
(2x^2-3x+1)A(x) &= 1\\
A(x) &= \frac{1}{2x^2-3x+1}.
\end{align*}
$$

We can factor $2x^2-3x+1=(1-2x)(1-x)$. We are hoping to get the
right side of the equation in terms of something we know an
equivalent power series for, because then the coefficients of that
power series would give us the value for our sequence $\{a_n\}$.

One commonly known fact is that

$$1/(1-ax) = \sum_{i=1}^\infty (ax)^i.$$

This can be verified via exactly the same methods shown for $1/(1-x)$
above. To take advantage of this, let us rewrite our expression for
$A(x)$ as follows

$$
\begin{align*}
A(x) &= \frac{1}{2x^2-3x+1}\\
&= \frac{1}{(1-2x)(1-x)}\\
&= \frac{ax+b}{1-2x} + \frac{cx+d}{1-x}.
\end{align*}
$$

From the equation above we know that

$$
\begin{align*}
(-a-2c)x^2+(a-b+c-2d)x+b+d &= 1.
\end{align*}
$$

Therefore we simply need to solve the system of equations

$$
\begin{align*}
-a-2c &= 0\\
a-b+c-2d &= 0\\
b+d &= 1\\
\end{align*}
$$

We find that $a=-2$, $b=3$, $c=1$, and $d=-2$. So we have

$$
\begin{align*}
A(x) &= \frac{-2x+3}{1-2x} + \frac{x-2}{1-x}\\
&= (-2x+3)\sum_{i=0}^\infty (2x)^i
    + (x-2)\sum_{i=0}^\infty x^i\\
&= \sum_{i=1}^\infty (2^i-1)x^i + 1.
\end{align*}
$$

Reading off the coefficient of $x^n$ coefficients we
find that $a_0=1$, as expected, and that $a_n=2^n-1$!

Of course, this is something
we probably should have known just by thinking a little bit about a
binary tree... But hey, I'm proud that I recalled how to use
generating functions.

[1]: https://en.wikipedia.org/wiki/Birthday_problem
