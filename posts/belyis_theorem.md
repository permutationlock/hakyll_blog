---
documentclass: article
title: Belyi's Theorem
tags: math algebra geometry arithmetic
published: December 2, 2018
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts,tikz-cd}
---

Belyi's Theorem is an arithmetic-geometric result that has the unusual quality
of being both fairly recent and provable with only the material from a
first course in classical algebraic geometry.

In what follows $\mathbb{A}^n,\mathbb{P}^n$
are over $\mathbb{C}$ unless otherwise specified.

**Theorem (Belyi, 1979).** Let $C\subset\mathbb{P}^2$ be an irreducible
non-singular
projective curve defined as the zero set of $F\in\overline{\mathbb{Q}}[X,Y,Z].$
Then there exists a
finite regular map $C\to\mathbb{P}^1$ ramified only over a subset of
$\{0,1,\infty\}.$

The proof strategy takes a projection $\pi$ of $C$ onto $\mathbb{P}^1$
and then use a
sequence of regular maps $\{\psi_i\}$ to gather the ramification points of
$\pi$ into the set $\{0,1,\infty\}.$

**Lemma 1.** The projection away from the point $(0:0:1)$ is
a finite regular map $\pi:C\to\mathbb{P}^1$
ramified over the points $\pi(P)$ such that $F_Z(P)=0.$

<div class="proof">
Let $Q=(x_0:y_0)\in\mathbb{P}^1.$ Then $\pi$ is ramified over $Q$ if
and only if there exists $P\in\pi^{-1}(Q)$ such
that $e_P(\pi)>1.$ Note that

$$
    t_Q=(y_0X-x_0Y)/H(X,Y)
$$

is a local
parameter for $Q$, where $H(X,Y)$ is any linear form not vanishing at $Q.$
Thus

$$
    e_P(\pi)=\text{ord}_P(\pi^*(t_Q)).
$$

Hence $e_P(\pi)>1$ if and only if $y_0X-x_0Y$ is zero on the
tangent line $\Theta_{C,P}.$ The tangent line is described by

$$
    F_X(P)X+F_Y(P)Y+F_Z(P)Z=0.
$$

We care whether the tangent line contains the line $y_0X-x_0Y$, i.e.,
if the tangent line passes through $(0:0:1).$ This is equivalent to $F_Z(P)=0.$
</div>

**Claim 1.** The map $\pi$
is ramified only over points in $\overline{Q}\cup\{\infty\}.$

<div class="proof">
By Lemma 1 above we know that the ramification points of $\pi$ are the images
of points in $C\cap V(F_Z).$ By Bezout's Theorem

$$
    C\cap V(F_Z)\subset\mathbb{P}^2(\overline{\mathbb{Q}})
$$

contains precisely
$\text{deg}F\cdot\text{deg}F_Z$ points when counted with multiplicity.
By Bezout again, $C\cap V(F_Z)$ contains precisely the same number of points
when considered in $\mathbb{P}^2(\mathbb{C}).$ Since
$\mathbb{P}^2(\overline{\mathbb{Q}})\subset\mathbb{P}^2(\mathbb{C})$ all
intersection
points are contained in
$\mathbb{P}^2(\overline{\mathbb{Q}}).$
</div>

**Lemma 2.** A regular map $\psi:\mathbb{A}^1\to\mathbb{A}^1$ defined by
$x\mapsto G(x)$ is ramified precisely over the images of points $x_0$ where
$G_X(x_0)=0.$ Hence it extends to a map $\mathbb{P}^1\to\mathbb{P}^1$
ramified over only over those affine ramification points and $\infty$.


<div class="proof">
Note that if $\psi$ is ramified over $y_0\in\mathbb{A}^1$ if and only if
there exists
$x_0\in\mathbb{A}^1$ such that $G(x_0)=y_0$ and $e_{x_0}(\psi)>1.$

Observe that

$$
    e_{x_0}(\psi)=\text{ord}_{x_0}(\psi^*(Y-y_0)).
$$

We find that

$$
    \psi^*(Y-y_0)=G(X)-G(x_0)=H(X),
$$

and thus $G_X=H_X.$ Thus if $G_X(x_0)=0$, then

$$
    H(x_0)=H_X(x_0)=0
$$

and $(X-x_0)^2\mid H(X).$
Conversely if we write $H(X)=(X-x_0)^mu(X)$ for some unit $u(X)\in\mathcal{O}_{\mathbb{A}^1,x_0}$
then $H_X(x_0)=0$ if and only if $m>1.$
</div>

Let $\varphi:C\to\mathbb{P}^1$ be a regular map ramified only over a finite set
$S\subset\overline{\mathbb{Q}}\cup\{\infty\}.$

**Claim 2.** If $S\subset\overline{\mathbb{Q}}\cup\{\infty\}$, then there
exists $\psi:\mathbb{P}^1\to\mathbb{P}^1$ such that $\psi\circ\phi$ is ramified
only over points in $\mathbb{Q}\cup\{\infty\}.$

<div class="proof">
Let $n$ be the maximum degree over $\mathbb{Q}$ of an element in $S$ and $p$ be
the number of elements in $S$ of degree $n.$
We will proceed by double induction on $n$ and $p.$

Our object is to construct a map $\psi:\mathbb{P}^1\to\mathbb{P}^1$
such that $\psi\circ\phi$ is ramified
only over points in $S'\subset \overline{\mathbb{Q}}\cup\{\infty\}$ where the
number of elements in $S'$ of degree $n$ over $\mathbb{Q}$ is less than $p$ and
the maximum degree of elements in $S'$ is less than or equal to $n.$

Let $\alpha\in S$ be an element of degree $n$ over $\mathbb{Q}$ and let
$G\in\mathbb{Q}[X]$ be its minimal polynomial. Define
$\psi:\mathbb{P}^1\to\mathbb{P}^1$ by $x\mapsto G(x).$ Let
$\{\beta_1,\ldots,\beta_r\}$ be the roots of $G_X.$ Note that since
$\text{deg}(G_X)<n$ the elements $\beta_1,\ldots,\beta_r$ are of degree $n-1$
or fewer over $\mathbb{Q}.$

Observe that ramification points of $\psi\circ\phi$ are

$$
    S'=\psi(\{\beta_1,\ldots,\beta_r\})\cup\psi(S).
$$

Moreover, applying a polynomial over
$\mathbb{Q}$ cannot increase the degree of an algebraic element over
$\mathbb{Q}.$ Thus the maximum degree over $\mathbb{Q}$ of elements in $S'$ is
$n$ and there are at most $p-1$ such elements since $\phi(\alpha)=0.$
</div>

**Claim 3.** If $S\subset\mathbb{Q}\cup\{\infty\}$ then there exists a regular
map
$\psi:\mathbb{P}^1\to\mathbb{P}^1$ such that $\psi\circ\varphi$ is ramified
only over points in $\{0,1,\infty\}.$

<div class="proof">
If $|S|\le 3$ then clearly there is an automorphism $\psi$ such that
$\psi(S)\subset\{0,1,\infty\}.$

Suppose that $|S|\ge 4.$ We will construct a map
$\psi:\mathbb{P}^1\to\mathbb{P}^1$ such that $\psi\circ\varphi$ is ramified
over some set of points $S'\subset\mathbb{Q}\cup\{\infty\}$ with $|S'|<|S|.$

By applying an appropriate automorphism of $\mathbb{P}^1$ we may assume that
$S=\{0,1,\infty,\lambda_1,\ldots,\lambda_r\}.$ Subsequently we may apply the
automorphisms $(X,Y)\mapsto (1-X,Y)$
and $(X,Y)\mapsto (Y,X)$ as necessary to ensure that $0<\lambda_1<1$; note that
each maps $\{0,1,\infty\}\to\{0,1,\infty\}.$

Write $\lambda_1=m/(m+n).$ Let $c=\frac{(m+n)^{m+n}}{m^nn^m}.$
Let us define $\psi:\mathbb{P}^1\to\mathbb{P}^1$
by $x\mapsto G(x)$ where $g(X)=cX^m(1-X)^n.$ Observe
that

$$
    G_X=cX^{m-1}(1-X)^{n-1}(m-(n+m)X)
$$

which has zeros only at $0,1$ and
$\lambda_1.$ Thus $\psi$ is ramified only over

$$
    \psi(S)=\{0,1,\infty,\psi(\lambda_2),\ldots,\psi_(\lambda_r)\}.
$$

Continuing inductively and composing we construct a map
$\psi:\mathbb{P}^1\to\mathbb{P}^1$
such that $\psi(S)\subset\{0,1,\infty\}$ and $\psi$ is ramified only over the
values in $S.$
</div>

Belyi's Theorem follows from claims 1, 2, and 3.


**Example.** Consider the elliptic curve $C$ defined by the affine equation

$$
    F(X,Y)=Y^2-X(X-1)(X-\sqrt{2})=0.
$$

Note the curve has one point "at infinity,"
namely the point $(0:1:0).$ Let $\pi$ be the projection away from this point,
that is, $(x,y)\mapsto x$ on the affine patch $z=1.$ Note that that this
extends to a projection $C\to\mathbb{P}^1$ since $C$ is nonsingular. This is a
slightly different projection than the one in the proof above, but works nicely
in this case.

Observe that $\text{deg}(\pi)=2$ since $[\mathbb{C}(C):\mathbb{C}(X)]=2.$
Therefore projection $\pi$ is
ramified over $\infty$ and the images of the points in $C$ where
$F_Y(X,Y)=2Y=0.$ These are the points $0,1,\sqrt{2}.$

The minimal polynomial of $\sqrt{2}$ over $\mathbb{Q}$ is $X^2-2.$ Thus we
consider the map $\psi_1$ defined by $t\mapsto t^2-2$; note this map is
ramified only at $0.$ We compute

$$
    \psi_1(\{0,1,\infty, \sqrt{2}\})=\{0,-1,-2,\infty\}.
$$

We next apply the map $\phi_2$
defined by $t\mapsto -1/t$ which achieves the dual purpose of mapping
$\{0,-1,\infty\}$ to $\{0,1,\infty\}$ and placing $\psi_2(-2)=1/2\in (0,1).$
Finally, we apply the Belyi map $t\mapsto 4t(1-t)$ which sends
$\{0,1,\infty,1/2\}$ to $\{0,1,\infty\}.$
