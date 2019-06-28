---
documentclass: article
title: Fulton 1.2
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**1.8.\*** Show that the algebraic subsets of $\mathbb{A}^1(k)$ are just the
finite subsets, together with $\mathbb{A}^1(k)$ itself.

<div class="proof">
Suppose $A=V(S)$ for some $S\subset k[X].$ If $S=\emptyset$ then $A=
\mathbb{A}^1(k).$ Otherwise let $F\in S.$ Note $A\subset V(F).$
Since $F$ has finitely many roots, $V(F)$ is finite. Thus $A$ is finite.

Conversely, by (4) any finite subset of $\mathbb{A}^1(k)$ is algebraic.
</div>

**1.9.** If $k$ is a finite field, show that every subset of $\mathbb{A}^n
(k)$ is algebraic.

<div class="proof">
Since $k$ is finite, $\mathbb{A}^n(k)$ is finite. Thus every subset of
$\mathbb{A}^n(k)$ is finite and therefore algebraic by (4).
</div>

**1.10.** Give an example of a countable collection of algebraic sets whose
union is not algebraic.

Let $F_n=(x-n)^2\in\mathbb{R}[X],$ $n\in\mathbb{Z}.$ We claim $S=\bigcup_{n\in\mathbb{Z}}V(F_n)\subset\mathbb{R}$
is not algebraic in $\mathbb{A}^2(k).$

<div class="proof">
Suppose $S$ is algebraic. Then $S\subset V(F)$ for some $F\in \mathbb{R}[X].$ Observe
$V(X)\cap S=\mathbb{Z}.$ Thus $V(X)\cap
V(F)$ is infinite, a contradiction.
</div>

**1.11.** Show that the following are algebraic sets:

\(a\) $\{(t,t^2,t^3)\in\mathbb{A}^3(k) \ | \ t\in k\}$;

Observe  $V(X-Y^2,X-Z^3)=\{(t,t^2,t^3)\in\mathbb{A}^3(k) \ | \ t\in k\}.$

\(b\) $\{(\cos(t), \sin(t))\in\mathbb{A}^2(k) \ | \ t\in\mathbb{R}\}$;

It is easy to check $V(X^2+Y^2-1)=\{(\cos(t), \sin(t))\in\mathbb{A}^2(k) \ | \
t\in\mathbb{R}\}.$

\(c\) the set of points in $\mathbb{A}^2(\mathbb{R})$ whose polar coordinates
$(r,\theta)$ satisfy the equation $r=\sin(\theta).$

It is can be verified that this is the set $V(X^2+(Y-1/2)^2-1/4).$

**1.12.** Suppose $C$ is an affine plane curve, and $L$ is a line in
$\mathbb{A}^2(k),$ $L\not\subset C.$ Suppose $C=V(F),$ $F\in k[X,Y]$ a
polynomial of degree $n.$ Show that $L\cap C$ is a finite set of no more than
$n$ points.

<div class="proof">
Suppose $L=V(aY+bX+c),$ where $a,b,c\in k$ and at least one of $a$ or $b$ is
nonzero. Without loss of generality, suppose $a\ne 0.$ Then $L=V(Y-dX-e),$ where
$d=-b/a$ and $e=-c/a.$

If $(x,y)\in L$ then $y=dx+e.$ Thus if $(x,y)\in L\cap C$ then
$F(x,y)=F(x,dx+e)=0.$ Observe $F(X,dX+e)\in k[X]$ with degree $n.$ Thus
$F(X,dX+e)$ has at most $n$ roots and $L\cap C$ contains at most $n$
points.
</div>

**1.13.** Show that each of the following sets is not algebraic:

\(a\) $\{(x,y)\in\mathbb{A}^2(\mathbb{R}) \ | \ y=\sin(x)\}$;

<div class="proof">
Suppose $A=\{(x,y)\in\mathbb{A}^2(\mathbb{R}) \ | \ y=\sin(x)\}=V(S),$ $S\subset \mathbb{R}[X,Y].$
Clearly $A\ne\mathbb{A}^2(\mathbb{R}),$ so $S\ne\emptyset.$ Let $F\in S.$ Then $A\subset
V(F).$ But $A\cap V(Y)=\{(2\pi t,0) \ | \ t\in\mathbb{Z}\}$ is infinite. Thus $V(F)\cap
V(Y) \supset A\cap V(Y)$ is infinite, a contradiction to Problem 1.12.
</div>

\(b\) $\{(z,w)\in\mathbb{A}^2(\mathbb{C}) \ | \ |z|^2+|w|^2=1\}$;

<div class="proof">
Suppose $A=\{(z,w)\in\mathbb{A}^2(\mathbb{C}) \ | \ |z|^2+|w|^2=1\}=V(S),$ $S\subset\mathbb{C}(Z,
W).$ Clearly $S\ne\emptyset.$ Let $F\in S.$ Observe $A\cap V(W)=\{(z,0)\in\mathbb{A}^2
(\mathbb{C}) \ | \ |z|^2=1\},$ an infinite set. Thus $V(F)\cap V(W)$ is infinite, a
contradiction to Problem 1.12.
</div>

\(c\) $\{\cos(t),\sin(t),t)\in\mathbb{A}^3(\mathbb{R}) \ | \ t\in\mathbb{R}\}.$

<div class="proof">
Suppose $A=\{\cos(t),\sin(t),t)\in\mathbb{A}^3(\mathbb{R}) \ | \ t\in\mathbb{R}\}=V(S),$ $S\subset
\mathbb{R}[X_1,X_2,X_3].$ Clearly $S\ne\emptyset.$ Let $F\in S.$ Note $A\subset V(F).$
So $F(\cos(t),\sin(t),t)=0$ for all $t\in\mathbb{R}.$ Thus for any $\theta\in[0,2\pi),$
$$
        F(\cos(\theta+2n\pi),\sin(\theta+2n\pi),\theta+2n\pi)=
                F(\cos(\theta),\sin(\theta),\theta+2n\pi).
$$
for any $n\in\mathbb{Z}.$
But fixing $\theta$ this gives $G(X)=F(\cos(\theta),\sin(\theta),X)\in \mathbb{R}[X]$
with $G(n)=0$ for any $n\in\mathbb{Z}.$ This is a contradiction as $G$ must have
finitely many roots in $\mathbb{R}.$
</div>

**1.14.\*** Let $F$ be a nonconstant polynomial in $k[X_1,\ldots,X_n],$ $k$
algebraically closed. Show that $\mathbb{A}^n(k)\setminus V(F)$ is infinite if $n\ge
1.$

<div class="proof">
Suppose $n=1.$ Then $F\in k[X]$ and $F$ has finitely many roots. Since $k$ is
algebraically closed, $k$ is infinite by Problem 1.6. Thus $\mathbb{A}^1(k)\setminus
V(F)$ is infinite.

Suppose $n>1.$
Let $a_1,\ldots,a_{n-1}\in k.$ Then $G=F(a_1,\ldots,a_{n-1},X_n)\in k[X_n].$
Note $G$ has finitely many roots $b_1,\ldots,b_m.$ Observe
$$
        \mathbb{A}^n(k)\setminus V(F)\supset\{(a_1, \ldots,a_{n-1},x)\in\mathbb{A}^n(k) \
                | \ x\in k\setminus\{b_1,\ldots,b_m\}\}.
$$
Therefore $\mathbb{A}^n(k)\setminus V(F)$ is infinite.
</div>

Show $V(F)$ is infinite if $n\ge 2.$

<div class="proof">
Since $F$ is nonconstant there exists $i\in\{1,\ldots,n\}$ such that
$$
        G=F(a_1, \ldots,a_{i-1},X_i,a_{i+1}\ldots, a_n)\in k[X_i]
$$
is nonconstant, with arbitrary $a_1,\ldots,a_{i-1}a_{i+1}\ldots a_n\in k.$ Since
$k$ is algebraically closed and $G$
nonconstant, $G$ has a root in $k.$ Thus for each selection of $a_1,
\ldots, a_{i-1},a_{i+1},\ldots, a_n\in k,$ $k$ infinite, there is a distinct root
of $F$ in $\mathbb{A}^n(k).$ Thus $V(F)$ is infinite.
</div>

**1.15.\***Let $V\subset\mathbb{A}^n(k),$ $W\subset\mathbb{A}^m(k)$ be algebraic sets.
Show that
$$
        V\times W=\{(a_1,\ldots,a_n,b_1,\ldots,b_m) \ | \ (a_1,\ldots,a_n)\in V,
                (b_1,\ldots,b_m)\in W\}
$$
is an algebraic set.

<div class="proof">
Let $V=V(A),$ $W=V(B)$ with $A\subset k[X_1,\ldots,X_n],$ $B\subset k[X_1,
\ldots,X_m].$ Let us define $\varphi:k[X_1,\ldots,X_n]\to k[X_1,\ldots,X_{n+m}]$
by
$$
    \varphi(F)(X_1,\ldots,X_{n})=F(X_1,\ldots,X_n).
$$
Similarly define $\psi: k[X_1,\ldots,X_m]\to k[X_1,\ldots,X_{n+m}]$ by
$$
    \psi(F)(X_1,\ldots,X_{m})=F(X_{n+1},\ldots,X_{n+m}).
$$
It is simple to see
$$
        V\times W=V(\varphi(A) \cup \psi(B)).
$$
</div>
