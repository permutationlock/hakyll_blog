---
documentclass: article
title: Very ample divisors on curves
tags: math algebra geometry arithmetic
published: December 11, 2018
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}
---

All of the sources that I have found characterizing very ample divisors reference
Hartshorne or otherwise use, in my opinion, "non-elementary" arguments. Here are some
"elementary" proofs that
I worked out with some fellow grad students.

Let $C$ be a projective non-singular irreducible curve over an algebraically
closed
field. Suppose that $D$ is a
divisor on $C$ such that $l(D)=n>0.$ Let
$$
    L(D)=\langle f_1,\ldots,f_n\rangle.
$$
Here $L(D)$ is the Riemann-Roch space of $D$ and $l(D)$ is the dimension of
$L(D)$.
We define the map $\phi_D:C\to\mathbb{P}^{n-1}$
by
$$
    \phi_D(P)=(f_1(P):\ldots:f_n(P)).
$$

**Lemma 1.** If $D$ and $D'$ are linearly equivalent divisors, then there is an
automorphism $\psi$ of $\mathbb{P}^{n-1}$ given by linear forms such that
$\phi_{D'}(C)=\psi(\phi_D(C)).$

<div class="proof">
Let $g\in k(C)^*$ be such that $D+\text{div}(g)=D'.$ Then the map $f\mapsto
gf$ gives an isomorphism $L(D)\to L(D').$

If $\phi_{D'}$ is defined by the basis $\{ gf_1,\ldots,gf_n\}$,
then
$$
\begin{aligned}[t]
    \phi_{D'}(P) &= (g(P)f_1(P),\ldots,g(P)f_n(P)) \\
                 &= (f_1(P),\ldots,f_n(P))         \\
                 &= \phi_D(P).
\end{aligned}
$$
By the isomorphism above, given any basis $\{h_1,\ldots,h_n\}$
for $L(D')$
the set $\{g^{-1}h_1,\ldots,g^{-1}h_n\}$ is a basis for $L(D).$ Thus there
exists a change of basis on $L(D)$ defined by linear forms
$T_i\in k[X_1,\ldots,X_n]_1$ such that
$T_i(f_1,\ldots,f_n)=g^{-1}h_i$ for each $i.$
Therefore $T$ also gives a projective linear map $\psi$ defined by
$$
    (x_1,\ldots,x_n)\mapsto (T_1(x_1,\ldots,x_n),\ldots,T_n(x_1,\ldots,x_n)).
$$
Moreover,
$\phi_{D'}(P)=\psi(\phi_{D}(P))$ as desired.
</div>

**Definition 2.** A divisor $D$ is said to be *very ample* if the map
$\phi_D$ gives an isomorphism
to its image.

**Theorem 3.** A divisor $D$ with $l(D)>0$ is very ample if and only if for
every two points $P,Q\in C$ we have
$$l(D-P-Q)=l(D)-2.$$

<div class="proof">
Suppose that $D$ is very ample. Let $P\in C.$ Pick $Q\in C$ such that $Q\ne P.$
Observe that by applying a linear automorphism to $\mathbb{P}^{n-1}$
(changing basis for $L(D)$)
we can assume that
$$
    \phi_D(P)=(1:0:\ldots:0), \ \phi_D(Q)=(0:1:0:\ldots:0).
$$
Let $\phi_D$ be defined by the basis $f_1,\ldots,f_n\in L(D).$ Then
$f_1(P)=1$ and $f_1(Q)=0.$ Since $D$ is effective,
we must have $f_1\in L(D)$ and
$f_1\not\in L(D-P).$ Moreover, $f_2(Q)=1$ and $f_2(P)=0.$ Thus similarly
we have $f_2\in
L(D-P)$ but $f_2\not\in L(D-P-Q).$ So
$$l(D-P-Q)=l(D)-2.$$
It remains to show that
$$l(D-2P)=l(D)-2.$$
Note we have already shown above that
$$l(D-P)=l(D)-1.$$
Suppose that
$$L(D-P)=L(D-2P).$$
Then every $f\in L(D)$
that has a zero at $P$ must have a double zero at $P.$ Note that
$x_i/x_1$ must be a local parameter at $\phi_D(P)$ on $\phi_D(C)$ for
some $i\in\{2,\ldots,n\}$ since
$\{ x_i \, : \, 2\le i\le n\}$ generate $\mathfrak{m}_{\phi_D(P)}.$ But
by our assumptions above
it must be that
$$
    \text{ord}_P(\phi_D^*(x_i/x_1))=\text{ord}_P(f_i/f_1)\ge 2.
$$
This is a
contradiction to $\phi_D$ being an isomorphism.

Conversely suppose that
$$l(D-P-Q)=l(D)-2$$
for all $P,Q\in C.$
We will first show that $\phi_D$ is injective. Let $P,Q\in
D$ and assume $P,Q\not\in C.$ By our assumption we may pick
$$f_1\in L(D-P)\setminus L(D-P-Q)$$
and therefore
$f_1(P)=0$ and $f_1(Q)\ne 0.$ Thus
$\phi_D(P)\ne\phi_D(Q).$

It remains to show that $\phi_D(C)$ is nonsingular.
Again suppose that $P=(1:0:\ldots:0).$ Let us pick
$$f_1\in L(D)\setminus L(D-P)$$
and
$$f_2\in L(D-P)\setminus L(D-2P).$$
We may then
extend this to a basis $\{f_1,\ldots, f_n\}$ and note that $f_3,\ldots,f_n\in
L(D-2P).$

Note that we may find a non-singular model $X$ for $\phi_D(C).$ Therefore
there exists a birational map between $C$ and $X$ and hence a $\phi_D$ is
a birational map from $C$ to $\phi_D(C).$ So $\phi_D^*:k(\phi_D(C))\to k(C)$
is an isomorphism.

Observe that $\{ x_i/x_1 \, : \, 2\le i\le n\}$
generates $\mathfrak{m}_{\phi_D(P)}.$ Moreover, for $i\ge 3$ we have
$$
    \text{ord}_P(\phi_D^*(x_i/x_1))=\text{ord}_P(f_i/f_1)>1
$$
and
$$
    \text{ord}_P(\phi_D^*(x_2/x_1))=\text{ord}_P(f_2/f_1)=1.
$$
Therefore for $i\ge 3$ we have
$$
    \phi_D^*(x_i/x_1)=\phi_D^*(u)\phi_D^*(x_2/x_1)^m
$$
where $m>1$ and $u$ is a unit in $\mathcal{O}_{\phi_D(P)}.$ So
$x_i/x_1=u(x_2/x_1)^m$ and thus $x_i/x_1$ has order $m$ at $\phi_D(P)$. Thus
$\{x_2/x_1\}$ is a basis for $\mathfrak{m}_{\phi_D(P)}/\mathfrak{m}_{\phi_D(P)}^2$ and
$\phi_D(C)$ is nonsingular at $\phi_D(P).$ Thus $\phi_D(C)$ is nonsingular and
$\phi_D$ defines an isomorphism.
</div>
