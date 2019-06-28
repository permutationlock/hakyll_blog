---
documentclass: article
title: Fulton 1.3
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**1.16.\*** Let $V,W$ be algebraic sets in $\mathbb{A}^n(k).$ Show that $V=W$ if
and only if $I(V)=I(W).$

<div class="proof">
Suppose $V=W.$ Then by (6), $I(V)=I(W).$ Conversely, suppose $I(V)=I(W).$ Then
by (9), $V(I(V))=V$ and $V(I(W))=W.$ By (3), since $I(V)=I(W),$
$V(I(V))=V(I(W)).$ Therefore $V=W.$
</div>

**1.17.\*** \(a\) Let $V$ be an algebraic set in $\mathbb{A}^n(k),$ $P\in\mathbb{A}^n(k)$
a point not in $V.$ Show that there is a polynomial $F\in k[X_1,\ldots,X_n]$
such that $F(Q)=0$ for all $Q\in V,$ but $F(P)=1.$

<div class="proof">
Since $P\not\in V,$ there exists $F\in I(V)$ such that $F(P)\ne 0.$ Moreover,
$F(Q)=0$ for all $Q\in V$ by definition of $I(V).$ Since $k$ is a field there
exists $(F(P))^{-1}\in k$ such that $(F(P))^{-1}F(P)=1.$ Let $G=(F(P))^{-1}F.$
Observe $G(Q)=(F(P))^{-1}F(Q)=0$ for all $Q\in V$ and $G(P)=(F(P))^{-1}F(P)=1.$
</div>

\(b\) Let $P_1,\ldots,P_r$ be distinct points in $\mathbb{A}^n(k),$ not in an algebraic
set $V.$ Show that there are polynomials $F_1,\ldots,F_r\in I(V)$ such that
$F_i(P_j)=0$ if $i\ne j,$ and $F_i(P_i)=1.$

<div class="proof">
Let $i\in \{1,\ldots,r\}.$ Let $W_i=V\cup\{P_1,\ldots,P_r\}\setminus\{P_i\}.$
Since $P_i\not\in W_i,$ by \(a\) there exists $F_i\in I(W_i)$ such that $F_i(P_j)=
0$ for all $j\ne i,$ and $F_i(P_i)=1.$
</div>

\(c\) With $P_1,\ldots,P_r$ and $V$ as in (b), and $a_{ij}\in k$ for $1\le i,j\le
r,$ show that there are $G_i\in I(V)$ with $G_i(P_j)=a_{ij}$ for all $i$ and
$j.$

<div class="proof">
Let $F_1,\ldots,F_r$ be as in (b). For each $i\in\{1,\ldots,r\}$ define $G_i=
\sum_{j=1}^ra_{ij}F_j.$ Then for $i,j\in\{1,\ldots,r\},$
$$
        G_i(P_j)=\sum_{l=1}^ra_{il}F_l(P_j)=a_{ij}.
$$
</div>

**1.18.\*** Let $I$ be an ideal in a ring $R.$ If $a^n\in I,$ $b^m\in I,$
show that $(a+b)^{n+m}\in I.$

<div class="proof">
By the binomial theorem observe
$$
        (a+b)^{n+m}=\sum_{i=0}^{n+m}\binom{n+m}{i}a^{n+m-i}b^{i}.
$$
If $i<m$ then $n+m-i>n.$ Therefore each term of the sum has either $a^n$ or
$b^m$ as a factor. Since $a^n,b^m\in I,$ $(a+b)^{n+m}\in I.$
</div>

Show that $\text{Rad}(I)$ is an ideal, in fact a radical ideal.

<div class="proof">
Suppose $a,b\in\text{Rad}(I),$ that is, suppose $a^n,b^m\in I$ for some $n,m\in\mathbb{N}.$
By our result above, $(a+b)^{n+m}\in I.$ Thus $a+b\in\text{Rad}(I).$ Moreover if $a\in
\text{Rad}(I),$ then $-a\in\text{Rad}(I).$ Thus $\text{Rad}(I)$ is an abelian group.

Now suppose $a\in\text{Rad}(I),$ with $a^n\in I,$ $n\in\mathbb{N}.$ Let $r\in R.$ Clearly
$(ra)^n=r^na^n\in I$ and thus $ra\in\text{Rad}(I).$ Thus $\text{Rad}(I)$ is an ideal.

Finally, suppose $r\in R$ such that $r^n\in\text{Rad}(I),$ $n\in\mathbb{N}.$ Then $(r^n)^m\in
I$ for some $m\in\mathbb{N}.$ So $r^{nm}\in I$ and $r\in\text{Rad}(I).$ Thus $\text{Rad}(I)$ is a
radical ideal.
</div>

**1.19.** Show that $I=(X^2+1)\subset\mathbb{R}[X]$ is a radical (even a prime)
ideal, but $I$ is not the ideal of any set in $\mathbb{A}^1(\mathbb{R}).$

<div class="proof">
Note $X^2+1$ is irreducible as it is degree $2$ and has no roots in $\mathbb{R}.$
Therefore $(X^2+1)$ is a prime ideal. Moreover, since $X^2+1$ has no roots in
$\mathbb{R},$ $X^2+1\not\in I(V(S))$ for any $S\subset\mathbb{A}^1(\mathbb{R}),$ $S\ne\emptyset.$
Additionally, $I(V(\emptyset))=\mathbb{R}[X]\ne(X^2+1).$ Thus $(X^2+1)$ is not the
ideal of any set in $\mathbb{A}^1(\mathbb{R}).$
</div>

**1.20.\*** Show that for any ideal $I$ in $k[X_1,\ldots,X_n],$ $V(I)=V(
\text{Rad}(I)),$ and $\text{Rad}(I)\subset I(V(I)).$

<div class="proof">
It is clear $I\subset\text{Rad}(I),$ so $V(\text{Rad}(I))\subset V(I).$ Suppose $P\in
V(I).$ Let $F\in\text{Rad}(I).$ Then $F^n\in I.$ Therefore $(F(P))^n=0.$ Thus $F(P)
=0,$ since there are no zero divisors in $k.$ Thus $P\in V(\text{Rad}(I)).$
So $V(I)=V(\text{Rad}(I)).$

Observe by the result above we have $I(V(I))=I(V(\text{Rad}(I))).$ Therefore by (8),
$\text{Rad}(I)\subset I(V(I)).$
</div>

**1.21.\*** Show that $I=(X_1-a_1,\ldots,X_n-a_n)\subset k[X_1,\ldots,X_n]$
is a maximal ideal, and that the natural homomorphism from $k$ to $k[X_1,\ldots,
X_n]/I$ is an isomorphism.

<div class="proof">
Suppose $J$ is an ideal such that $I\subset J.$ Let $F\in J.$ By Problem 1.7 we
may write
$$
        F=\sum\lambda_(i)(X_1-a_1)^{i_1}\ldots(X-a_n)^{i_n}.
$$
If $F$ has no constant term clearly $F\in I.$ Suppose $F\not\in I,$ that is,
suppose $F$ has a nonzero constant term $c\in k.$ Then we may write $F=G+c$ with
$G\in I.$ Thus $F-G=c\in J.$ So $1\in J$ and $J=k[X_1,\ldots,X_n].$
Therefore $J=I$ or $J=k[X_1,\ldots,X_n].$ So $I$ is maximal.

Let $\varphi:k\to k[X_1,\ldots,X_n]/I$ be the natural homomorphism. Let
$F+I\in k[X_1,\ldots,X_n]/I.$ By the same argument above we may write $F=G+c$
with $G\in I,$ $c\in k.$ Thus $F+I=c+I=\varphi(c).$ So $\varphi$ is surjective.
Moreover, $\ker\varphi=I\cap k=\{0\}.$ Thus $\varphi$ is injective. Hence $\varphi$ is
an isomorphism.
</div>
