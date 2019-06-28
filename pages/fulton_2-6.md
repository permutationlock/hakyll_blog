---
documentclass: article
title: Fulton 2.6
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.33.** Factor $Y^3-2XY^2+2X^2Y+X^3$ into linear factors in $\mathbb{C}[X,Y].$

We note that factoring $F(X,Y)=Y^3-2XY^2+2X^2Y+X^3$ is the same as factoring
$F(X,1)$ or $F(1,Y).$ Unfortunately, neither factors nicely, so we will
simply note that $F(X,Y)=(X-Y\lambda_1)(X-Y\lambda_2)(X-Y\lambda_3),$
where $\lambda_1,\lambda_2,\lambda_3$ are the roots to $F(X,1)$ in $\mathbb{C}.$

**2.34.** Suppose $F,G\in k[X_1,\ldots,X_n]$ are forms of degree $r,r+1$
respectively, with no common factors ($k$ a field). Show that $F+G$ is irreducible.

<div class="proof">
Suppose $F+G=HJ$ for some $H,J\in k[X_1,\ldots,X_n].$ By Proposition 5,
$(F+G)^*=X_{n+1}F+G=H^*G^*=(HG)^*.$ Since $X_{n+1}F+G$ is degree 1 as a polynomial in $X_{n+1},$
one of $H^*,G^*$ must be of degree $0$ as a polynomial in $X_{n+1}.$ Therefore one
of $H^*,G^*$ divides $F$ and $G$ and is therefore constant, since $F,G$ share no
common factors. Thus one of $H,G$ are constant. Hence $F+G$ is irreducible.
</div>

**2.35.\*** \(a\) Show that there are $d+1$ monomials of degree $d$ in $R[X,Y],$
and $1+2+\ldots+(d+1)=(d+1)(d+2)/2$ monomials of degree $d$ in $R[X,Y,Z].$

<div class="proof">
A monomial of degree $d$ in $R[X,Y]$ is of the form $X^iY^j,$ $i+j=d.$
There are $d+1$ options for $i$ and $j=d-i.$

A monomial of degree $d$ in $R[X,Y,Z]$ is of the form $F_iZ^j$ where $F_i$ is
a monomial of degree $i$ in $R[X,Y]$ and $i+j=d.$ There are $i+1$ monomials
of degree $i$ in $R[X,Y].$ Thus there are $1+2+\ldots+(d+1)=(d+1)(d+2)/2$ monomials
of degree $d$ in $R[X,Y,Z].$
</div>

\(b\) Let $V(d,n)=\{\text{forms of degree } d \text{ in } k[X_1,\ldots,X_n]\},$
$k$ a field. Show that $V(d,n)$ is a vector space over $k,$ and the monomials of
degree $d$ form a basis.

<div class="proof">
It is immediate that $V(d,n)$ is a vector space with the natural addition and
scalar multiplication operations. Clearly the monomials of degree $d$ span
$V(d,n)$ and are linearly indpendent over $k.$ Thus they form a basis.
</div>

\(c\) Let $L_1,L_2,\ldots$ and $M_1,M_2,\ldots$ be sequences of nonzero linear
forms in $k[X,Y],$ and assume no $L_i=\lambda M_j,$ $\lambda\in k.$ Let
$A_{ij}=L_1L_2\ldots L_iM_1M_2\ldots M_j,$ $i,j\ge 0$ ($A_{00}=1$). Show that
$\{A_{ij} \, | \, i+j=d\}$ forms a basis for $V(d,2).$

It suffices to show that $\{A_{ij} \, | \, i+j=d\}$ is a linearly independent
set.

<div class="proof">
Suppose to the contrary that
$$
	0= \sum_{i+j=d} a_{ij}L_1\ldots L_i M_1\ldots M_j, \ a_{ij}\in k\setminus\{0\}.
$$
Then
$$
	L_1\ldots L_d=\sum_{i+j=d, j>0} a_{ij}L_1\ldots L_i M_1\ldots M_j.
$$
So
$$
	L_1\ldots L_d=M_1\sum_{i+j={d-1}} a_{ij}L_1\ldots L_i M_2\ldots M_j.
$$
Thus, since $L_1,\ldots, L_d, M_1$ are all linear, $M_1=\lambda L_i,$ a
contradiction.
</div>

**2.36.** With the above notation, show that $\dim V(d,n)=\binom{d+n-1}{n-1}.$

<div class="proof">
There are $n$ variables, whose powers must add up to $d.$ This is counted by
$$
	\left(\binom{n}{d}\right)=\binom{d+n-1}{n-1}.
$$
</div>
