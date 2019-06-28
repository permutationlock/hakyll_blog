---
documentclass: article
title: Fulton 2.1
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.1.\*** Show the map that associates each $F\in k[X_1,\ldots,X_n]$ to a
polynomial function in $\mathcal{F}(V,k)$ is a ring homomorphism with kernel
$I(V).$

<div class="proof">
Clearly the map is a ring homorphism. Suppose $F\in k[X_1,\ldots,X_n]$ such that
$F$ is the zero function in $\mathcal{F}(V,k).$ Then $F(P)=0$ for all $P\in V.$
Thus $F\in I(V).$ Similarly, if $F\in I(V)$ then $F$ is the zero function.
</div>

**2.2.\*** Let $V\subset\mathbb{A}^n$ be a variety. A *subvariety* of $V$
is a variety $W\subset\mathbb{A}^n$ that is contained in $V.$ Show that there is a
natural one-to-one correspondence between algebraic subsets (resp. subvarieties,
resp. points) of $V$ and radical ideals (resp. prime ideals, resp. maximal
ideals) of $\Gamma(V).$

<div class="proof">
Recall there is a one to one correspondence between radical, prime, and
maximal ideals $J/I(V)$ in $\Gamma(V)$ and radical, prime, and maximal ideals
$J$ in $k[X_1,\ldots,X_n]$ such that $I(V)\subset J.$
Observe that that $W$ is an algebraic subset, subvariety, or point of $V,$ if and
only if $I(W)$ is a radical, prime, or maximal ideal in $k[X_1,\ldots,X_n]$
with $I(V)\subset I(W).$
</div>

**2.3.\*** Let $W$ be a subvariety of a variety $V,$ and let $I_V(W)$ be
the ideal of $\Gamma(V)$ corresponding to $W.$

\(a\) Show that every polynomial function on $V$ restricts to a polynomial
function on $W.$

<div class="proof">
Suppose $f\in\mathcal{F}(V,k)$ is a polynomial function. Then there exists
$F\in k[X_1,\ldots,X_n]$ such that $f(a_1,\ldots,a_n)=F(a_1,\ldots,a_n)$ for
all $(a_1,\ldots,a_n)\in V.$ Since $W\subset V,$ $f|_W\in\mathcal{F}(W,k)$ is
polynomial as well. In terms of coordinate rings, this restriction map defines
a homomorphism $\varphi:\Gamma(V)\to\Gamma(W)$ where $\varphi(F+I(V))=F+I(W).$
</div>

\(b\) Show the map $\varphi:\Gamma(V)\to\Gamma(W)$ defined in \(a\) is surjective with
kernel $I_V(W),$ so $\Gamma(W)\cong\Gamma(V)/I_V(W).$

<div class="proof">
Let $\pi_V:k[X_1,\ldots,X_n]\to \Gamma(V)$ and $\pi_W:k[X_1,\ldots,X_n]\to \Gamma(W)$
be the natural homomorphisms. Since $\pi_W,\pi_V$ are surjective and
$\ker\pi_V\subset\ker\pi_W,$ by descending to the quotient ([Lemma
1.1](/pages/fulton_1-lemmas.html))
there exists a surjective homomorphism $\widetilde{\pi_W}:\Gamma(V)\to\Gamma(W)$
with $\ker\widetilde{\pi_W}=\pi_W(I(V))=I_V(W).$ Moreover,
$\widetilde{\pi_W}(F+I(V))=\pi_W(F)=F+I(W)=\varphi(F+I(V)).$ So
$\widetilde{\pi_W}=\varphi.$
</div>

**2.4.\*** Let $V\subset\mathbb{A}^n$ be a nonempty variety. Show the following are
equivalent:
 (1) $V$ is a point;
 (2) $\Gamma(V)=k$;
 (3) $\dim_k\Gamma(V)<\infty.$

<div class="proof">
*(1) $\Leftrightarrow$ (2).* Suppose $V=\{(a_1,\ldots,a_n)\}\subset\mathbb{A}^n.$ Then
$I(V)=(X_1-a_1,\ldots,X_n-a_n).$ Therefore $\Gamma(V)=k.$ Conversely, suppose
$\Gamma(V)=k.$ Then $I(V)$ is maximal. Since
$k$ is algebraically closed, $I(V)=(X-a_1,\ldots,X-a_n).$ Thus $V$ is a point.

*(2) $\Leftrightarrow$ (3).* Clearly if $\Gamma(V)=k,$ then
$\dim_k\Gamma(V)=1.$ Conversely, suppose $\dim_k\Gamma(V)<\infty.$ Let $L$ be
the field of fractions
for $\Gamma(V).$ By [Lemma 2.1](/pages/fulton_2-lemmas.html), $L$ is
finite field extension of $k.$ Since
$k$ is algebraically closed, by Problem 1.48, $L=k.$ Thus $\Gamma(V)=L=k.$
</div>

**2.5.** Let $F$ be an irreducible polynomial in $k[X,Y],$ and suppose $F$
is monic in $Y$: $F=Y^n+a_1(X)Y^{n-1}+\ldots+a_n(X),$ with $n>0.$ Let
$V=V(F)\subset\mathbb{A}^2.$ Show that the natural homomorphism from $k[X]$ to
$\Gamma(V)=k[X,Y]/I(F)$ is one-to-one, so that $k[X]$ may be regarded as a
subring of $\Gamma(V)$; show that the residues $\overline{1},\overline{Y},\ldots,
\overline{Y}^n-1$ generate $\Gamma(V)$ over $k[X]$ as a module.

<div class="proof">
Observe that if $n=0,$ then $F(X,Y)=1$ is constant, $(F)=k[X,Y],$ and
$k[X,Y]/(F)$ is the trivial ring. Thus suppose $n\ge 1.$

Note that the natural homomorphism $k[X]\to\Gamma(V)$ maps $G\mapsto G+(F).$
Suppose $G,H\in k[X]$ such that $H+(F)=G+(F),$ that is, $H-G\in (F).$ Observe
that $F\nmid H-G,$ since $n\ge 1,$ so $G-H=0$ and $G=H.$ So the map is one-to-one.

Let $G+(F)\in\Gamma(V).$ Observe we have $Y^n+(F)=-a_1(X)Y^{n-1}-\ldots-a_0(X)+(F).$
Hence for any $m>0,$ $Y^m+(F)=\sum_{i=0}^{n-1}b_{m,i}(X)Y^i$ for some $b_{m,i}\in
k[X].$ Thus $\overline{1},\overline{Y},\ldots,
\overline{Y}^{n-1}$ generate $\Gamma(V)$ over $k[X]$ as a module.
</div>
