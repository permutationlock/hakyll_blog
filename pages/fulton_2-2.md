---
documentclass: article
title: Fulton 2.2
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**Bonus problem 2.a.** Show the map
$\widetilde{\varphi}:\mathcal{F}(W,k)\to\mathcal{F}(V,k)$ is a homomorphism.

<div class="proof">
Observe that for any $f,g\in\mathcal{F}(W,k)$ and any $x\in W,$
$$
\begin{aligned}[t]
((f+g)\circ\varphi)(x) &= f(\varphi(x))+g(\varphi(x))\\
&= (f\circ\varphi)(x)+(g\circ\varphi)(x)\\
&= ((f\circ\varphi)+(g\circ\varphi))(x).
\end{aligned}
$$
Therefore $\widetilde{\varphi}(f+g)=\widetilde{\varphi}(f)+\widetilde{\varphi}(g).$
Similarly, $\widetilde{\varphi}(fg)=\widetilde{\varphi}(f)\widetilde{\varphi}(g).$
</div>

**Bonus problem 2.b.** If $T_1,\ldots,T_m\in k[X_1,\ldots,X_n]$ determine a
polynomial map $T:\mathbb{A}^n\to\mathbb{A}^m,$ the $T_i$ are uniquely determined by $T.$

<div class="proof">
Suppose $F\in k[X_1,\ldots,X_n]$ such that $F(P)=T_i(P)$ for all $P\in\mathbb{A}^n.$
Then $(F-T_i)(P)=0$ for all $P\in\mathbb{A}^n.$ So $F=T_i$ by Problem 1.4.
</div>

**Bonus problem 2.c.** Let $V\subset\mathbb{A}^n,W\subset\mathbb{A}^m$ be a varieties. Let
$\varphi:V\to W$ be a polynomial map. Then $\widetilde{\varphi}:\Gamma(W)\to\Gamma(V)$
is a homomorphism. Moreover, $F+I(W)\mapsto F\circ\varphi+I(V).$

<div class="proof">
Suppose $\varphi$ is defined by the polynomials $T_1,\ldots,T_m\in k[X_1,\ldots,X_n].$
For any polynomial $F\in k[X_1,\ldots,X_m],$
$\widetilde{\varphi}(F)=F\circ\varphi=F(T_1,\ldots,T_m)$ is a polynomial in
$k[X_1,\ldots,X_n].$

We will now show that $\widetilde{\varphi}$ induces a well defined homomorphism
$\Gamma(W)\to\Gamma(V),$ that is, if $F+I(W),G+I(W)\in\Gamma(W)$ then
$\widetilde{\varphi}(F)+I(V)=\widetilde{\varphi}(G)+I(V).$ Let $H\in I(W).$ Observe
that $(H\circ\varphi)(a_1,\ldots,a_n)=0$ for all $(a_1,\ldots,a_n)\in V.$
Thus $\widetilde{\varphi}(H)\in I(V).$ Thus, by Problem 2.a, the map is a well
defined homomorphism.

Although a slight abuse of notation, we use $\widetilde{\varphi}$ to
refer to the homomorphism $\Gamma(W)\to\Gamma(V)$ induced by $\widetilde{\varphi}.$
This falls in line with the dual use of $\Gamma(V)$ as both the
ring $k[X_1,\ldots,X_n]/I(V)$ and the set of all polynomial maps in
$\mathcal{F}(V,k).$
</div>

**Note.** Recall that Fulton assumes all homomorphisms fix the field $k.$
That is, the following statement is a bijection between polynomial maps
and homomorphisms of coordinate rings that fix $k.$

**Proposition 1.** Let $V\subset\mathbb{A}^n,$ $W\subset\mathbb{A}^m$ be affine
varieties. There is a one-to-one correspondence between the polynomial maps
$\varphi:V\to W$ and the homomorphisms $\widetilde{\varphi}:\Gamma(W)\to\Gamma(V).$
Any such $\varphi$ is the restriction of a polynomial map $\mathbb{A}^n\to\mathbb{A}^m.$

<div class="proof">
By Problem 2.c, each polynomial map $V\to W$ induces a homomorphism
$\Gamma(W)\to\Gamma(V).$ Moreover, any polynomial map $V\to W$ is the
restriciton of a polynomial map $\mathbb{A}^n\to\mathbb{A}^m.$ Thus it remains to prove
the converse.

Let $\alpha:\Gamma(W)\to\Gamma(V)$ be a homomorphism. For each
$i\in\{1,\ldots,m\}$ select $T_i\in k[X_1,\ldots,X_n]$
such that $T_i+I(V)=\alpha(X_i+I(W)).$ Let $T:\mathbb{A}^n\to\mathbb{A}^m$ be the polynomial
map defined by $(T_1,\ldots,T_m).$ Let $\widetilde{T}:k[X_1,\ldots,X_m]\to
k[X_1,\ldots,X_n]$ be the homomorphism defined by $F\mapsto F\circ T.$ Let
$\pi_V:k[X_1,\ldots,X_n]\to\Gamma(V)$ and $\pi_W:k[X_1,\ldots,X_m]\to\Gamma(W)$
be the natural homomorphisms.
$$
\xymatrix{
    k[X_1,\ldots, X_m] \ar[r]^{\widetilde{T}}
    \ar[d]^{\pi_W}
        & k[X_1,\ldots,X_n] \ar[d]^{\pi_V}\\
    \Gamma(W) \ar[r]^{\alpha} & \Gamma(V)
}
$$
Observe that
$$
\begin{aligned}[t]
\pi_V(\widetilde{T}(F)) &= \pi_V(F(T_1,\ldots,T_m))\\
&= F(T_1,\ldots,T_m)+I(V)\\
&= F(\alpha(X_1+I(W)),\ldots,\alpha(X_m+I(W)))+I(V)\\
&= \alpha(F+I(W))\\
&= \alpha(\pi_W(F)).
\end{aligned}
$$
Thus the diagram above commutes. In particular, $\pi_V(\widetilde{T}(I(W)))=
\alpha(\pi_W(I(W)))=0+I(V).$ Thus $\widetilde{T}(I(W))\subset I(V).$

Let $(a_1,\ldots,a_n)\in\mathbb{A}^n.$ Suppose $F\in I(W).$ Then $F(T(a_1,\ldots,a_n))=
\widetilde{T}(F)(a_1,\ldots,a_n)=0,$ since $\widetilde{T}(F)\in I(V).$ Thus
$T(a_1,\ldots,a_n)\in W.$

Observe that $\widetilde{T|_V}$ is the same as the homomorphism resulting
from applying [Lemma 1.1](/pages/fulton_1-lemmas.html)
to $\pi_W$ and $\pi_V\circ\widetilde{T}.$ Thus
$T|_V:V\to W$ is a polynomial map such that $\widetilde{T|_V}=\alpha.$
</div>

**Corollary.** Let $V,W$ be affine varieties. Then $V$ is isomorphic to
$W$ if and only if $\Gamma(V)$ is isomorphic to $\Gamma(W).$

<div class="proof">
Suppose $V\cong W,$ that is, there exist polynomial maps $\varphi:V\to W$ and
$\psi:W\to V$ such that $\psi\circ\varphi=\text{id}_V$ and $\varphi\circ\psi=\text{id}_W.$ Note
that $\text{id}_{\Gamma(V)}=\widetilde{\text{id}_V}=\widetilde{\psi\circ\varphi}.$ Thus by
Problem 2.6, $\text{id}_{\Gamma(V)}=\widetilde{\psi\circ\varphi}
=\widetilde{\varphi}\circ\widetilde{\psi}.$ Similarly, $\text{id}_{\Gamma(W)}=
\widetilde{\psi}\circ\widetilde{\varphi}.$ Thus $\widetilde{\varphi},\widetilde{\psi}$
are isomorphisms and $\Gamma(V)\cong\Gamma(W).$

Conversely, suppose $\Gamma(V)\cong\Gamma(W),$ that is, there exist
isomorphisms $\widetilde{\varphi}:\Gamma(W)\to\Gamma(V)$ and $\widetilde{\psi}:
\Gamma(V)\to\Gamma(W)$ such that $\widetilde{\varphi}\circ\widetilde{\psi}=
\text{id}_{\Gamma(V)}$ and $\widetilde{\psi}\circ\widetilde{\varphi}=\text{id}_{\Gamma(W)}.$
By Proposition 1, $\widetilde{\varphi},\widetilde{\psi}$ are induced by some
polynomial maps $\varphi:V\to W$ and $\psi:W\to V.$ Since $\widetilde{\varphi}\circ
\widetilde{\psi}=\text{id}_{\Gamma(V)},$ $F\circ(\psi\circ\varphi)+I(V)=F+I(V)$ for any
$F+I(V)\in\Gamma(V).$ In particular, this is true for $\sum_{i=1}^nX_i+I(V).$
Thus $(\psi\circ\varphi)(a_1,\ldots,a_n)=(a_1,\ldots,a_n)+F(a_1,\ldots,a_n)$ for
some $F\in I(V).$ Thus $(\psi\circ\varphi)(a_1,\ldots,a_n)=(a_1,\ldots,a_n)$ for
every $(a_1,\ldots,a_n)\in V.$ So $\psi\circ\varphi=\text{id}_{V}.$ A similar argument
shows $\varphi\circ\psi=\text{id}_{W},$ and therefore $V\cong W.$
</div>

**2.6.\*** Let $\varphi:V\to W,$ $\psi:W\to Z.$ Show that
$\widetilde{\psi\circ\varphi}=\widetilde{\varphi}\circ\widetilde{\psi}.$ Show that
the composition of a polynomial map is a polynomial map.

<div class="proof">
Let $\varphi:V\to W,$ $\psi:W\to Z.$ Let $f\in\mathcal{F}(Z,k).$
Observe that
$$
\begin{aligned}[t]
\widetilde{\psi\circ\varphi}(f) &= f\circ(\psi\circ\varphi)\\
&= (f\circ\psi)\circ\varphi\\
&= \widetilde{\varphi}(f\circ\psi)\\
&= \widetilde{\varphi}(\widetilde{\psi}(f))\\
&= (\widetilde{\varphi}\circ\widetilde{\psi})(f).
\end{aligned}
$$
Therefore $\widetilde{\psi\circ\varphi}=\widetilde{\varphi}\circ\widetilde{\psi}.$
It is simple to verify that composing polynomial maps results in a polynomial
map.
</div>

**2.7.\*** If $\varphi:V\to W$ is a polynomial map, and $X$ is an algebraic
subset of $W,$ show that $\varphi^{-1}(X)$ is an algebraic subset of $V.$ If
$\varphi^{-1}(X)$ is irreducible, and $X$ is contained in the image of $\varphi,$
show that $X$ is irreducible. This gives a useful test for irreducibility.

<div class="proof">
Observe that if $P\in\varphi^{-1}(X)$ then $\varphi(P)\in X.$ Thus
$F(\varphi(P))=0$ for all $F\in I(X).$ Moreover, if $F(\varphi(P))=0,$ then
$\varphi(P)\in X,$ and $P\in\varphi^{-1}(X).$ So
$\varphi^{-1}(X)=V(\widetilde{\varphi}(I(X))).$

Suppose that $\varphi^{-1}(X)$ is irreducible and $X\subset\varphi(V).$ Suppose that
$I_W(X)$ is not prime, that is,
there exist $f,g\in\Gamma(W)\setminus I_W(X)$ such that $fg\in
I_W(X).$ Since $f,g\not\in I_W(X)$ there exist $P,Q\in X$ such that
$f(P)\ne 0,$ $g(Q)\ne 0.$ Since $X\subset\varphi(V),$ there exist
$P',Q'\in\varphi^{-1}(V)$ such that $\varphi(P')=P,$ $\varphi(Q')=Q.$ Therefore
$(f\circ\varphi)(P')=f(P)\ne 0$ and $\widetilde{\varphi}(f)\not\in I_V(\varphi^{-1}(X)).$
Similarly, $\widetilde{\varphi}(g)\not\in I_V(\varphi^{-1}(X)).$ But,
$\widetilde{\varphi}(f)\widetilde{\varphi}(g)=\widetilde{\varphi}(fg)\in I_V(\varphi^{-1}(X)).$
So $I_V(\varphi^{-1}(X))$ is not prime, a contradiction to the
assumption that $\varphi^{-1}(X)$ is irreducible.
</div>

**2.8.** \(a\) Show that $\{t,t^2,t^3\}\subset\mathbb{A}^3(k)$ is an affine
variety.

<div class="proof">
Let $\varphi:\mathbb{A}^1(k)\to\mathbb{A}^3(k)$ be defined by $t\mapsto (t,t^2,t^3).$
Observe $\varphi$ is a polynomial map. Let $V=V(X-Y^2, X-Z^3)=\{(t,t^2,t^3)\in\mathbb{A}^3(k)\}.$
Note that $\varphi(\mathbb{A}^1(k))=V$ and $\varphi^{-1}(V)=\mathbb{A}^1(k).$ Recall
$\mathbb{A}^1(k)$ is irreducible by Problem 1.29. Therefore $V$ is irreducible by
Problem 2.7.
</div>

\(b\) Show that $V(XZ-Y^2, YZ-X^3, Z^2-X^2Y)\subset\mathbb{A}^3(\mathbb{C})$ is a variety.

<div class="proof">
Let $V=V(XZ-Y^2, YZ-X^3, Z^2-X^2Y).$
It is simple to compute that $Y^3-X^4, Z^3-X^5, z^4-Y^5\in I(V).$ Suppose
$(a,b,c)\in V.$ Then $b^3=a^4,$ that is, $b=t^4$ whenever $t$ is a third root
of $a$ in $\mathbb{C}.$ Similalry, $c=s^5$ where $s$ is a third root of $a.$ Since $c^4=b^5$
we have $s^{20}=t^{20}.$ But $t^3=a=s^3.$ Thus $t(s^{20})=t(t^{20})=(t^{3})^{7}=
(s^{3})^7=s(s^{20}).$ So $t=s$ and $(a,b,c)=(t^3,t^4,t^5).$ Moreover, for any
$t\in \mathbb{C},$ observe that $(t^3,t^4,t^5)\in V.$ Let
$\varphi:\mathbb{A}^1(\mathbb{C})\to \mathbb{A}^3(\mathbb{C})$ be the polynomial map defined by
$t\mapsto(t^3,t^4,t^5).$ Then $V=\varphi(\mathbb{A}^1(\mathbb{C}))$ and thus $V$ is
irreducible by Problem 2.7.
</div>

**2.9.\*** Let $\varphi:V\to W$ be a polynomial map of affine varieties, and
let $V'\subset V,$ $W'\subset  W$ be subvarieites. Suppose $\varphi(V')\subset W'.$

\(a\) Show that $\widetilde{\varphi}(I_W(W'))\subset I_V(V').$

<div class="proof">
Suppose $f\in I_W(W').$ Then $f(P)=0$ for all $P\in\varphi(V')\subset W'.$
Thus $\widetilde{\varphi}(f)\in I_V(V').$
</div>

\(b\) Show that the restriction of $\varphi$ gives a polynomial map $V'\to W'.$

<div class="proof">
The restriction of any polynomial map is a polynomial map by the proof of Proposition
1. Thus since $\varphi(V')\subset W',$ $\varphi|_{V'}$ is a polynomial map
$V'\to W'.$
</div>

**2.10.\*** Show that the projection map $\mathbb{A}^n\to\mathbb{A}^r$ with $n\ge r$
is a polynomial map.

<div class="proof">
The projection map is defined by the polynomials $T_i=X_i.$
</div>

**2.11.** Let $f\in\Gamma(V),$ $V\subset\mathbb{A}^n$ a variety. Define
$$
    G(f)=\{(a_1,\ldots,a_n,a_{n+1})\in\mathbb{A}^{n+1} \ | \ (a_1,\ldots,a_n)\in V
        \text{ and } a_{n+1}=f(a_1,\ldots,a_n)\},
$$
the *graph* of $f.$ Show that $G(f)$ is an affine variety, and the map
$$
    (a_1,\ldots,a_n)\mapsto (a_1,\ldots,a_n,f(a_1,\ldots,a_n))
$$
defines an isomorphism of $V$ with $G(f).$

<div class="proof">
Let $\varphi:\mathbb{A}^n\to G(f)$ be the map defined above. It is immediate that
$\phi$ is a bijection with inverse $\pi|_{G(f)},$ where $\pi:\mathbb{A}^{n+1}\to\mathbb{A}^n$ is the
projection map.  Since both are polynomial maps, $\varphi$ is an isomorphism.
</div>

**2.12.** \(a\) $\varphi:\mathbb{A}^1\to V=V(Y^2-X^3)\subset\mathbb{A}^2$ be defined by
$\varphi(t)=(t^2,t^3).$ Show that although $\varphi$ is a one-to-one, onto
polynomial map, $\varphi$ is not an isomorphism.

<div class="proof">
Observe if $(a,b)\in V$ the $a^3=b^2.$ Since $k$ is algebraically closed, there
exists $t\in k$ such that $t^2=a.$ So $b^2=t^6$ and $b=\pm t^3.$ Thus either
$\varphi(t)=(a,b)$ or $\varphi(-t)=(a,b).$ So $\varphi$ is onto.

Suppose $t,s\in\mathbb{A}^1$ such that $\varphi(t)=\varphi(s).$ Then $t^2=s^2$ and
$t^3=s^3.$ Thus $t(t^2)=s(s^2)=s(t^2),$ and $t=s.$ Therefore $\varphi$ is
one-to-one.

It remains to show that $\varphi$ is not an isomorphism, that is, its inverse
is not a polynomial map. Observe that $\widetilde{\varphi}(f)=f(T^2,t^3)$ for
every $f\in\Gamma(V).$ Therefore $\widetilde{\varphi}(\Gamma(V))\subset (T^2,T^3)
\subsetneq k[T]=\Gamma(\mathbb{A}^1).$ Thus $\widetilde{\varphi}:\Gamma(V)\to\Gamma(\mathbb{A}^1)$
is not an isomorphism. Hence $\varphi$ is not an isomorphism by Proposition 1.
</div>

\(b\) Let $\varphi:\mathbb{A}^1\to V=V(Y^2-X^2(X-1))$ be defined by
$\varphi(t)=(t^2-1,t(t^2-1)).$ Show that $\varphi$ is one-to-one and onto,
except that $\varphi(\pm 1)=(0,0).$

<div class="proof">
Let $(a,b)\in V.$ Then $b^2=a^2(a+1).$ Let $t\in k$ such that $t^2=a+1.$ Then
$a=t^2-1,$ $b=(t^2-1)t.$ Therefore $\varphi(t)=(a,b).$ So $\varphi$ is onto.

Suppose $t,s\in k$ such that $\varphi(t)=\varphi(s).$ Then $(t^2-1)=(s^2-1).$
Moreover, $(t^2-1)t=(s^2-1)s.$ If $t^2-1\ne 0,$ then $t=s.$ If $t^2-1=0,$ then
$t=\pm 1,$ $s=\pm 1.$ Therefore $\varphi$ is one-to-one and onto,
except that $\varphi(\pm 1)=(0,0).$
</div>

**2.13.** Let $V=V(X^2-Y^3, Y^2-Z^3)\subset\mathbb{A}^3$ as in Problem 1.40,
$\overline{\alpha}:\Gamma(V)\to k[T]$ induced by the homomorphism $\alpha$ of
that problem.

\(a\) What is the polynomial map $f$ from $\mathbb{A}^1$ to $V$ such that $\widetilde{f}
=\overline{\alpha}$?

<div class="proof">
Let $f:\mathbb{A}^1\to\mathbb{A}^3$ be the polynomial map $(T^9,T^6,T^3).$ Observe that
$f(\mathbb{A}^1)\subset V$ so $f:\mathbb{A}^1\to V$ is a polyomal map such that
$\widetilde{f}=\overline{\alpha}.$
</div>

\(b\) Show that $f$ is one-to-one and onto, but not an isomorphism.

<div class="proof">
First note that $f$ is clearly one-to-one. Suppose $(a,b,c)\in V.$ Let $t\in k$
such that $t^4=c.$ Then $b^2=c^3=t^{12},$ so $b=\pm t^6.$ Moreover,
$a^2=b^3=\pm t^{18},$ so $a=\pm t^{9},$ or $\pm t^{9}\sqrt{-1}.$ Therefore
$(a,b,c)$ is either $(\pm t^9,t^6,t^4),$ or
$(\pm t^9\sqrt{-1},-t^6,t^4).$ Thus $(a,b,c)$ is equal to either $f(\pm t)$
or $f(\pm t\sqrt{-1}).$ So $f$ is onto.

It remains to show that $f$ is not an isomorphism. By Proposition 1 it suffices
to observe that $\widetilde{f}$ is not an isomorphism, since
$\widetilde{f}(\Gamma(V))\subset (T^9, T^6, T^4)\subsetneq k[T]=\Gamma(\mathbb{A}^1).$
</div>
