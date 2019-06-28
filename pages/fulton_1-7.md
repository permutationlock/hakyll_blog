---
documentclass: article
title: Fulton 1.7
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**1.32.** Show that both the weak and strong Nullstellensatz and all of the
corollaries are false if $k$ is not algebraically closed.

For the Weak Nullstellensatz, observe $(X^2+1)\subset\mathbb{R}[X]$ is a proper ideal
but $V(X^2+1)=\emptyset.$

For the Strong Nullstellensatz, consider Problem 1.26. Let
$F=Y^2+X^2(X-1)^2\in\mathbb{R}[X,Y].$ Note $V(F)=\{(0,0),(1,0)\}.$ Thus
$I(V(F))=(X(X-1), Y)\ne\text{Rad}{(F)}=F.$ This is also a counterexample to Corollary
1.

For Corollaries 2 and 3 we again consider Problem 1.26. Observe $F=Y^2+X^2(X-1)^2$ is
irreducible, so $(F)$ is prime. But $V(F)=V(X, Y)\cup V(X-1,Y).$ Let $G=X^2+1.$
Additionally if $G=X^2+1,$ then $(G)$ is a maximal ideal but $V(G)=\emptyset.$

For Corollary 4 let $I=(Y^2+X(X-1)^2).$ Above we saw $V(I)=\{(0,0),(1,0)\}.$
However, $k[X,Y]/I$ has the infinite linearly independent subset
$\{\overline{1},\overline{Y},\overline{Y}^2,\ldots\}.$

**1.33** \(a\) Decompose $V(X^2+Y^2-1, X^2-Z^2-1)\subset\mathbb{A}^3(\mathbb{C})$ into
irreducible components.

Observe that $(X^2+Y^2-1) - (X^2-Z^2+1) = Y^2+Z^2.$ Therefore
$(Y+iZ)(Y-iZ)\in (X^2+Y^2+1, X^2-Z^2-1).$ If $Y+iZ=X^2+Y^2-1=0$ we have
$Y=-iZ.$ So $X^2-Z^2-1=0.$ The same is true if $Y-iZ=X^2+Y^2-1=0.$ Therefore
$V(X^2+Y^2-1, X^2-Z^2-1)=V(X^2+Y^2-1, Y+iZ)\cup V(X^2+Y^2, Y-iZ).$ Observe
$$
\begin{aligned}[t]
\mathbb{C}[X,Y,Z]/(X^2+Y^2-1, X+iZ)
&\cong (\mathbb{C}[X,Z]/(X+iZ))[Y]/(X^2+Y^2-1)\\
&\cong \mathbb{C}[X,Y]/(X^2+Y^2-1).
\end{aligned}
$$
We may observe that $X^2+Y^2-1$ is irreducible over $\mathbb{C}[X][Y].$ Therefore
$\mathbb{C}[X,Y]/(X^2+Y^2-1)$ is an integral domain and $(X^2+Y^2-1, X+iZ)$ is a prime
ideal. So $V(X^2+Y^2-1, X+iZ)$ is irreducible. A similar argument follows for
$V(X^2+Y^2-1, X-iZ).$

\(b\) Let $V=\{(t,t^2,t^3)\in\mathbb{A}^3(\mathbb{C}) \ | \ t\in\mathbb{C}\}.$ Find $I(V),$ and show
that $V$ is irreducible.

Note that $V=V(Y-X^2, Z-X^3).$
We will show that $(Y-X^2, Z-X^3)$ is a prime ideal by showing that
$\mathbb{C}[X,Y,Z]/(Y-X^2, Z-X^3)$ is an integral domain. Observe
$$
\begin{aligned}[t]
\mathbb{C}[X,Y,Z]/(Y-X^2, Z-X^3) &\cong (\mathbb{C}[X,Y]/(Y-X^2))[Z]/(Z-X^3)\\
&\cong \mathbb{C}[X,Z]/(Z-X^3)\\
&\cong\mathbb{C}[X].
\end{aligned}
$$
The first equality follows from the third isomorphism theorem (numbering from
Dummit and Foote). The second equality
follows from the homomorphism $Y\mapsto X^2.$ The final equality follows
from the homomorphism $Z\mapsto X^3.$

**1.34.** Let $R$ be a UFD.

\(a\) Show a monic polynomial of degree two or three in $R[X]$ is irreducible if
and only if it has no roots in $R.$

<div class="proof"> Let $F\in R[X]$ with $2\le\deg(F)\le 3.$

Suppose $G,H\in R[X]$
are nonconstant such that $F=GH.$ Note that $G$ and $H$ must be monic.
Since $\deg(F)=\deg(G)+\deg(H),$ one of $\deg(G)$ or $\deg(H)$ must be $1.$
Therefore one is of the form $X-a,$ $a\in R.$ So $a$ is a root
of $F.$

Suppose $a\in R$ is a root of $F.$ Then by the division algorithm there
exists $G\in R[X]$ such that $F(X)=(X-a)G(X).$
</div>

\(b\) The polynomial $X^2-a$ is irreducible if and only if $a$ is not a square in
$R.$

<div class="proof"> Note that $X^2-a$ has a root in $R$ if and only if $a$ is a square
in $R.$ Therefore the result follows from part (a).
</div>

**1.35.** Show that $V(Y^2-X(X-1)(X-\lambda)\subset\mathbb{A}^2(k)$ is an irreducible curve for
any algebraically closed field $k$ and any $\lambda\in k.$

<div class="proof"> Note $Y^2-X(X-1)(X-\lambda)$ has no root in $k[X],$ and therefore
it is irreducible over $k[X,Y].$
</div>

**1.36.** Let $I=(Y^2-X^2,Y^2+X^2)\subset \mathbb{C}[X,Y].$ Find $V(I)$ and
$\text{dim}_\mathbb{C}(\mathbb{C}[X,Y]/I).$

Note that $X^2,Y^2\in I.$ Thus since it is easy to see $(X^2,Y^2)\supset I,$
$I=(X^2,Y^2).$ Observe that $\mathbb{C}[X,Y]/(X^2,Y^2)$ is spanned by the linearly
independent set $\{\overline{1},\overline{X},\overline{Y},\overline{XY}\}.$

**1.37.** Let $k$ be any field, $F\in k[X]$ of degree $n>0.$ Show the
residues $\overline{1},\overline{X},\ldots,\overline{X}^{n-1}$ for a basis for $k[X]/(F).$

<div class="proof">
Let $F=\sum_{i=0}^na_ix^i.$ Then $\overline{X}^n =
\sum_{i=0}^{n-1}\overline{-a_n^{-1}a_ix^i}.$ By induction, $\overline{X}^m$ is a linear
combination of the linearly independent set $\{\overline{1},\overline{X},\ldots,
\overline{X}^{n-1}\}.$
</div>

**1.38.\*** Let $R=k[X_1,\ldots,X_n],$ $k$ algebraically closed, $V=V(I).$
Show that there is a natural one-to-one correspondence between algebraic subsets
of $V$ and radical ideals in $R/I,$ and that irreducible
algebraic sets (resp. points) correspond to prime ideals (resp. maximal ideals).

<div class="proof">
Let $U\subset V$ be an algebraic set. Then $I(U)$ is a radical ideal in $R$ such
that $I\subset I(V)\subset I(U).$ Thus by Problem 22 (the Fourth Isomorphism Theorem
in Dummit and Foote), $\overline{I(U)}$ is a radical ideal in $R/I.$

Similarly, if $\overline{J}$ is a radical ideal in $R/I$ then $J$ is a radical ideal
in $R$ with $I\subset J.$ Thus $V(J)\subset V(I).$

We prove the final statements by noting that an ideal $J\supset I$ in $R$ is
prime (resp. maximal) if and only if $\overline{J}$ is prime (resp. maximal) in
$R/I.$
</div>

**1.39.** \(a\) Let $R$ be a UFD, and let $P=(t)$ be a principal proper prime
ideal. Show that there is no prime ideal $Q$ such that $0\subset Q\subset P,$
$Q\ne 0,$ $Q\ne P.$

<div class="proof">
Suppose $Q\subset P$ is a prime ideal. Let $q\in Q,$ $q\ne 0.$ Let
$q=p_0p_1\ldots p_k$ with $p_0,\ldots,p_k$ irreducible. For any $i$ note that
if $p_i\in P$ then $p_i=ut$ for some unit $u.$

Suppose, with renumbering, that $p_0,\ldots,
p_l\not\in P$ and $p_{l+1},\ldots,p_k\in P,$ were $0\le l\le k.$ Note that since
$q\in P,$ $l<k.$ Moreover, since $p_0\ldots p_l\not\in Q,$ it must be that
$p_{l+1}\ldots p_k\in Q.$ But $p_{l+1}\ldots p_k=ut^{k-l}$ for some unit $u.$
So it must be that $t^{k-l}\in Q.$ But $Q$ is prime, so therefore $t\in Q.$
Thus $Q=P.$
</div>

\(b\) Let $V=V(F)$ be irreducible. Show that there is no irreducible algebraic
set $W$ such that $V\subset W\subset\mathbb{A}^n,$ $W\ne V,$ $W\ne\mathbb{A}^n.$

<div class="proof">
Observe if $F$ is reducible then, by Corollary 4, $F=G^n$ and $I(V(F))=(G)$
is prime.
If $V\subset W\subset\mathbb{A}^n,$ then $0\subset I(W)\subset I(V)=(G).$ Therefore
by part \(a\) $I(W)=0$ or $I(W)=I(V).$
</div>


**Bonus (Exercise 1.9 from Gathman's notes).** Prove that every affine
variety $X\subset\mathbb{A}^n$ consisting of finitely many points is the zero locus of
$n$ polynomials.

**Note.** For this exercise I used more standard notation
(lowercase $f,g,\ldots$ for polynomials, etc.) to match the notation used by
Gathmann.

<div class="proof">
Let $X=\{p_1,\ldots,p_r\}$ with each $p_i=(a_{i1},\ldots,a_{in})\in\mathbb{A}^n.$
We will define $f_1,\ldots,f_n\in k[x_1,\ldots,x_n]$ such that
$X=V(f_1,\ldots,f_n).$

Let $S=\{1,\ldots,r\}.$ Define $f_1=\prod_{i\in S}(x_1-a_{i1}).$

Let $i\in S.$ Define the polynomial
$$
    g_i(x_1)=\frac{\prod_{j\in S\setminus\{i\}}(x_1-a_{j1})}
        {\prod_{j\in S\setminus\{i\}}(a_{i1}-a_{j1})}.
$$
Suppose $f_1=0,$ that is, $x_1=a_{k1}$ for some $k.$ Then $g_i(x_1)=0$ for all
$i\ne k$ and $g_k(x_1)=1.$

Let $d\in \{2,\ldots,n\}.$ Define the polynomial
$$
    f_d(x_1,\ldots,x_n)=\prod_{i\in S}(x_d-a_{id})-\sum_{i\in S}\left[
            g_i(x_1)(x_d-a_{id})\left(
                    \prod_{j\in S\setminus\{i\}}(x_d-a_{jd}) + 1
                \right)
        \right].
$$
Suppose $f_1=0,$ that is, $x_1=a_{k1}$ for some $k.$ Then $g_i(x_1)=0$ for all
$i\ne k$ and $g_k(x_1)=1.$ Therefore
$$
    f_d(x_1,\ldots,x_n) = \prod_{i\in S}(x_d-a_{id}) - 
        (x_d-a_{id})\left(
                \prod_{j\in S\setminus\{i\}}(x_d-a_{jd}) + 1
            \right)
        = (x_d-a_{kd}).
$$
Thus $X=V(f_1,\ldots,f_n).$
</div>


**1.40.** Let $I=(X^2-Y^3, Y^2-Z^3)\subset k[X,Y,X].$ Define
$\alpha:k[X,Y,Z]\to k[T]$ by $\alpha(X)=T^9,$ $\alpha(Y)=T^6,$ and
$\alpha(Z)=T^4.$

\(a\) Show that every element of $k[X,Y,Z]/I$ is the residue of an element $A+XB+
YC+XYD,$ for soem $A,B,C,D\in K[Z].$

<div class="proof">
Observe that $\overline{X}^2=\overline{Y}^3$ and $\overline{Y}^2=\overline{Z}^3.$ Thus
for $n,m\ge 2$ we may write $\overline{X}^n$ and $\overline{Y}^m$ as the residue of a
polynomial in $k[Z].$
</div>

\(b\) If $F=A+XB+YC+XYD,$ $A,B,C,D\in k[Z]$ such that $\alpha(F)=0,$ show $F=0.$

<div class="proof">
We may factor out $T$'s to find $\alpha(F)=\alpha(A)+T^9\alpha(B)+T^6\alpha(C)
+T^15\alpha(D).$ Observe $4n=6+4m$ and $9+5n=15+4m$ have no integer solutions.
Thus it must be that $A=B=C=D=0.$
</div>

\(c\) Show that $\ker\alpha=I,$ so $I$ is prime, $V(I)$ is irreducible, and
$I(V(I))=I.$

<div class="proof">
By \(b\) we know $\ker\alpha\subset I.$ It remains to show $I\subset\ker\alpha.$
Observe $\alpha(X^2-Y^3)=T^{18}-T^{18}=0$ and $\alpha(Y^2-Z^3)=T^{12}-T^{12}=0.$
Thus if $F\in I,$ $\alpha(F)=0,$ and $F\in\ker\alpha.$

Since $\ker\alpha=I,$ by the first isomorphism theorem
$k[X,Y,Z]/I\cong\text{Im }\alpha\subset k[T].$ Since $k[T]$ is an integral domain,
so is $k[X,Y,Z]/I$;
therefore $I$ is prime. Thus $V(I)$ is irreducible and $I(V(I))=I.$ 
</div>
