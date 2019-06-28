---
documentclass: article
title: Fulton 1.5
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**1.23.** Give an example of a collection of ideals $\mathcal{P}$ ideals in
a Noetherian ring such that no maximal member of $\mathcal{P}$ is a maximal
ideal.

Take $\mathcal{P}=\{(x^n) \ | \ n > 0\}$ and note all ideals in $\mathcal{P}$
are properly contained in the maximal ideal $(x,2).$

**1.24.** Show that every proper ideal in a Noetherian ring is contained in
a maximal ideal.

<div class="proof">
Let $P$ be a prime ideal in a Noetherian ring $R.$ Let
$$
    \mathcal{P}=\{I\subset R \ |
        \ I \text{ is an ideal, } P\subset I\}
$$
The maximal element of $\mathcal{P}$ is a maximal ideal of $R.$
</div>

\begin{lemma}\label{lem:pid_prime_ideals}
If $R$ is a PID then the prime ideals of $R[X]$ are precisely
those of the form $(0),$ $(F(X))$ where $F$ is irreducible, and $(P, F(X))$
where $P$ is a prime ideal and $F(X) + P$ is irreducible over $(R/P)[X].$
\end{lemma}

<div class="proof">
Let $I$ be a non-zero prime ideal of $R[X].$

Suppose $J=I\cap R\ne \{0\}.$ Then
$$
R[X]/I\cong (R[X]/J[X])/(I/J[X])\cong(R/J)[X]/(I/(J[X])).
$$
Since $R/J$ is a field, $(R/J)[X]$ is a Euclidean Domain. Thus $I/(J[X])=(F(X)+J[X])$
for some $F\in R[X],$ of minumum degree, such that $F+J[X]$ is irreducible over
$(R/J)[X].$ Therefore $I=(J, F(X)).$

Suppose $I\cap R=\{0\}.$ Let $F\in I$ be of minimum degree. Suppose $G\in I$
such that $\text{gcd}(F,G)=1.$ By Gauss' Lemma $\text{gcd}(F,G)=1$ in
$K[X]$ where $K$ is the field of fractions of $R.$ Since $K[X]$ is a Euclidean
domain, there exist $A,B\in K[X]$ such that $A(X)F(X)+B(X)G(X)=1\in K[X].$ If we let
$\alpha\in R\setminus\{0\}$ be a common denominator for the coefficients of
$A,B$ then $\alpha A(X)F(X)+\alpha B(X)G(X)=\alpha.$ Therefore $\alpha\in I,$ a
contradiction. Thus $F\mid G$ for all $G\in I.$ So $I=(F).$
</div>

**1.25.** \(a\) Show that $V(Y-X^2)\subset\mathbb{A}^2(\mathbb{C})$ is irreducible, in
fact, $I(V(Y-X^2))=(Y-X^2).$

<div class="proof">
Note $Y-X^2$ is irreducible in $\mathbb{C}(Y)[X].$
Thus by Gauss' lemma $Y-X^2$ is reducible in $\mathbb{C}[Y][X]=\mathbb{C}[X,Y].$
</div>

\(b\) Decompose $V(Y^4-X^2,Y^4-X^2Y^2+XY^2-X^3)\subset\mathbb{A}^2(\mathbb{C})$ into irreducible
components.

<div class="proof">
Let $V=V(Y^4-X^2,Y^4-X^2Y^2+XY^2-X^3).$
Observe $Y^4-X^2=(Y^2-X)(Y^2+X)$ and $Y^4-X^2Y^2+XY^2-X^3=(Y^2-X^2)(Y^2+X)
=(Y-X)(Y+X)(Y^2+X).$ Thus
$$
    V = V(Y^2-X,(Y-X)(Y+X))\cup V(Y^2+X).
$$
Observe if $Y-X=0$ or $Y+X=0$ then $Y^2=X^2.$ So $X^2-X=0$ and $X=0$ or $1.$
$$
\begin{aligned}[t]
    V(Y^2-X,(Y-X)(Y+X))&=V(X,Y)\cup V(X-1,Y^2-1)\\
    &=V(X,Y)\cup V(X-1,Y-1)\cup V(X-1,Y+1).
\end{aligned}
$$
By Lemma \ref{lem:pid_prime_ideals}, these are all irreducible varieties. Thus
$$
   V=V(X,Y)\cup V(X-1,Y-1)\cup V(X-1,Y+1)\cup V(Y^2+X).
$$
</div>

**1.26.** Show that $F=Y^2+X^2(X-1)^2\in\mathbb{R}[X,Y]$ is an irreducible
polynomial, but $V(F)$ is reducible.

<div class="proof">
Observe $Y^2+X^2(X-1)^2$ is irreducible in $k(Y)[X]$ and thus irreducible in
$k[X,Y].$ However, $V(F)=\{(0,0),(1,0)\}=V(X, Y)\cup V(X-1, Y).$
</div>

**1.27.** Let $V,W$ be algebraic sets in $\mathbb{A}^n(k)$ with $V\subset W.$
Show that each irreducible component of $V$ is contained in some irreducible
component of $W.$

<div class="proof">
Let $V=V_1\cup\ldots\cup V_n$ and $W=W_1\cup\ldots\cup W_m$ be the decomposition
of each into irreducible algebraic sets. Let $i\in\{1,\ldots,n\}.$ Suppose $V_i
\not\subset W_j$ for any $j\in\{1,\ldots,m\}.$ Then since $V\subset W$ it must
be that $V_i\subset \left(\bigcup_{k} W_{j_k}\right).$ But then
$(V_i\cap W_{j_1}),\ldots,(V_i\cap W_{j_k}$ is a decomposition of $V_i$ into
proper algebraic subsets, a contradiction.
</div>

**1.28.** If $V=V_1\cup\ldots\cup V_r$ is the decomposition of an algebraic
set into irreducible components, show that $V_i\not\subset\bigcup_j\ne i V_j.$

<div class="proof">
Suppose $V_i\subset V_j$ for some $j\ne i.$ Then $V_i\cap V_j$ and
$V_i\cap\left(\bigcup_{k\ne j}V_k\right)$ is a decomposition of $V_i$ into
proper algebraic subsets, a contradiction.
</div>

**1.29.\*** Show that $\mathbb{A}^n(k)$ is irreducible if $k$ is infinite.

<div class="proof">
Suppose $k$ is an infinite field. Suppose $\mathbb{A}^k=V_1\cup V_2.$ Then
$V_1\subset V(F)$ and $V_2\subset V(G)$ for some nonzero $F,G\in k[X_1,\ldots,X_n].$
So $\mathbb{A}^k= V(F)\cup V(G)=V(FG).$ By Problem 1.4,
$FG=0,$ thus $F=0$ or $G=0,$ a contradiction.
</div>
