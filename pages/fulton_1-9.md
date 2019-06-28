---
documentclass: article
title: Fulton 1.9
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**Proposition 3.** Let $R$ be a subring of a domain $S,$ $v\in S.$ Then
the following are equivalent:
 (1) $v$ is integral over $R$;
 (2) $R[v]$ is module finite over $R$;
 (3) there is a subring $R'$ of $S,$ $R[v]\subset R',$ such that $R'$
is module finite over $R.$

We provide a detailed proof of *(3)* $\Rightarrow$ *(1)* to clarify Fulton's proof.

<div class="proof">
Since $R'$ is module finite over $R,$ $R'=\sum_{i=1}^nRw_i$ for some
$w_1,\ldots,w_n\in R'.$ For each $i$ observe $w_iv\in R',$ so we may write
$w_iv=\sum_{j=1}^na_{i,j}w_j$ for some $a_{i,1},\ldots,a_{i,n}\in R.$ Let
$F$ be the field of fractions for $S$ and define $A\in F^{n\times n}$ as
$$
A = \begin{bmatrix}
        v-a_{1,1} & -a_{1,2} & \ldots & -a_{1,n}\\
        -a_{2,1} & v-a_{2,2} & \ldots & -a_{2,n}\\
        \vdots & \vdots & \ddots & \vdots\\
        -a_{n,1} & -a_{n,2} & \ldots & v-a_{n,n}\\
    \end{bmatrix}
$$
Note that $v$ appears only along the diagonal of $A,$ so we may write
$\det(A)=v^n+r_{n-1}v^{n-1}+\ldots+r_1v+r_0,$ for some $r_0,\ldots,r_{n-1}\in R.$

Let $w=(w_1,\ldots,w_n)\in F^n.$ Then
$$
    Aw = \begin{bmatrix}
        \sum_{j=1}^n(\delta_{i,j}v-a_i)w_j
        \end{bmatrix}_{i\in\{1,\ldots,n\}}
$$
where $\delta_{i,j}=0$ if $i\ne j,$ and $\delta_{i,i}=1.$ Observe
$$
    \sum_{j=1}^n(\delta_{i,j}v-a_i)w_j=w_iv-\sum_{j=1}^na_{i,j}w_j=0.
$$
Since $w\ne 0,$ $A$ is nonsingular. Thus
$$
    v^n+r_{n-1}v^{n-1}+\ldots+r_1v+r_0=\det(A)=0.
$$
So $v$ is integral over $R.$
</div>

**1.46.\*** Let $R$ be a subring of $S,$ $S$ a subring of a domain $T.$
If $S$ is integral over $R,$ and $T$ is integral over $S,$ show that $T$ is
integral over $R.$

<div class="proof">
Let $v\in T.$ Then there exists $f\in S[X]$ such that $F(v)=0.$ Let
$F=X^n+a_{n-1}X^{n-1}+\ldots+a_0,$ $a_0,\ldots,a_{n-1}\in S.$ Note that
$a_0,\ldots,a_{n-1}$ are each integral over $R.$ Thus $R[a_0,\ldots,a_{n-1}]$
is module finite over $R$ by inductively applying problem 1.45\(a\) and proposition 3.
Observe that $v$ is integral over $R[a_0,\ldots,a_{n-1}].$ Therefore by
problem 1.45\(a\) and proposition 3, $R[a_0,\ldots,a_{n-1}][v]$ is module finite
over $R.$ Since $R[v]\subset R[a_0,\ldots,a_{n-1}][v],$ $v$ is integral over
$R$ by proposition 3.
</div>

**1.47.\*** Suppose a domain $S$ is ring finite over $R.$ Show that $S$ is
module finite over $R$ if and only if $S$ is integral over $R.$

<div class="proof">
Suppose $S=R[v_1,\ldots,v_n]$ is integral over $R.$ Then by inductively applying
problem 1.45\(a\) and proposition 3 wwe have $S$ module finite over $R.$

Suppose $S$ is module finite over $R.$ Let $v\in S.$ Then $R[v]\subset S$
and $v$ is integral over $R$ by proposition 3.
</div>

**1.49.\*** Let $L$ be a field, and $k$ an algebraically closed subfield of
$L.$

\(a\) Show that any element of $L$ that is algebraic over $k$ is in $k.$

<div class="proof">
Let $v\in L$ such that $F(v)=0$ for some nonzero $F\in k[X].$ Since $k$ is
algebraically closed, $F(X)=\prod_{i=1}^n(X-a_i)$ for $a_1,\ldots,a_n\in k.$
Thus $v=a_i$ for some $i.$
</div>

\(b\) An algebraically closed field has no module finite field extension other
than itself.

<div class="proof">
Suppose $L$ is a module finite field extension of $k.$ Let $v\in L.$ Since
$k[v]\subset L,$ $v$ is algebraic over $k$ by Proposition 3. Thus $v\in k$ by
part (a). So $L=k.$
</div>

**1.49.\*** Let $K$ be a field, $L=K(X).$

\(a\) Show any element of $L$ that is integral over $K[X]$ is already in $K[X].$

<div class="proof">
Suppose $H/G\in K(X),$ with $H,G$ relatively prime, such that
$$
    \left(\frac{H}{G}\right)^n+F_{n-1}\left(\frac{H}{G}\right)^{n-1}+\ldots+F_0=0, 
$$
where $F_0,\ldots,F_{n-1}\in K[X].$ So
$$
    H^n+F_{n-1}H^{n-1}G+\ldots+F_0G^n=0.
$$
Therefore
$$
    H^n=G(-F_{n-1}H^{n-1}-\ldots-F_0G^{n-1})
$$
and $G\mid H.$ Thus it must be that $G\in K$ and $H/G\in K[X].$
</div>

\(b\) Show that there is no nonzero element $F\in K[X]$ such that for every
$z\in L,$ $F^nz$ is integral over $K[X]$ for some $n>0.$

<div class="proof">
By part \(a\) this would imply that fo each $G\in K[X],$ there exists $n>0$ such
that $F^n/G\in K[X].$ But clearly we may select a nonunit $G\in K[X]$ that is
relatively prime to $F,$ a contradiction.
</div>

**1.50.\*** Let $K$ be a subfield of a field $L.$

\(a\) Show that the set of elements of $L$ that are algebraic over $K$ is a
subfield of $L$ containing $K.$

<div class="proof">
By the Corollary of Proposition 3 we have that the set of elements algebraic
over $K$ is a ring containing $K.$ Thus it suffices to show if $v\in L$ is
algebraic over $K$ then so is $v^{-1}.$

Suppose $v\in L\setminus\{0\}$ such
that $v^n+a_{n-1}+\ldots+a_0=0,$ with $a_0,\ldots,a_{n-1}\in K.$ Observe
$$
    0 = v^n+a_{n-1}+\ldots+a_0
        = v^n\left(1+\frac{1}{v}a_{n-1}+\ldots+a_0\frac{1}{v^n}\right).
$$
Since $v\ne 0,$ $v^n\ne 0,$ it must be that
$$
    0=1+\frac{1}{v}a_{n-1}+\ldots+a_0\frac{1}{v^n}
        =a_0\left(a_0^{-1}+\ldots+(v^{-1})^n\right).
$$
So $a_0^{-1}+\ldots+(v^{-1})^n=0$ and $v^{-1}$ is algebraic over $K.$
</div>

\(b\) Suppose $L$ is module finite over $K$ and $K\subset R\subset L.$ Show
$R$ is a field.

<div class="proof">
Let $v\in R\setminus\{0\}.$ It suffices to show $v^{-1}\in R.$
Since $K[v]\subset R\subset L$ and $L$ is module
finite over $K,$ by Proposition 3, $v$ is integral over $K.$ Thus
$F(v)=v^n+a_{n-1}+\ldots+a_0=0,$ with $a_0,\ldots,a_{n-1}\in K.$ Suppose $F(X)$
is irreducible, and therefore $a_0\ne 0.$ Then
$$
    v(v^{n-1}+\ldots+a_1)=-a_0.
$$
So
$$
    v(v^{n-1}+\ldots+a_1)(-a_0^{-1})=1.
$$
Therefore
$v^{-1}=(v^{n-1}+\ldots+a_1)(-a_0^{-1})\in R.$
</div>
