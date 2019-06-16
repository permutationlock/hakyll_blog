---
documentclass: article
title: Basic facts about matrix algebras
geometry: margin=1in
fontsize: 12pt
published: May 25, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}
...

Let $R$ be a ring, and let $m,n$ be positive integers.

**Proposition 1.** Show that the $r$-algebras $M_m(M_n(R))$ and $M_{mn}(R)$ are
isomorphic.

<div class="proof">
The obvious map between is clearly a bijection and can be seen to be an
$R$-algebra homomorphism by observing that multiplication works as expected in
each.
</div>

**Proposition 2.** Show that if $S$ is an $R$-algebra, then there is a natural
isomorphism $M_n(R)\otimes_R S\to M_n(S)$.

<div class="proof">
Let $\varphi:M_n(R)\otimes S\to M_n(S)$ be defined by $m\otimes_R s\mapsto sm$.
This map is clearly an $s$-algebra homomorphism.

Let $E^{a,b}$ be the $n\times n$ matrix with $(E^{a,b})_{i,j}=1$ if $(i,j)=(a,b)$ and
$(E^{a,b})_{i,j}=0$ otherwise. Observe that the map $\varphi$ is onto as the
$E^{a,b}$ matrices generate $M_n(S)$ as an $S$-algebra.

Suppose that $m\otimes_R s\mapsto 0$. Then either $m=0$, $s=0$, or $sm_{i,j}=0$
for all $i,j$. Writing $m=\sum_{i,j}m_{i,j}E^{i,j}$ we see that
$$
    (m\otimes_R s)=\sum_{i,j} (m_{i,j}E^{i,j}\otimes_R s)=\sum_{i,j}
    (E^{i,j}\otimes_R sm_{i,j})=0.
$$
Thus $\varphi$ is injective and hence an isomorphism.
</div>

**Proposition 3.** If $I$ is an ideal of $R$, let $M_n(I)$ denote the subset of
$M_n(R)$ consisting of matrices with entries in $I$. The identification
$I\leftrightarrow M_n(I)$ is a bijection between the set of two-sided ideals of
the ring $R$ and the set of two-sided ideals of $M_n(R)$.

<div class="proof">
First observe that if $I$ is a two sided ideal of $R$ then $M_n(I)$ as
defined above is clearly a two-sided ideal of $M_n(R)$. Now suppose that $J$ is
a two sided ideal of $M_n(R)$ and define
$$
    J^*=\{r\in R : r=m_{i,j} \text{ for some } m\in J\}.
$$
It is easy to see that $J^*$ is a two sided ideal of $R$ since $J$ is a two
sided ideal of $M_n(R)$ as follows.
Suppose that $j\in J^*$ and $r\in R$. By the definition of $J^*$ there exists a
matrix $m(j)\in J$ with $j$ as an entry. We then easily define a matrix with
one nonzero entry equal to $r$ such that $rj$ is an entry of
$m(r)m(j)$. Hence $rj\in J^*$. A similar argument shows $jr\in J^*$.

It now suffices to show that $M_n(I)^*=I$ and $M_n(J^*)=J$ for any two sided
ideals $I\subseteq R$ and $J\subseteq M_n(R)$. It is immediate from definitions
that $M_n(I)^*=I$. To show the second equality, first observe that
$J\subset M_n(J^*)$ by definition. For any $s\in J^*$, by matrix multiplication
we can construct a matrix $sE^{i,j}$ (with $E^{i,j}$ defined as above). Hence
we can generate all matrices in $M_n(J^*)$.
we can construct a matrix 
</div>

**Proposition 4.** Let $\text{Id}$ denote the identity matrix in $M_n(R)$. The
map $R\to M_n(R)$ defined by $r\mapsto r\text{Id}$ identifies $R$ with the
centre of $M_n(R)$.

<div class="proof">
It is easy to see that if a matrix has nozero entries off of the diagonal, then
we may construct a matrix that does not commute with it. If a diagonal matrix
has two differring entries along the diagonal, then this matrix does not
commute with the matrix consisting of a single nonzero column of $1$'s in the
position matching one of the distinct entries. Thus the only matrices with a
chance of commuting are diagonal matrices of the form $r\text{Id}$.
Thus the map identifies the centre $R$ with the centre of $M_n(R)$.
</div>

**Proposition 5.** If $A$ is a central simple algebra over a field
$k$, then $M_n(A)$ is also a central simple algebra over $k$.

<div class="proof">
By (iii) we see that $M_n(A)$ is simple iff $A$ is simple. By (iv) we see that
we may identify $A$ with the subring $A\text{Id}$ of $M_n(A)$ and that $M_n(A)$
has center equal to the center of $A$. Thus if $A$ is a CSA over $k$ then so is
$M_n(A)$.
</div>
