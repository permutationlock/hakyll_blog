---
documentclass: article
title: Fulton 2.3
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.14.\*** A set $V\subset\mathbb{A}^n(k)$ called a linear subvariety of
$\mathbb{A}^n(k)$ if $V=V(F_1,\ldots,F_r)$ for some polynomials $F_i$ of degree $1.$

\(a\) Show that if $T$ is an affine change of coordinates on $\mathbb{A}^n$ the $V^T$
is also a linear subvariety of $\mathbb{A}^n.$

<div class="proof">
Let $T=(T_1,\ldots,T_n)$ be an affine change of coordinates, and
$V=V(F_1,\ldots,F_r)$ a linear subvariety of $\mathbb{A}^n.$ Then
$I(V)=(F_1,\ldots,F_r),$ since $F_1,\ldots,F_r$ are of degree $1.$ Note that
$F_1^T,\ldots,F_r^T$ are also degree $1.$ So $I(V)^T=(F_1^T,\ldots,F_r^T).$
therefore $V^T=V(I(V)^T)=V(F_1^T,\ldots,F_r^T).$
</div>

\(b\) If $V\ne\emptyset,$ show that there is an affine change of coordinates $T$
of $\mathbb{A}^n$ such that $V^T=V(X_{m+1}\ldots,X_n).$ So $V$ is a variety.

<div class="proof"> Suppose $r=1,$ that is, $V=V(F).$ Let
$F=\sum_{i=1}^na_iX_i+a_0.$ Let $T'$ be an affine change of coordinates
relabeling varables such that $F^U=\sum_{i=d+1}^na_iX_i+a_0$ with
$a_{d+1},\ldots,a_n\in k\setminus\{0\}.$
By [Lemma 2.8](/pages/fulton_2-lemmas.html) there exists an affine
change of coordinates $U$ on
$\mathbb{A}^{n-d},$ defined by polynomials $U_d,\ldots,U_n\in k[X_d,\ldots,X_n]$
such that $(F^{T'})^{U}=X_n.$ Let $T''$ be the affine change of
coordinates defined by $T_i=X_i$ if $i<d$ and $T_i=U_i$ if $i\ge d.$ Let $I_d$
be the $d\times d$ identity matrix and observe that $T''$ is represented by
the following upper triangular matrix
$$
    T'' = \begin{bmatrix}
            I_d & 0\\
            0 & U
        \end{bmatrix}
$$
Thus $T''$ is invertible and
$T=T''\circ T'$ is an affine change of coordinates such that $F^{T}=X_n.$
Thus $V^{T}=V(X_n).$

Let $r\ge 1.$ Suppose for any $F_1,\ldots,F_r\in k[X_1,\ldots,X_n]$
there exists affine change of coordinates $T$ such that
$V(F_1,\ldots,F_r)^T=V(X_{m+1},\ldots,X_n)$ for some $m\ge 0.$
Let $F_1,\ldots,F_{r+1}\in k[X_1,\ldots,X_n]$ and $V=V(F_1,\ldots,F_{r+1}).$ By the inductive
hypothesis there exists affine change of coordinates $T'$ such that
$V(F_1,\ldots,F_{r})^T=V(X_{m+1},\ldots,X_n).$
Therefore $V^{T'}=V(F_{r+1}^{T'},X_{m+1},\ldots,X_n).$
If $F_{r+1}^{T'}\in (X_{m+1},\ldots,X_n)$ we are done. Otherwise, let us select
$G\in k[X_1,\ldots,X_{m}]$
such that
$$
    F_{r+1}^{T'}+(X_{m+1},\ldots,X_n)=G+(X_{m+1},\ldots,X_n).
$$
Then
$V^{T'}=V(G,X_{m+1},\ldots,X_n).$ By the same method as the
base case, we may construct an affine change of coordinates $T''$ on $\mathbb{A}^n,$
such
that $G^{T''}=X_{m}$ and $X_i^{T''}=X_i$ for all $i>m.$ Thus if we define
$T=T''\circ T',$ then $V^T=V(X_{m},\ldots,X_n).$
</div>

\(c\) Show that the $m$ appearing in part \(b\) is independent of the choice of
$T.$ Thus $V$ is isomorphic to $\mathbb{A}^m(k).$

<div class="proof">
Suppose there exist change of coordinates $T,U$ such that
$V^T=V(X_{m+1},\ldots,X_n)$ and $V^U=V(X_{d+1},\ldots,X_n).$ Observe that
$T|_{V^T}:V^T\to V$ and $U|_{V^U}:V^U\to V$ are each isomorphisms. So
$$
    k[X_1,\ldots,X_m]\cong\Gamma(V^T)\cong\Gamma(V)\cong\Gamma(V^U)\cong k[X_1,\ldots,X_d].
$$
Therefore $m=d.$ Moreover, $\Gamma(\mathbb{A}^m)\cong k[X_1,\ldots,X_m]\cong\Gamma(V).$
</div>

**2.15.\*** Let $P=(a_1,\ldots,a_n),$ $Q=(b_1,\ldots,b_n)$ be distinct
points in $\mathbb{A}^n.$ The line through $P$ and $Q$ is defined to be $L=\{
(a_1+t(b_1-a_1),\ldots,a_n+t(b_n-a_n)) \ | \ t\in k\}.$

\(a\) Show that if $L$ is the line through $P,Q$ and $T$ is an affine change of
coordinates, then $T(L)$ is the line through $T(P),T(Q).$

<div class="proof">
Let $T_i=\sum_{j=1}^nc_{i,j}X_i+c_{i,0}.$ Note that $T_i(P)=\sum_{j=1}^n
c_{i,j}a_j+c_{i,0}$ and $T_i(Q)=\sum_{j=1}^nc_{i,j}b_j+c_{i,0}.$ Let $M$ be the
line between $T(P)$ and $T(Q).$ Let $t\in k.$
Let
$$
    O=\left(\sum_{j=1}^nc_{1,j}(a_j+t(b_j-a_j)+c_{1,0},\ldots,
        \sum_{j=1}^nc_{n,j}(a_j+t(b_j-a_j)+c_{n,0}\right)\in M
$$
Let $O'=(a_1+t(b_1-a_1),\ldots,a_n+t(b_n-a_n))\in L.$ Observe
$T(O')=O$ and thus $T$ is a bijection between $L$ and $T(L)=M.$
</div>

\(b\) Show that a line is a linear subvariety of dimension 1, and that a linear
subvariety of dimesnion 1 is the line through any two of its points.

<div class="proof">
Let $L$ be the line between $P$ and $Q$ defined above.
Observe that $(X_1,\ldots,X_n)\in L$ if and only if there exists $t\in k$
such that $X_i=a_i+t(b_i-a_i)$ every $i.$ Since $P,Q$ are distinct points, there must exist
$i\in\{1,\ldots,n\}$ such that
$a_i\ne b_i.$ Therefore we may solve for $t$ and find $t=\frac{X_i}{b_i-a_i}-
\frac{a_i}{b_i-a_i}.$ So $(X_1,\ldots,X_n)\in L$ if and only if
$F_j=(b_i-a_i)(X_j-a_j)-(b_j-a_j)(X_i-a_i)=0$ for all $j\ne i.$ Therefore
$L=V(\{F_j \ | \ j\in\{1,\ldots,n\}\setminus\{i\}\}).$ Thus $L$ is a
linear subvariety of $\mathbb{A}^n.$ Observe that
$\{F_j \ | \ j\in\{1,\ldots,n\}\setminus\{i\}\}$ is a minimal generating set
([Definition 2.10](/pages/fulton_2-lemmas.html))
for $I(L).$ Therefore, by [Lemma 2.11](/pages/fulton_2-lemmas.html),
$L$ has dimension $1.$

Suppose $V$ is a linear subvariety of dimension $1.$ Then there exists an affine
change of coordinates $T$ such that $V^T=V(X_2,\ldots,X_n).$ Note that $V^T$ is
the line between $P=(0,0,\ldots,0)$ and $Q=(1,0,\ldots,0).$ Thus, by
part (a), $T(V^T)=V$ is the line between $T(P)$ and $T(Q).$
</div>

\(c\) Show that in $\mathbb{A}^2,$ a line is the same thing as a hyperplane.

<div class="proof">
By [Lemma 2.11](/pages/fulton_2-lemmas.html), in $\mathbb{A}^2$ a variety
has dimension 1 if and only if it is a
hyperplane. Thus, by part (b), a line is the same thing as a hyperplane.
</div>

\(d\) Let $P,P'\in\mathbb{A}^2,$ $L_1,L_2$ two distinct lines through $P,$ $L_1',L_2'$
distinct lines through $P'.$ Show that there is an affine change of coordinates
$T$ of $\mathbb{A}^2$ such that $T(P)=P'$ and $T(L_i)=L_i',$ $i=1,2.$

<div class="proof">
Let $U_P,U_{P'}$ be the translations sending $P,P'$ to the origin. Let
$v_1,v_2$ be unit vectors along $U_P(L_1),U_P(L_2).$ Similarly let $u_1,u_2$
be unit vectors along $U_{P'}(L_1'),U_{P'}(L_2').$ Note that $\{v_1,v_2\}$
and $\{u_1,u_2\}$ are each a basis for $\mathbb{A}^2.$ Let $T'$ be the linear map
defined by $v_i\mapsto u_i.$ Then $T=U_{P'}^{-1}\circ T'\circ U_P$ is an affine
change of coordinates such that $T(P)=P'$ and $T(L_i)=L_i',$ $i=1,2.$
</div>

**2.16.** Give $\mathbb{A}^n(\mathbb{C})=\mathbb{C}^n$ the usual topology.

\(a\) Show that $\mathbb{C}^n\setminus S$ is path-connected for any finite set $S.$

<div class="proof">
Let $P,Q\in\mathbb{C}^n\setminus S$ be distinct points. Let $R$ be the set of all
points on lines between $P$ or $Q$ and a point in $S,$ lines in
$\mathbb{R}^{2n},$ not dimension 1 linear subvarieties of $\mathbb{A}^n(\mathbb{C}).$ Since
$S$ is finite, $R\ne\mathbb{C}^n.$ Therefore we may pick a point $O\in\mathbb{C}^n\setminus R.$
Observe that neither the line segment from $P$ to $O$ nor the line segment from
$O$ to $Q$ contain a point in $S.$ Thus there is a path from $P$ to $Q$ in
$\mathbb{C}^n\setminus S.$
</div>

\(b\) Let $V$ be an algebraic set in $\mathbb{A}^n(\mathbb{C}).$ Show that
$\mathbb{A}^n(\mathbb{C})\setminus V$ is path connected.

<div class="proof">
If $V=\mathbb{A}^n(\mathbb{C})$ then $\mathbb{A}^n(\mathbb{C})\setminus V=\emptyset$ and is therefore
path connected. Suppose $V\subsetneq\mathbb{A}^n(\mathbb{C}).$ Let
$P,Q\in\mathbb{A}^n(\mathbb{C})\setminus V,$ $P\ne Q,$ and let $L$ be the line between $P$
and $Q,$ in the sense of linear subvariety of dimension 1. By
[Lemma 2.12](/pages/fulton_2-lemmas.html),
$V\cap L$ is finite. Moreover, $L$ is isomorphic to $\mathbb{A}^1(\mathbb{C}).$ Note
that polynomial maps are continuous with respect to the topology on $\mathbb{C}^n,$ so
$L$ is in fact homeomorphic to $\mathbb{A}^1(C).$ Therefore $L\setminus V$ is
homeomorphic to $\mathbb{C}$ with a finite set removed. Thus $L\setminus V$ is
path connected by part (a). Therefore $\mathbb{A}^n(\mathbb{C})\setminus V$ is path
connected.
</div>
