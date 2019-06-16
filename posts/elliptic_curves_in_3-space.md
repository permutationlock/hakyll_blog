---
documentclass: article
title: Elliptic curves in 3-space
tags: math number theory arithmetic algbraic geometry
published: June 10, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}\usepackage[all]{xy}
---

Suppose that we have an elliptic curve $E/k$ with Weierstrass coordinates
$x,y.$ I.e., $x,y\in k(E)$ such that $(x,y,1):E\to\mathbb{P}^2$ defines an
isomorphism of $E$ with the curve given by
$$
    Y^2Z+a_1XYZ+a_3YZ^2=X^3+a_2X^2Z+a_4XZ^2+a_6Z^3,
$$
with each $a_i\in k$ and distinguished point $O=[0,1,0].$
Let us from now on identify $E$ with the isomorphic
Weierstrass plane curve and let $x=X/Z, y=Y/Z$ be explicit Weierstrass
coordinates for $E.$

The map $\phi:E\to\mathbb{P}^3$ given by $(1,x,y,x^2)$ clearly maps
$E$ into the curve given by the intersection of
$$
    V_1: T_2^2+a_1T_1T_2+a_3T_2T_0=T_1T_3+a_2T_0T_3+a_4T_0T_1+a_6T_0^2
$$
and
$$
    V_2: T_0T_3=T_1^2
$$
in $\mathbb{P}^3.$ Observe that this map is clearly bijectve on the the
affine patch $T_0\ne 0$ as we then dehomogenize coordinates
with $T_0=1$ and find $T_3=T_1^2.$ We also find that there is a
single point $O'=[0,0,0,1]$ in $V_1\cap V_2.$
Observe that $X/Y,Z/Y$ generate $\mathfrak{m}_{E,O}$ and
$$
    \frac{Z}{Y}=\left(\frac{X}{Y}\right)^3
    \left(\frac{Y^2}{Y^2+a_1XY+a_3YZ-a_2X^2-a_4XZ-a_6Z^2}\right).
$$
Therefore $X/Y$ is a local parameter at $O$ and $Z/Y$ has a order $3$ at $O.$
Observe that $x=X/Z=(X/Y)(Y/Z)$ and thus $x$ has a pole of order $2$ at $O$;
thus $x^2$ has a pole of order $4.$
Similarly we see that $y=Y/Z$ has a pole of order $3$ at $O.$ Therefore
$O\mapsto [0,0,0,1]=O'.$

Finally, observe that the map
$\psi:V_1\cap V_2\to E$ given by $(T_1/T_0,T_2/T_0,1)$ is a rational inverse that
is regular on the affine patch $T_0\ne 0.$ It remains to check the one point
$O'$ in $V_1\cap V_2$ satisfying $T_0=0.$ At $O'$ we find, via
a similar argument to the one given above for the curve $E$, that $T_1/T_4$ is a
local parameter for $V_1\cap V_2$ at $O'$, $T_2/T_4$ has order $2$ at $O'$,
and $T_0/T_4$ has order $4.$
Therefore $T_1/T_0$ has a pole of order $3$ at $O'$ and $T_2/T_0$ has a pole of
order $2$ at $O'.$ Thus $\psi$ is regular at $O'$ and
$O'\mapsto O.$ Hence $\psi$ is in fact a
regular inverse to $\phi$ and
$E$ is isomorphic to $V_1\cap V_2.$ In particular, $\phi$ defines an isogeny
between the elliptic curve $E$ with base point $O$ and the elliptic curve
$V_1\cap V_2$ with base point $O'.$

Recall from
[Bezout's Theorem in projective space](/notes/bezout_in_projective_space.html)
that if $F_1,\ldots,F_n\in k[X_1,\ldots,X_{n+1}]_h$ define hypersurfaces
$Q_1,\ldots,Q_n$ in $\mathbb{P}^n$ in general position, i.e.
$\text{dim}\left(\bigcap_{i=1}^n Q_i\right)=0,$ then
$$
    \#\left(\bigcap_{i=1}^n Q_i\right)
    =\prod_{i=1}^n\text{deg}(F_i),
$$
counting points with multiplicity.
Therefore if $H\subset\mathbb{P}^n$ is a hyperplane, the above result tells us
that $\#(H\cap V_1\cap V_2)=4.$ We made the observation above that the
hyperplane defined by $T_0=0$ intersects $V_1\cap V_2$ at the point $O'$ with
multiplicity $4.$

Suppose that $P,Q,R,S\in E$ such that $P+Q+R+S=O.$ Then
$\phi(P)+\phi(Q)+\phi(R)+\phi(S)=O'.$ In $\text{Div}^0(V_1\cap V_2)$
this implies
$$
    (\phi(P))+(\phi(Q))+(\phi(R))+(\phi(S))-4(O')=\text{div}(f)
$$
for some $f\in \overline{k}(V_1\cap V_2).$ Observe that
$f$ is regular on the affine patch
$T_0\ne 0$ since it has only a single pole at $O'.$ Thus there exists a
polynomial $F\in \overline{k}[T_1,T_2,T_3]$ which defines a
hypersurface intersecting
$V_1\cap V_2$ at $\phi(P),\phi(Q),\phi(R),\phi(S).$ Homogenizing we get a form
$F^h\in \overline{k}[T_0,T_1,T_2,T_3]_h$ such that
$F^h/T_0^{\text{deg}(F^h)}\in\overline{k}(\mathbb{P}^3)$ and
$F^h/T_0^{\text{deg}(F^h)}$
restricts to $f\in\overline{k}(V_1\cap V_2).$
$$
\xymatrix{
    \overline{k}[\mathbb{A}^3] \ar[r] & \overline{k}(\mathbb{P}^3) \ar[d]\\
    \overline{k}[V_1\cap V_2\cap\mathbb{A}_3] \ar[u] \ar[r]
        & \overline{k}(V_1\cap V_2)
}
$$
Since $f$ has order $-4$ at $O'$ and $T_0$ intersects $V_1\cap V_2$ with
multiplicity $4$, this implies $\text{deg}(F^h)=1.$ Thus $F^h$ defines a
hyperplane in $\mathbb{P}^3$ that intersects $V_1\cap V_2$ at
$\phi(P),\phi(Q),\phi(R),\phi(S).$

Conversely, if $H\subset\mathbb{P}^3$ is a hyperplane defined by a form $F$
that intersects $V_1\cap
V_2$ at points $\phi(P),\phi(Q),\phi(R),\phi(S)$, then clearly $f=F/T_0\in
\overline{k}(V_1\cap V_2)$ such that the divisor of $f$ is
$(\phi(P))+(\phi(Q))+(\phi(R))+(\phi(S))-(O').$ Thus $P+Q+R+S=O.$

Thus four
points add to $O$ on $E$ if and only if there is a hyperplane in $\mathbb{P}^3$
intersecting $\phi(E)=V_1\cap V_2$ at their images under $\phi.$ The idea for
this post comes from Silverman (1, Exercise 3.10).

### References

 (1) Joseph Silverman, *The arithmetic of elliptic curves*, 2nd ed., Springer
 Graduate Texts in Mathematics, 1992
