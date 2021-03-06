---
documentclass: article
title: Gille-Szamuely
geometry: margin=1in
fontsize: 12pt
published: May 25, 2019
header-includes:
    - \usepackage{amsmath}
    - \usepackage{amsthm}
    - \usepackage{amssymb}
    - \usepackage{mathrsfs}
    - \usepackage[all]{xy}
...

Here are my exercise solutions and notes for Gille and Szamuely's *Central
Simple Algebras and Galois Cohomology*.

**1.1.** Since the map $\sigma$ is linear, for $q=x+yi+zj+wij$ we must have
$\sigma(q)=r_0x+r_1yi+r_2zj+r_3wij.$ To have $\sigma(q)=1$ we must have
$r_0=1.$ Computing the product $\sigma(q)q$ and
taking $x=y=1$, $z=w=0$ we find that $\sigma(q)q\in k$ only if $r_0=-r_1.$
Setting $z=w=1$ and $x=y=0$ we see that we must also have $r_2=r_3.$ Finally,
setting $y=z=1$ and $x=w=0$ we see that $r_1=r_2.$ Thus
$\sigma(q)=x-yi-zj-wij=\overline{q}.$

**1.3.** Let $K=k(\sqrt{a}).$ Recall that $Q=(a,b)$ for some $b\in K$ by
Lemma $1.2.2.$ Therefore let us pick the basis $\{1,\sqrt{a}\}$ for $K$ and
observe that $\{1,\sqrt{a},\sqrt{b},\sqrt{a}\sqrt{b}\}$ is a basis for $Q.$
Thus the quaternion norm agrees with the relative field norm $N_{K/k}$ for
elements of $K\subset Q.$

**1.5.** Recall that $(-1,p)$ splits over $\mathbb{Q}$ if and only if the curve
$C(-1,p)$ defined by $py^2=x^2+z^2$ has a $\mathbb{Q}$-rational point. Observe
that no $\mathbb{Q}$-rational point on $C$ has $y$-coordinate zero since such a
point would have $x^2+y^2=0$ for both $x,y\ne 0$, a contradiction. So all
$\mathbb{Q}$-rational points lie on the affine curve $p=x^2+y^2.$ By a classic
result from the Gaussian integers, a prime $p$ may be written as the sum of two
squares if and only if $p\equiv 1 \, (\text{mod } 4)$ (see the first section of
the first chapter of Neukirch's algebraic number theory book, for example).

**Lemma.** If $q\in (a,b)$ such that $q^2\in k^\times$ then $q+\overline{q}=0$
or $q\in k.$

<div class="proof">
Let $q=x+yi=zj+wij.$ Computing we find that
$$
    q^2=(x^2+y^2a+z^2b-w^2ab)+2xyi+2xzj+2xwij.
$$
Therefore $q^2=\in k^*$ if and only if $x=0$ or $y=z=w=0.$
</div>

**1.6.** Let $\varphi:(c,d)\to (a,b)$ be an isomorphism. Take $i,j\in (a,b)$
such that $i^2=a, j^2=b$, and similarly $i',j'\in (c,d)$ such that
$(i')^2=c,(j')^2=d.$ Then let $I=\varphi(i')$ and $J=\varphi(j').$

Define $B_0=\{q\in (a,b) \, : \, q+\overline{q}=0\}.$ Observe that $B_0$ is in
fact a subpsace of $(a,b)$ of dimension $3$ and is in fact the span of
$\{i,j,ij\}$ (assuming that $\text{char}(k)\ne 2$ we see that $q=\overline{q}$
implies that $q=yi+zj+wij$).

Since $I^2,J^2\in k$, by the lemma above we have $I,J\in B_0.$ Therefore we have
$2$-dimensional subspaces $\langle I,IJ\rangle$ and $\langle i,ij\rangle$ in
$B_0.$ Thus there is a nonzero element $\epsilon$ in their intersection. Since
$\epsilon\in\langle i,ij\rangle$ we see that $\epsilon j=-j\epsilon.$
Similarly, as $\epsilon\in\langle I,IJ\rangle$ we find $\epsilon J=-J\epsilon.$

Therefore $\{1,\epsilon,j,\epsilon j\}$ is an alternate basis for $(a,b)$ and
$\{1,\varphi^{-1}(\epsilon),j',\varphi^{-1}(\epsilon)j'\}$ is an alternate
basis for $(c,d).$ Taking $e=\epsilon^2$ we therefore have $(a,b)\cong (e,b)$
and $(c,d)\cong (e,d).$
