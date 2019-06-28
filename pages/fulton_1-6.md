---
documentclass: article
title: Fulton 1.6
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**1.30.** Let $k=\mathbb{R}.$

\(a\) Show that $I(V(X^2+Y^2+1))=(1).$

<div class="proof">
Observe $a^2+b^2+1\ge 1$ for all $a,b\in\mathbb{R}.$ Thus $I(V(X^2+Y^2+1))=I(\emptyset)
=(1).$
</div>

\(b\) Show every algebraic subset of $\mathbb{A}^2(\mathbb{R})$ is equal to $V(F)$ for some
$F\in\mathbb{R}[X,Y].$

<div class="proof">
By Corollary 2, it suffices to show points are $V(F)$ for some $F\in\mathbb{R}[X,Y].$
Let $(a_1,a_2)\in\mathbb{A}^2(\mathbb{R}).$ Define $F=(X-a_1)^2+(Y-a_2)^2.$ Observe that
$V(F)=\{(a_1,a_2)\}.$
</div>

**1.31.** \(a\) Find the irreducible components of $V(Y^2-XY-X^2Y+X^3)$ in
$\mathbb{A}^2(\mathbb{R})$ and $\mathbb{A}^2(\mathbb{C}).$

Observe that $Y^2-XY-X^2Y+X^3=(Y-X)(Y-X^2).$ So $V(Y^2-XY-X^2Y+X^3)=V(Y-X)\cup V(Y-X^2).$
Since $Y-X$ and $Y-X^2$ are irreducible over both $\mathbb{R}$ and $\mathbb{C},$ these are
irreducible components

\(b\) Do the same for $V(Y^2-X(X^2-1)),$ and for $V(X^3+X-X^2Y-Y).$

In $K[X]$ observe that $X(X^2-1)\in (X)$ and $X(X^2-1)\not\in (X^2).$ Therefore
$Y^2-X(X^2-1)$ is irreducible in $K[X][Y]$ by Eisenstein's criterion. Since
$Y^2-X(X^2-1)$ has infinitely many solutions over both the real and complex numbers,
$V(Y^2-X(X^2-1))$ is itself irreducible in $\mathbb{A}^2(\mathbb{R})$ and $\mathbb{A}^2(\mathbb{C}).$

Observe that $X^3+X-X^2Y-Y = (X^2+1)(X-Y).$ Note that $X^2+1$ has no solutions in
$\mathbb{R}.$ Thus in $\mathbb{A}^2(\mathbb{R})$ we have $V(X^3+X-X^2Y-Y)=V(X-Y),$ which is
irreducible. In $\mathbb{C}$ we have $X^3+X-X^2Y-Y=(X-i)(X+i)(X-Y)$ and
$V(X^3+X-X^2Y-Y)=V(X-i)\cup V(X+i)\cup V(X-Y).$
