---
documentclass: article
title: Fulton 2.11
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.54.** What does $M$ being free on $m_1,\ldots,m_n$ say in terms of the
elements of $M$?

It means that every element of $M$ may be written as a sum $\sum_{i=1}^na_im_i,$
where $a_1,\ldots,a_n\in R.$

**2.55.** Let $F=X^n+a_1X^{n-1}+\ldots+a_n$ be a monic polynomial in $R[X].$
Show that $R[X]/(F)$ is a free $R$-module with basis $\overline{1},\overline{X},
\ldots,\overline{X}^{n-1},$ where $\overline{X}$ is the residue of $X.$

<div class="proof">
Let $X=\{\overline{1},\overline{X},\ldots,\overline{X}^{n-1}\}.$ Observe
that the natural $R$-module homomorphism $M_X\to R[X]/(F)$ is
surjective with trivial kernel, and thus an isomorphism.
</div>

**2.56.** Show that a subset $X$ of a module $M$ generates $M$ if and only
if the homomorphism $M_X\to M$ is onto. Every module is isomorphic to a quotient
of a free module.

<div class="proof">
The first statement is immediate from Problem 2.54. Let $M$ be an arbitrary
$R$-module and $X$ a (possibly infinite) set of generators for $M.$ Then
there exists a natural surjective homomorphism $\pi:M_X\to M$ and
$M_X/\ker\pi$ is isomorphic to $M.$ 
</div>
