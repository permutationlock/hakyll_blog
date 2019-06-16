---
documentclass: article
title: Linear indpendence of characters
tags: math algebra galois theory characters groups
published: May 18, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}
---

**Theorem.** Let $G$ be an abelian group, let $K$ be a field, and suppose
that $\chi_1,\ldots,\chi_n$ are distinct characters of $G$ with values in
$K$, i.e., $\chi_i:G\to K^*$ is group
homomorphism for each $i=1,\ldots,n$.
Then the characters are linearly independent, i.e.,
if $c_1,\ldots,c_n\in K$ such that
$$
    c_1\chi_1(g)+\ldots+c_n\chi_n(g)=0
$$
for all $g\in G$, then it must be that $c_1=\ldots c_n=0$.

<div class="proof">
If $n=1$ observe that since $\chi_1(g)\in K^*$ in particular
$\chi_1(g)\ne 0$ for all $g\in G$. Thus $c_1\chi_1(g)=0$ implies $c_1=0$.

Suppose that for some $n\ge 2$ all sets of fewer than $n$ characters of $G$
are linearly indpendent. Let
$\chi_1,\ldots,\chi_{n}$ be distinct characters
and $c_1,\ldots,c_{n}\in K$ such that
$$
    c_1\chi_1(g)+\ldots+c_{n}\chi_{n}(g)=0
$$
for all $g\in G$. Let $i,j\in\{1,\ldots,n\}$ be distinct.
Observe that since the characters are distinct there exists
$g_{ij}\in G$ such that $\chi_i(g_{ij})\ne\chi_j(g_{ij})$.
Observe that by our choice of $c_1,\ldots,c_n$ we have
$$
\begin{aligned}[t]
    0 &= c_1\chi(g_{ij}g)+\ldots+c_n\chi_n(g_{ij}g)\\
      &= c_1\chi_1(g_{ij})\chi_1(g)+\ldots+c_n\chi_n(g_{ij})\chi_n(g)\\
      &= \sum_{r=1}^n c_r\chi_r(g_{ij})\chi_r(g)
\end{aligned}
$$
and
$$
\begin{aligned}[t]
    0 &=\chi_j(g_{ij})(c_1\chi(g)+\ldots+c_n\chi_n(g))\\
      &=c_1\chi_j(g_{ij})\chi_1(g)+\ldots+c_n\chi_j(g_{ij})\chi_n(g)\\
      &= \sum_{r=1}^n c_r\chi_j(g_{ij})\chi_r(g)
\end{aligned}
$$
for all $g\in G$. Therefore subtracting the two we see that
$$
    \sum_{r=1}^{n}c_r(\chi_r(g_{ij})-\chi_j(g_{ij}))\chi_r(g)=0
$$
for all $g\in G$.
Since the term $r=j$ vanishes, by the inductive hypothesis we must have all the
coefficients of the linear combination equal to zero. That is, we must have
$c_r(\chi_r(g_{ij})-\chi_j(g_{ij}))=0$ for all $r=1,\ldots,n$. In particular,
$c_i(\chi_i(g_{ij})-\chi_j(g_{ij}))=0$. But by our choice of $g_{ij}$ we have
$\chi_{i}(g_{ij})-\chi_j(g_{ij})\ne 0$. Thus $c_i=0$.
</div>

The above proof is inspired by the proof given in (1, Theorem 2.1).

### References

 (1) [Keith Conrad, *Linear independence of characters*](https://kconrad.math.uconn.edu/blurbs/galoistheory/linearchar.pdf)
