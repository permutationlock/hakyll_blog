---
documentclass: article
title: Fulton 1 - Lemmas
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**Lemma 1.**
Suppose $R,S,T$ are groups (or rings, modules) and $\pi:R\to S$, $\varphi:R\to T$
are group (or ring, module) homomorphisms such that $\ker\pi\subset\ker\varphi$.
Then there exists an
induced homomorphism $\widetilde{\varphi}:\pi(R)\to T$ such that
$\widetilde{\varphi}\circ\pi=\varphi$. Moreover, $\ker\widetilde{\varphi}=\pi(\ker\varphi)$.

<div class="proof">
Suppose $a,b\in R$ such that $\pi(a)=\pi(b)$. Then $a-b\in\ker\pi\subset\ker\varphi$.
So $a+\ker\varphi=b+\ker\varphi$ and $\varphi(a)=\varphi(b)$. Thus the map
$\widetilde{\varphi}:\pi(R)\to T$ defined by $\pi(a)\mapsto\varphi(a)$ is well defined
homomorphism.
</div>

**Lemma 2.**
If $R$ is a PID then the prime ideals of $R[X]$ are precisely
those of the form $(0)$, $(F(X))$ where $F$ is irreducible, and $(P, F(X))$
where $P$ is a prime ideal and $F(X) + P$ is irreducible over $(R/P)[X]$.

<div class="proof">
Let $I$ be a non-zero prime ideal of $R[X]$.

Suppose $J=I\cap R\ne \{0\}$. Then
$$
R[X]/I\cong (R[X]/J[X])/(I/J[X])\cong(R/J)[X]/(I/(J[X])).
$$
Since $R/J$ is a field, $(R/J)[X]$ is a Euclidean Domain. Thus $I/(J[X])=(F(X)+J[X])$
for some $F\in R[X]$, of minumum degree, such that $F+J[X]$ is irreducible over
$(R/J)[X]$. Therefore $I=(J, F(X))$.

Suppose $I\cap R=\{0\}$. Let $F\in I$ be of minimum degree. Suppose $G\in I$
such that $\text{gcd}(F,G)=1$. By Gauss' Lemma $\text{gcd}(F,G)=1$ in
$K[X]$ where $K$ is the field of fractions of $R$. Since $K[X]$ is a Euclidean
domain, there exist $A,B\in K[X]$ such that $A(X)F(X)+B(X)G(X)=1\in K[X]$. If we let
$\alpha\in R\setminus\{0\}$ be a common denominator for the coefficients of
$A,B$ then $\alpha A(X)F(X)+\alpha B(X)G(X)=\alpha$. Therefore $\alpha\in I$, a
contradiction. Thus $F\mid G$ for all $G\in I$. So $I=(F)$.
</div>
