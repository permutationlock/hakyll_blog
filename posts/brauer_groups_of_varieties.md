---
documentclass: article
title: Brauer Groups of Varieties
tags: math number theory arithmetic algbraic geometry
published: June 19, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}\usepackage[all]{xy}
---

In what follows all objects referred to explicitly as rings will be assumed to be
commutative with $1$, but not all algebras will be commutative.

**Definition.** A central simple algebra (CSA) over a field $k$ is a finitely generated
$k$-algebra that is central (i.e. has center $k$) and simple (i.e. has no
nontrivial proper ideals).

**Definition.** An algebra $A$ over a ring $R$ is Azumaya if $A$ is
finitely generated, projective, faithful, and the natural map
$$
    \phi_A:A\otimes_k A^{\text{op}}\to\text{End}_R(A)
$$
is an isomorphism.

**Proposition.** If $A$ is an Azumaya algebra over $R$, then $Z(A)=R$ and
there is a one-to-one correspondence between ideals of $R$ and ideals of $A$.

**Proposition.** An $k$-algebra $A$ is a CSA if and only if it is Azumaya.

**Proposition.** If $A,B$ are CSAs over $k$, then so is $A\otimes_k B$.

**Proposition.** An Azumaya algebra over a commutative and local ring
$\mathcal{O}$ is a free finite rank $\mathcal{O}$-module.

**Proposition.** If $A$ is a free module of finite rank over a local ring
$\mathcal{O}$ with maximal ideal $\mathfrak{m}$ and residue field
$k=\mathcal{O}/\mathfrak{m}$, then $A$ is an Azumaya algebra over
$\mathcal{O}$ if and only if $A\otimes_{\mathcal{O}} k$ is a CSA over $k$;

**Note.** We will work exclusively over fields and local rings and thus the
above proposition will effectively work as our definition of an
Azumaya algebra.

**Corollary.** If $A,B$ are Azumay over a local ring $\mathcal{O}$,
then so is $A\otimes_{\mathcal{O}} B$.

<div class="proof">
Let $k$ be the residue field of $\mathcal{O}$.
Observe that $A\otimes_{\mathcal{O}} k$ and $B\otimes_{\mathcal{O}} k$ are CSAs
over $k$. Therefore so is
$$
    (A\otimes_{\mathcal{O}} k)\otimes_k(B\otimes_{\mathcal{O}} k)
    \cong (A\otimes_{\mathcal{O}} B)\otimes_{\mathcal{O}} k.
$$
Thus $A\otimes_{\mathcal{O}} B$ is Azumaya over $\mathcal{O}$.
</div>

**Theorem (1, Proposition 2.2.1).**
If $A$ is a CSA over $k$ then there exists a finite extension
$L/k$ such that $A\otimes_k L\cong M_n(L)$. In this case we say $A$ splits over
$L$.

**Theorem (1, Proposition 2.2.5).**
A CSA over $k$ splits over a separable extension of $k$. Thus
for every CSA $A$ over $k$ we may pick a Galois extension $L/k$ such that
$A\cong M_n(L)$.

**Theorem.** If $A$ is an Azumaya algebra over a local ring $\mathcal{O}$
then all automorphisms of $A$ are inner.

**Definition.** Two Azumaya algebras $A,B$ over a ring $R$ are Brauer equivalent if
there exist finitely generated projective faithful $R$-modules $S,T$ such that
$A\otimes_{R}\text{End}_R(S)\cong B\otimes \text{End}_R(T)$. If $R=k$ is a
field, then $A,B$ are equivalent if $A\otimes_{k} M_s(k)\cong B\otimes_{k}
M_t(k)$ for some integers $s,t$.

**Definition.** The Brauer group of a ring $R$, denoted $\text{Br}(R)$, is the
set of equivalence classes of Azumaya algebras over $R$ with group operation
given by $\otimes_R$. Note that the class of $R$ is the identity element
and $A\otimes_{R}A^{\text{op}}\cong\text{End}_R(A)$ by definition, so the class
of $A^{\text{op}}$ is the inverse of the class of $A$.

**Lemma.** Let $A$ be an Azumaya algebra over $R_1$ and $f:R_1\to R_2$ a ring
homomorphism. Then $A\otimes_{R} S$ is Azumaya over $S$. Moreover, if $A,B$ are
equivalent Azumaya algebras over $R$, then $A\otimes_R S$ and $B\otimes_R S$
are equivalent over $S$. This defines a map $f^*:\text{Br}(R)\to\text{Br}(S)$
and in fact this gives a covariant functor from the category of commutative
rings to the category of abelian groups.

**Theorem.** If $R$ is an integral domain with field of fractions $K$, then the
map $\text{Br}(R)\to\text{Br}(K)$ is injective.

**Definition.** Let $X$ be a variety over a field $k$. For each $x\in X$ we
have that $k(X)$ is the field of fractions of the local ring
$\mathcal{O}_{X,x}$. Thus by the previous theorem
we may consider $\text{Br}(\mathcal{O}_{X,x})$ as a
subset of $\text{Br}k(X)$. We define the Brauer group of $X$ to be the subgroup
$$
    \text{Br}(X)=\bigcap_{x\in X}\text{Br}(\mathcal{O}_{X,x}).
$$
An element of $\text{Br}(k(X))$ is said to be regular if it lies in
$\text{Br}(X)$. Observe that for each $x\in X$ the map
$$
    \text{Br}(\mathcal{O}_{X,x})\to
    \text{Br}(\mathcal{O}_{X,x}/\mathfrak{m}_{X,x})=\text{Br}(k)
$$
gives us a map $\varphi_x:\text{Br}(X)\to\text{Br}(k)$.
