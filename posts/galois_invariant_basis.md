---
documentclass: article
title: Galois invariant basis
tags: math algebra galois theory cohomology
published: May 29, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}
---

Suppose that $K/k$ is a Galois extension with Galois group $G$.
Let $V$ be a $K$ vector space with a semi-linear Galois action, i.e.,
a group homomorphism $G\times V\to V$ denoted $(\sigma,v)\mapsto
\sigma(v)$ such that $\sigma(\lambda v)=\sigma(\lambda)\sigma(v)$
for all $\lambda\in K$ and $v\in V$. We define the $k$-vector space
$$
    V^G=\{v\in V : \sigma(v)=v \ \text{for all} \ \sigma\in G\}.
$$

**Theorem.** The natural homomorphism $V^G\otimes_k K\to V$
given by $v\otimes\lambda\mapsto \lambda v$ is an isomorphism.

<div class="proof">
To show injectivity it suffices to show that if
$\{v_1,\ldots,v_n\}$ are $k$-linearly
independent, then they are also $K$-linearly independent.
Suppose $n$ is the least integer such that there exists
a $k$-linearly independent set $\{v_1,\ldots,v_n\}$ and $\lambda_1,\ldots,
\lambda_n\in K$ such that
$$
    \sum_{i=1}^n\lambda_i v_i =0.
$$
By dividing by $\lambda_1$ we may assume that $\lambda_1=1$. Moreover,
since $\{v_1,\ldots,v_n\}$ is $k$-linearly independent, we must have
$\lambda_j\not\in k$ for some $j\ge 2$. Thus there exists $\sigma\in G$ such
that $\sigma(\lambda_j)\ne\lambda_j$. Observe that
$$
\begin{aligned}[t]
    \sum_{i=2}^n(\sigma(\lambda_i)-\lambda_i)v_i
        &= \sigma\left(\sum_{i=1}^n\lambda_iv_i\right)
            -\sum_{i=1}^n\lambda_iv_i\\
        &= 0.
\end{aligned}
$$
Therefore $\{v_2,\ldots,v_n\}$ is a smaller $k$-linearly independent set
that is not $K$-linearly independent, a contradiction to our selection of $n$
as the smallest number of vectors needed for such an occurrence.

It remains to show that the map is surjective. Let us pick a $k$-basis
$\lambda_1,\ldots,\lambda_n$ for $K$ and write
$G=\{\sigma_1,\ldots,\sigma_n\}$ with $\sigma_1=\text{id}_{K}$.
Define the matrix
$$
    A=  \begin{bmatrix}
            \sigma_1(\lambda_1) & \sigma_1(\lambda_2) & \ldots &
                \sigma_1(\lambda_n)\\ 
            \sigma_2(\lambda_1) & \sigma_2(\lambda_2) & \ldots &
                \sigma_2(\lambda_n)\\
            \vdots & \vdots & \vdots & \vdots\\
            \sigma_n(\lambda_1) & \sigma_n(\lambda_2) & \ldots &
                \sigma_n(\lambda_n)
        \end{bmatrix}.
$$
Observe that each column defines a character of $G$ with values in $K$ given by
$\sigma\mapsto\sigma(\lambda_i)$. Thus the columns are linearly independent by
[linear independence of
characters](/posts/linear_indpendence_of_characters.html).
So $A$ is an invertible matrix with inverse
$A^{-1}=\left( a_{ij}\right)_{i,j}$.
For any $v\in V$ let us also define
$$
    a(v)=
    \begin{bmatrix}
        \sigma_1(v)\\
        \sigma_2(v)\\
        \vdots\\
        \sigma_n(v)
    \end{bmatrix}, \ 
    b(v)=
    \begin{bmatrix}
        \sum_{i=1}^n\sigma_i(\lambda_1 v)\\
        \sum_{i=1}^n\sigma_i(\lambda_2 v)\\
        \vdots\\
        \sum_{i=1}^n\sigma_i(\lambda_n v)
    \end{bmatrix}.
$$
Now let us pick a fixed $v\in V$. Observe we have the equation
$Aa(v)=b(v)$ given explicitly by
$$
    \begin{bmatrix}
        \sigma_1(\lambda_1) & \sigma_2(\lambda_1) & \ldots &
            \sigma_n(\lambda_1)\\ 
        \sigma_1(\lambda_2) & \sigma_2(\lambda_2) & \ldots &
            \sigma_n(\lambda_2)\\
        \vdots & \vdots & \vdots & \vdots\\
        \sigma_1(\lambda_n) & \sigma_2(\lambda_n) & \ldots &
            \sigma_n(\lambda_n)
    \end{bmatrix}
    \begin{bmatrix}
        \sigma_1(v)\\
        \sigma_2(v)\\
        \vdots\\
        \sigma_n(v)
    \end{bmatrix}
    =
    \begin{bmatrix}
        \sum_{i=1}^n\sigma_i(\lambda_1 v)\\
        \sum_{i=1}^n\sigma_i(\lambda_2 v)\\
        \vdots\\
        \sum_{i=1}^n\sigma_i(\lambda_n v)
    \end{bmatrix}.
$$
Multiplying both sides on the right by $A^{-1}$ gives us
$a(v)=A^{-1}b(v)$. In particular, we have
$$
    v=\sigma_1(v)=\sum_{j=1}^n a_{1j}b(v)_j.
$$
We finally observe that $b(v)_j\in V^G$ for each $j$ as each is
invariant under the action of $G$. Thus the map is surjective.
</div>

The inspiration for the above proof comes from (1, Proposition 1.2.2).
Different proofs of the
statement can be found in (2, Lemma II.5.8.1) or (3, Lemma 2.3.8).

### References

 (1) [Nicolas Garrel, *An introduction to Galois cohomology through central
 simple algebras*, 2014](https://www.math.univ-paris13.fr/~garrel/chennai.pdf)
 (2) Joseph Silverman, *The arithmetic of elliptic curves*, 2nd ed., Springer
 Graduate Texts in Mathematics, 1992
 (3) Gille Szamuely, *Central simple algebras and galois cohomology*, Cambridge
Studies in Advanced Mathematics, 2006.
