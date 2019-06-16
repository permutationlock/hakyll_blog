---
documentclass: article
geometry: margin=1in
header-includes:
    - \usepackage{amsmath}
    - \usepackage{amsthm}
...

Vakil: Foundations of Algebraic Geometry
========================================

Here are my exercise solutions and notes.

Chapter 1: Category Theory
--------------------------

**1.2.A.**

 a. The set $\text{Aut}(A)$ in a groupoid with one object is a group,
and vice versa.

 b. Pick any groupoid with two or more objects?

**1.2.B.** Identity and associativity under the operation of composition
follows from the definition of morphisms in a category. Thus the set of
invertile automorphisms forms a group.

The automorphism groups in the category of sets are the symmetric
groups. Assuming vector spaces are finite dimensional, their automorphism groups
are general linear groups. For infinite dimensional vector spaces more
complicated groups would arise.

If $A$ is isomorphic to $B$ then $\text{Aut}(A)$ is isomorphic to
$\text{Aut}(B).$

\begin{proof}
Suppose $\phi$ is an isomorphism $A\to B$. Let $f\in\text{Aut}(A)$.
Observe that $\phi\circ f\circ\phi^{-1}$ is a morphism $B\to B$ with inverse
$\phi\circ f^{-1}\circ\phi^{-1}$. So $\phi\circ f\circ\phi^{-1}\in\text{Aut}
(B,B)$.

Let $\phi':\text{Aut}(A)\to\text{Aut}(B)$ be defined by $f\mapsto\phi\circ
f\circ\phi^{-1}$. Note that this map is surjective by the argument above.
Moreover, for $f,g\in\text{Aut}(A)$, observe that
$\phi'(fg)=\phi'(f)\phi'(g)$. Thus $\phi'$ is a homomorphism.
Finally, suppose that $f\mapsto\text{id}_B$. Then

\begin{align*}
f &= (\phi^{-1}\circ\phi)\circ f\circ (\phi^{-1}\circ\phi)\\
  &= \phi^{-1}\circ\text{id}_B\circ\phi\\
  &= \phi^{-1}\circ\phi\\
  &= \text{id}_A.
\end{align*}

Thus $\phi'$ has trivial kernel and is hence injective.
\end{proof}

The point of the "isomorphic, but not canonically isomorphic" comment is that
although the automorphism groups are pairwise isomorphic, there is not
necessarily a "canonical" way of identifying the groups; that is, there might
be distinct isomorphisms between the same two automorphism groups. This is
classically the case with pointed fundamental groups in the situation that
Vakill describes.

**1.3.A.** Let $A, B$ be two initial objects. Let $\varphi:A\to B$, $\psi:B\to
A$ be the only morphisms. Observe that both $\text{id}_A$ and $\psi\circ\varphi$
are morphisms $A\to A$, thus they must be equal since $A$ is initial. Similarly
for $B$. Thus $\varphi,\psi$ are isomorphisms.

A similar argument follows for $A, B$ final.

**1.3.B.** In *Sets* and *Top* the initial object is $\emptyset$ and the final
object is a singleton $\{a\}$. In *Rings* the initial object is $\mathbb{Z}$
and the final object is the zero ring.

**1.3.C.** Let $\varphi:A\to S^{-1}A$ be the natural homomorphism. Observe that
$\varphi(a)=\varphi(b)$ if and only if $s(a-b)=0$ for some $s\in S$. Thus
$\ker\varphi=\{a\in A \ : \ as=0 \ \text{for some} \ s\in S\}$, and $\varphi$
injective if and only if there exist no zero divisors in $S$.

**1.3.D.** Suppose $f:A\to B$ is an $A$-algebra homomorphism sending each
element of $s$ to a unit in $B$. Observe that if $as=0$ for some $s\in S$, then
$f(a)=f(a)f(s)f(s)^{-1}=f(as)f(s)^{-1}=0$. Thus $\ker f\subseteq\ker\varphi$.
Hence there exists a unique map $\widetilde{f}:S^{-1}A\to B$ such that
$f=\widetilde{f}\circ\varphi$ (see Lemma 1.2 in my [Fulton notes][BroFulton]).

**1.3.E.** Clearly $S^{-1}M$ as defined in the hint is an abelian group under
the defined addition, with identity $0/1\equiv 0/s$ for all $s\in S$. Moreover,
the defined ring action defined satisfies the axioms for $S^{-1}M$ to be an
$S^{-1}A$-module.

**1.3.F.**

**\(a\)** Define $\varphi:S^{-1}(M_1\times\ldots\times M_n)\to S^{-1}M_1\times\ldots
S^{-1}M_n$ by $(m_1,\ldots,m_n)/s\mapsto (m_1/s,\ldots,m_n/s)$. Observe that
\begin{align*}
    \varphi\left(\frac{(m_1,\ldots,m_n)}{s}+\frac{(k_1,\ldots,k_n)}{t}\right)
        &= \varphi\left(\frac{(tm_1+sk_1,\ldots,tm_n+sk_n)}{st}\right)\\
        &=\left(\frac{tm_1+sk_1}{st},\ldots,\frac{tm_n+sk_1}{st}\right)\\
        &=\left(\frac{m_1}{s},\ldots,\frac{m_n}{s}\right)
            + \left(\frac{k_1}{t},\ldots,\frac{k_n}{t}\right)\\
        &=\varphi\left(\frac{(m_1,\ldots,m_n)}{s}\right)+
            \varphi\left(\frac{(k_1,\ldots,k_n)}{t}\right).
\end{align*}
Therefore $\varphi$ is a group homomorphism, and thus clearly an
$S^{-1}R$-module homomorphism.

Suppose that $(m_1,\ldots,m_n)/s\mapsto 0$. Then
$s_1m_1=s_2m_2=\ldots=s_nm_n=0$ for some
$s_1,\ldots,s_n\in S$. Therefore $s_1\ldots s_n m_i=0$ for all $i$ and thus
$s_1\ldots s_n(m_1,\ldots,m_n)=(0,\ldots,0)$. Hence $(m_1,\ldots,m_n)/s=0\in
S^{-1}(M_1\times\ldots\times M_n)$. Thus $\ker\varphi=0$.

Let $(m_1/s_1,\ldots,m_n/s_n)\in S^{-1}M_1\times\ldots\times
S^{-1}M_n$. Let $s=s_1s_2\ldots s_n$ and $t_i=s_1\ldots s_{i-1}\ldots
s_{i+1}\ldots s_n$. Then observe that $(t_1m_1,\ldots,t_nm_n)/s\mapsto
(m_1/s_1,\ldots,m_n/s_n)$. Thus $\varphi$ is surjective.

**\(b\)** The same argument above works equivalently for arbitrary direct sums.

**\(c\)** Let $S=\{q^n \ : \ n\in\mathbb{N}\}$ for some prime $q\in\ZZ$. Then
$\prod_{i=1}^k M_i$.

[BroFulton]: http://permutationlock.com/static/algebraic_curves.pdf
