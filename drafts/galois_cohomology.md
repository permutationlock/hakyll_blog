---
documentclass: article
title: Galois Cohomology
tags: math algebra galois theory cohomology
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}
---

Below I collect my understanding of the introductory facts about
group cohomology and Galois cohomology.

### Group Cohomology

Let $G$ be a group. A $G$-module is an abelian group with an action of $G,$
$m\mapsto m^{\sigma}$ for $\sigma\in G,$ satisfying
$m^{\sigma}+n^{\sigma}=(m+n)^{\sigma}$ and
$(m^{\sigma})^{\tau}=m^{\sigma\tau}$ for all $\sigma,\tau\in G.$ A map of
$G$-modules $\varphi:M\to N$ is a group homomorphism compatible with the action
of $G$ on $N$ and $M,$ i.e., $\varphi(m^{\sigma})=\varphi(m)^{\sigma}.$

Let $M$ be a $G$-module. We define a $1$-cochain with values in $M$ to be a map
$\xi:G\to M$ which we will denote by $\sigma\mapsto\xi_{\sigma}.$ A $1$-cocycle
is a $1$-cochain that satisfies
$\xi_{\sigma\tau}=\xi_{\sigma}^{\tau}+\xi_{\tau}$ for all $\sigma,\tau\in G.$ A
$1$-coboundary is a $1$-cochain $\xi$ if there exists $m\in M$ such that
$\xi_{\sigma}=m^{\sigma}-m$ for all $\sigma\in G.$ Observe that if $\xi$ is a
$1$-coboundary then
$$
    \begin{aligned}[t]
        \xi_{\sigma\tau} &= m^{\sigma\tau}-m\\
            &= m^{\sigma\tau}-m^{\tau}+m^{\tau}-m\\
            &= \xi_{\sigma}^{\tau}-\xi_{\tau}.
    \end{aligned}
$$
Thus $1$-coboundaries are $1$-cocycles. We denote the set of $1$-cochains with
values in $M$ by $C(G,M),$ the set of cocycles with $Z(G,M),$ and the set of
coboundaries with $B(G,M).$ It is easy to see that $C(G,M)$ forms an abelian
group via the group operation from $M$ and
$Z(G,M), B(G,M)$ are subgroups. The first cohomology group is defined to be
$H^{1}(G,M):=Z(G,M)/B(G,M).$ Note that this is only one of many equivalent
definitions for this group. We will also define the "zeroeth" cohomology
group $H^0(G,M):=M^{G}$ to be the set of elements of $M$ that are unaffected by
the action of $G.$

Suppose that $P,M,N$ are $G$-modules and we have an exact sequence of
$G$-module homomorphisms
$$
    0 \to P\overset{\varphi}\to M\overset{\psi}{\to} N\to 0.
$$
We claim that there is a corresponding exact sequence of cohomology groups
$$
    \begin{aligned}[t] 
    0 &\to H^0(G,P)\overset{\varphi^0}{\to}H^0(G,M)\overset{\psi^0}{\to} H^0(G,M)
    \overset{\delta^0}{\to}\\
    &\overset{\delta^0}{\to}
    H^1(G,P)\overset{\varphi^1}{\to}H^1(G,M)\overset{\psi^1}{\to} H^1(G,M),
$$
where the maps $\varphi^i,\psi_i$ are induced by the maps $\varphi,\psi.$

We define
the "connecting homomorphism" $\delta^0$ as follows. Let $n\in H^0(G,P)$
and pick $m\in M$ such that $\psi(m)=n.$ Note that
$$
    \psi(m^{\sigma}-m)=\psi(m)^{\sigma}-\psi(m)=0,
$$
so $m\in\text{ker}(\psi)=\varphi(P).$ We define the $1$-cochain with values in
$P$ to be the map $\delta^0(n):G\to P$ given by
$\sigma\mapsto\delta^0(n)_{\sigma}=\psi^{-1}(m^{\sigma}-m).$ Observe that if
$m'\in M$ is another element such that $\psi(m')=n,$ then $m'-m\in\varphi(P)$
so $m'-m=\varphi(p)$ for some $p\in P.$
Thus
$$
    \begin{aligned}[t]
        (m^{\sigma}-m)-((m')^{\sigma}-m')&=
            (m-m')^{\sigma}-(m-m'))\\
            &=\varphi(p)^{\sigma}-\varphi(p).
    \end{aligned}
$$
Hence $m^{\sigma}-m$ and $(m')^{\sigma}-m'$ are in the same class in
$H^1(G,P).$ It is simple to check, using an argument similar to how we showed
that a coboundary is a cocycle, that
$\delta^0(n)_{\sigma\tau}=\delta^0(n)_{\sigma}^{\tau}-\delta^0(n)_{\tau}$ and
therefore $\delta^0(n)$ is in fact a $1$-cocycle. Therefore $\delta^0$ is a
well defined map from $H^0(G,N)\to H^1(G,P).$ To see that it is a group
homomorphism observe that if $n_1,n_2\in H^0(G,N)$ and $m_1,m_2\in M$ are 
such that $\psi(m_1)=n_1, $\psi(m_2)=n_2,$ then
$$
    \begin{aligned}[n]
        \delta^0(n_1+n_2)_{\sigma}=(m_1+m_2)^{\sigma}-(m_1+m_2)\\
            &=(m_1^{\sigma}-m_1)+(m_2^{\sigma}-m_2)\\
            &=\delta^0(n_1)_{\sigma}+\delta^0(n_2)_{\sigma}.
    \end{aligned}
$$
It remains to show that the sequence given earlier is exact. It is immediate
from the definition of $\delta^0$ that
$\text{ker}(\delta^0)=\text{im}(\psi^0).$ Suppose that
$\xi\in Z(G,P)$ is such that $\varphi^1(\xi)\in B(G,M).$ Then for all
$\sigma\in G$ we have
$\varphi(\xi_\sigma)=m^{\sigma}-m$ for a fixed $m\in M.$ Thus the class of
$\xi$ in $H^1(G,P)$ is $\delta^0(\psi(m)).$ So
$\text{ker}(\varphi^1)=\text{im}(\delta^0).$ Now suppose that $\xi\in Z(G,M)$
is such that $\psi'(\xi)\in B(G,N).$ Then $\psi(\xi_\sigma)=n^{\sigma}-n$ for
some fixed $n=\varphi(m)\in N.$ Therefore $\xi_\sigma=m^\sigma-m+p_\sigma$
where $p^\sigma\in\varphi(P).$ Observe that
$$
    \begin{aligned}[t]
    p_{\sigma\tau} &= \xi_{\sigma\tau}-(m^{\sigma\tau}-m)\\
        &= \xi_\sigma^{\tau}+\xi_\tau-(m^{\sigma}-m)^{\tau}-(m^\tau-m)\\
        &=p_\sigma^\tau-p_\sigma.
    \end{aligned}
$$
So $p_{\sigma\tau}$ is the image of a $1$-cocycle under the map $\varphi^1.$
Hence $\text{ker}(\psi^1)=\text{im}(\varphi^1).$

### References

 (1) [Nicolas Garrel, *An introduction to Galois cohomology through central
 simple algebras*, 2014](https://www.math.univ-paris13.fr/~garrel/chennai.pdf)
