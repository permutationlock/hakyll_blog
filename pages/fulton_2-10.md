---
documentclass: article
title: Fulton 2.10
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.48.\*** Verify that for any $R$-module homomorphism $\varphi:M\to M',$
$\ker(\varphi)$ and $\text{im}(\varphi)$ are submodules of $M$ and $M'$ respectively.
Show that
$$
	0\longrightarrow \ker(\varphi)\longrightarrow M\overset{\varphi}{\longrightarrow}
		\text{im}(\varphi)\longrightarrow 0
$$
is exact.

<div class="proof">
The map $\phi:M\to im(\phi)$ is surjective by definition and similarly
$\ker\phi\hookrightarrow M$ is the inclusion map and therefore injective.
</div>

**2.49.\*** \(a\) Let $N$ be a submodule of $M,$ $\pi:M\to M/N$ the natural
homomorphism. Suppose $\varphi:M\to M'$ is a homomorphism of $R$-modules, and
$\varphi(N)=0.$ Show that there is a unique homomorphism $\overline{\varphi}:
M/N\to M'$ such that $\overline{\varphi}\circ\pi=\varphi.$

<div class="proof">
See [Lemma 1](/pages/fulton_1-lemmas.html).
</div>

\(b\) If $N$ and $P$ are submodules of a module $M,$ with $P\subset N,$ then there
are natural homomorphisms from $M/P$ onto $M/N$ and from $N/P$ into $M/P.$
Show that the resulting sequence
$$
	0\longrightarrow N/P\longrightarrow M/P\longrightarrow M/N
		\longrightarrow 0
$$
is exact ("Second Noether Isomorphism Theorem").

<div class="proof">
The map $N/P\hookrightarrow M/P$ is the natural inclusion map. The map
$\varphi:M/P\to M/N$ exists by part (a). Again by part (a)
$\ker\varphi=\ker(\varphi\circ\pi)$ where $\pi:M\to M/P$ is the natural
homomorphism. Thus the sequence is exact.
</div>

\(c\) Let $U\subset W\subset V$ be vector spaces, with $V/U$ finite dimensional.
Then $\dim V/U=\dim V/W+\dim W/U.$

<div class="proof">
Observe that
$$
    0\longrightarrow W/U\longrightarrow V/U\longrightarrow V/W
		\longrightarrow 0
$$
is exact by part (b). The result then follows by Proposition 7.
</div>

\(d\) If $J\subset I$ are ideals in a ring $R,$ there is a natural exact sequence
of $R$-modules:
$$
	0\longrightarrow I/J\longrightarrow R/J\longrightarrow R/I\longrightarrow 0.
$$

<div class="proof">
Note that ideals of a ring $R$ are clearly $R$-submodules of $R.$ Therefore the
result follows from part (b).
</div>

\(e\) If $\mathcal{O}$ is a local ring with maximal ideal $\mathfrak{m},$ there is a natural
exact sequence of $\mathcal{O}$-modules
$$
	0\longrightarrow\mathfrak{m}^n/\mathfrak{m}^{n+1}\longrightarrow\mathcal{O}/\mathfrak{m}^{n+1}
		\longrightarrow\mathcal{O}/\mathfrak{m}^n\longrightarrow 0.
$$

<div class="proof">
Observe that $\mathfrak{m}^{n+1}\subset\mathfrak{m}^n\subset\mathcal{O}.$ Thus the result follows by
part (d).
</div>

**2.50.\*** Let $R$ be a DVR satisfying the conditions of Problem 2.30. Then
$\mathfrak{m}^n/\mathfrak{m}^{n+1}$ is an $R$-module, and so also a $k$-module, since
$k\subset R.$

\(a\) Show that $\dim_k(\mathfrak{m}^n/\mathfrak{m}^{n+1})=1$ for all $n\ge 0.$

<div class="proof">
By \(b\) $R/\mathfrak{m}^n$ has dimension $n$ over $k.$ Moreover, by 2.49(e),
the following sequence is exact
$$
    0\longrightarrow\mathfrak{m}^n/\mathfrak{m}^{n+1}\longrightarrow R/\mathfrak{m}^{n+1}
		\longrightarrow R/\mathfrak{m}^n\longrightarrow 0.
$$
Thus, by Proposition 7,
$$
    \dim_k(\mathfrak{m}^n/\mathfrak{m}^{n+1})=\dim_k(R/\mathfrak{m}^{n+1})-\dim_k(R/\mathfrak{m}^n)
        =n+1-n=1.
$$
</div>

\(b\) Show that $\dim_k(R/\mathfrak{m}^n)=n$ for all $n>0.$

<div class="proof">
By Problem 2.30 $R=\{\lambda_0+\ldots+\lambda_{n-1}t^{n-1}+z^nt^n\},$ where
$t$ is the uniformizing parameter.
Therefore $R/\mathfrak{m}^n$ has spanning linearly indpendent set $\{t^d \, | \, 0\le
d\le n-1\}.$
</div>

\(c\) Let $z\in R.$ Show that $\text{ord}(z)=n$ if $(z)=\mathfrak{m}^n,$ and hence that
$\text{ord}(z)=\dim_k(R/(z)).$

<div class="proof">
If $(z)=\mathfrak{m}^n$ then $z=ut^n$ and $\text{ord}(z)=n.$
</div>

**2.51.** Let $0\longrightarrow V_1\longrightarrow\ldots\longrightarrow V_n
\longrightarrow 0$ be an exact sequence of finite-dimensional vector spaces.
Show that $\sum(-1)^i\dim(V_i)=0.$

<div class="proof">
For $n=2$ the statment is trivial. Proposition 7 proves the statement for
such sequences with $n=3,4.$ Suppose
$n>4$ and the statement holds for all such exact sequences of length less
than $n$ (and at least 2). For $1\le k\le n-1,$ let
$\varphi_k:V_k\to V_{k+1}$ be the
homomorphism from the exact sequence. Define
$W=\ker\phi_{n-1}=\text{im}\phi_{n-2}.$ Then
$$
    0\longrightarrow V_1\longrightarrow\ldots\longrightarrow
    V_{n-2}\longrightarrow W\longrightarrow 0 \ \text{and} \  0\longrightarrow
    V_{n-1}\longrightarrow V_n\longrightarrow W\longrightarrow 0
$$
are exact. The second sequence gives us that
$\dim W=\dim V_{n-1}-\dim V_n.$ Therefore, by the first sequence,
$$
    0=\sum_{i=1}^{n-2}(-1)^i\dim V_i + (-1)^{n-1}(W)
    =\sum_{i=1}^n(-1)^i\dim V_i.
$$
</div>

**2.52.\*** Let $N,P$ be submodules of a module $M.$ Show that the subgroup
$N+P=\{n+p \, | \, n\in N, p\in P\}$ is a submodule of $M.$ Show that there is a
natural $R$-module isomomrphism of $N/(N\cap P)$ onto $(N+P)/P$ ("First Noether
Isomorphism Theorem").

<div class="proof">
Observe that for $n,m\in N,$ $n-m\in P$ if and only if $n-m\in N\cap P.$
Thus the map $\varphi:N/(N\cap P)\to (N+P)/P$ defined by $n+(N\cap P)\mapsto
n+P$ is a well defined injective $R$-module homomorphism. Moreover, if
$x\in N+P$ then
$x=n+p$ for some $n\in N,$ $p\in P.$ So $x+P=n+P=\varphi(n+(N\cap P)).$ So
$\varphi$ is surjective.
</div>

**2.53.\*** Let $V$ be a vector space, $W$ a subspace, $T:V\to V$ a one-to-one
linear map such that $T(W)\subset W,$ and assume $V/W$ and $W/T(W)$ are finite
dimensional.

\(a\) Show that $T$ induces an isomorphism of $V/W$ with $T(V)/T(W).$

<div class="proof">
Let $\pi:V\to V/W$ and $\pi_T:T(V)\to T(W)$ be the natural homomorphisms.
Observe that $\ker \pi_T\circ T=W=\ker\pi.$ Thus by
[Lemma 1](/pages/fulton_1-lemmas.html), $\pi_T\circ T$ induces an isomorphism $V/W\to
T(V)/T(W).$
</div>

\(b\) Construct an isomorphism between $T(V)/(W\cap T(V))$ and $(W+T(V))/W,$ and
an isomorphism between $W/(W\cap T(V))$ and $(W+T(V))/T(V).$

<div class="proof">
Note that $T(V),W$ are both submodules of $V.$ Therefore both results follow
from Problem 2.52.
</div>

\(c\) Use Problem 2.49\(c\) to show that $\dim V/(W+T(V))=\dim (W\cap T(V))/T(W).$

<div class="proof">
Observe that $V/W$ is isomorphic to $T(V)/T(W)$ since $T$ is an
isomorphism. Therefore $\dim V/W=\dim T(V)/T(W)=n.$ Note that $W\subset
W+T(V)\subset V$ and $V/W$ is finite dimensional. Therefore by Problem
2.49(c),
$$
    n=\dim V/W = \dim V/(W+T(V))+\dim (W+T(V))/W.
$$
Similarly, $T(W)\subset W\cap T(V)\subset T(V)$ and thus
$$
    n=\dim T(V)/T(W)=\dim T(V)/(W\cap T(V))+\dim (W\cap T(V))/T(W).
$$
By part (b), $\dim T(V)/(W\cap T(V))=\dim (W+T(V))/W,$ yielding the
desired equality $\dim V/(W+T(V))=\dim (W\cap T(V))/T(W).$
</div>

\(d\) Conclude finally that $\dim V/T(V)=\dim W/T(W).$

<div class="proof">
Observe that $(V/T(W))/(W/T(W))\cong T/W$ by the second isomorphism theorem
for modules. Since $W/T(W)$ and $T/W$ are finitely dimensional,
[Lemma 2.1](/pages/fulton_2-lemmas.html) implies that $V/T(W)$
is finitely dimensional. Since $T(V)\subset W+T(V)\subset V,$
Problem 2.49\(c\) shows
$$
    \dim V/T(V)=\dim V/(W+T(V))+\dim (W+T(V))/T(V).
$$
Moreover, since $W/T(W)$ is finite dimensional and $T(W)\subset W\cap
T(V)\subset W,$
$$
    \dim W/T(W)=\dim W/(W\cap T(V))+\dim (W\cap T(V))/T(W).
$$
By part (b),
$$
    \dim W/(W\cap T(V))=\dim (W+T(V))/T(V).
$$
By part (c),
$$
    \dim (W\cap T(V))/T(W)=\dim V/(W+T(V)).
$$
Therefore
$$
    \dim V/T(V)=\dim W/T(W).
$$
</div>
