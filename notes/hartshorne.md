---
documentclass: article
title: Hartshorne
geometry: margin=1in
fontsize: 12pt
published: June 16, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...


Here are my exercise solutions and notes for Hartshorne's *Algebraic Geometry*.

## Chapter 2

### Section 1: Sheaves

**1.1.** Let $\mathscr{F}$ be the sheaf described above.
Observe that $\mathscr{F}_x=A$ for each $x\in X.$ Therefore the sheafification
of $\mathscr{F}$ is the sheaf such that $\mathscr{F}(U)$ is the set of
continuous functions from $U\to \mathscr{F}_x=A$, i.e., the constant sheaf
$\mathscr{A}.$

**1.2.a.** Let $s_P\in\text{ker}(\varphi)_P.$ Then there exists open
$U$ containing $P$ and
$s\in\mathscr{F}(U)$ such that $\varphi(U)(s)=0.$ Thus
$\varphi(s)_P=\varphi_P(s_P)=0.$ Moreover, if $s_P\in\text{ker}(\varphi_P)$,
then there exists an open $U$ containing $P$ such that $\varphi(U)(P)=0.$

The argument follows similarly for images, or see the sheafification
construction which explicitly constructs $\text{im}(\varphi)$ such that
$\text{im}(\varphi)_P=\text{im}(\varphi_P).$

**1.2.b.** Note that $\text{ker}(\varphi)=0$ if and only if
$\text{ker}(\varphi)_P=0$
for all $P\in X.$ By part (a), this is true if and only if
$\text{ker}(\varphi_P)$ for all $P$, i.e., if the morphism is injective on
the stalks.

Let $\varphi:\mathscr{F}\to\mathscr{G}$ be a morphism of sheaves. If
$\text{im}(\varphi)=\mathscr{G}$ then $\text{im}(\varphi)_P=\mathscr{G}_P$
for all $P.$ Moreover, $\text{im}(\varphi_P)=\text{im}(\varphi)_P$ by part
(a). Suppose conversely that
$\text{im}(\varphi_P)=\mathscr{G}_P$ for all $P\in X.$ Suppose that
$U\subset X$ and $s\in\mathscr{G}(U).$ Then $s_P\in\text{im}(\varphi_P)$
for each $P\in U.$ So there exist $V_p\subset U$ and
$s^p\in\text{im}(\varphi)(V_p)$ such that $s^p=s|_{V_P}.$ But then by the
sheaf axiom, $s\in\text{im}(\varphi)(U).$ So
$\text{im}(\varphi)=\mathscr{G}.$

**1.2.c.** This follows immediately from parts (a) and (b).

**Lemma 1.** Suppose that $\mathscr{F}\subset\mathscr{G}$ with $\mathscr{F}$ a
presheaf and $\mathscr{G}$ a sheaf. We define by
$$
    \overline{\mathscr{F}}(U)=\left\{s\in\mathscr{G}(U) : \exists\text{ a cover
    } \{V_i\} \text{ of } U \text{ and } s_i\in\mathscr{F}(V_i),
    s|_{V_i}=s_i\right\}.
$$
Then $\overline{\mathscr{F}}=\mathscr{F}^+$ where $\mathscr{F}^+$ is the
sheafification of $\mathscr{F}.$

<div class="proof">
It is easy to see that $\overline{\mathscr{F}}$ is a sheaf. By the universal
property of the sheafification of $\mathscr{F}$, denoted
$\mathscr{F}^+$, we have morphisms $f:\mathscr{F}\to\mathscr{F}^+$ and
$\psi:\mathscr{F}^+\to\overline{\mathscr{F}}$ such that the inclusion map
$i:\mathscr{F}\to\overline{\mathscr{F}}$ factors as $i=\psi\circ f.$ On the
other hand, given $s\in\overline{\mathscr{F}}(U)$ we may choose a cover
$\{V_i\}$ of $U$ and $s_i\in\mathscr{F}(V_i)$ with $s|_{V_i}=s_i.$ There also
exists $t\in F^+$ such that $t|_{V_i}=s_i.$ Thus we may define
$\beta:\overline{\mathscr{F}}(U)\to\mathscr{F}^+(U)$ by $s\mapsto t.$ This
gives us a morphism $\beta:\overline{\mathscr{F}}\to\mathscr{F}^+$ and
$\beta$ and $\psi$ are inverses.
</div>

**Corollary 2.** Suppose that $f:\mathscr{F}\to\mathscr{G}$ is a morphism of
sheaves.
Then $\text{im}(f)\subset\mathscr{G}.$

**1.3.a.**
By the lemma above we see that the sheafification of the image presheaf of
$\varphi$, regarded as a subsheaf of $\mathscr{G}$,
is equal to $\mathscr{G}$ if and only if the condition is satisfied.

**1.3.b.**
Let $X=\{P,Q,R\}$ with nontrivial open sets $U_P=\{P,R\}$, $U_Q=\{Q,R\}$,
and $U_R=\{R\}.$
Take $\mathscr{F}$ to
be the constant sheaf of $\mathbb{Z}$ on $X$ and $\mathscr{G}$ to be
$\mathbb{Z}_P\oplus\mathbb{Z}_Q$ where $\mathbb{Z}_P,\mathbb{Z}_Q$ are the
skyscraper sheaf over $P$ and $Q$ respectively. Let us define
$\varphi:\mathscr{F}\to\mathscr{G}$ such that
$\varphi(X):\mathscr{F}(X)\to\mathscr{G}(X)$ is the map $z\mapsto (z,z)$,
and the rest of the maps are induced by restrictions. Observe that this
morphism is surjective since given $s=(z_1,z_2)\in\mathscr{G}(X)$ we have
$z_1\in\mathscr{F}(U_P)$ and $z_2\in\mathscr{F}(U_Q)$ such that
$\varphi(U_P)(z_1)=s|_{U_P}$ and $\varphi(U_Q)(z_2)=s|_{U_Q}.$ Thus $\varphi$
is a surjective morphism despite the fact that $\varphi(X)$ is not surjective.

**1.4.a.**
Suppose that $s,t\in\mathscr{F}^+(U)$ such that
$\varphi^+(U)(s)=\varphi^+(U)(t).$ By the definition of the associated
sheaf there exists an open cover $\{V_i\}$ of $U$ such that there exist
$s_i,t_i\in\mathscr{F}(V_i)$ with $s_i=s|_{V_i}$, $t_i=t|_{V_i}.$ Thus
$\varphi(s_i)=\varphi(t_i)$ for each $i.$ Hence $s_i=t_i$ for each $i$ and
thus $s=t.$

**1.4.b.**
Already showed in Lemma 1 and Corollary 2 earlier.

**1.5.**
If a morphism $\varphi:\mathscr{F}\to\mathscr{G}$ is bijective, then
$\varphi(U):\mathscr{F}(U)\to\mathscr{G}(U)$ is bijective for each
$U\subset X$, i.e., $\varphi(U)$ is a group (ring, module, etc.) isomorphism.
Thus we may define an inverse morphism $\varphi^{-1}$ by taking
$\varphi^{-1}(U)=(\varphi(U))^{-1}.$ It easily follows that this is a morphism
of sheaves and it is clearly the inverse of $\varphi.$

**1.6.a.**
Observe that the map $\mathscr{F}\to\mathscr{F}/\mathscr{F}'$ given locally by
$s\mapsto s+\mathscr{F}'(U)$ for $s\in\mathscr{F}(U)$ is surjective onto the
quotient presheaf. Thus it is surjective onto the quotient sheaf as the image
sheaf of this map and quotient sheaf $\mathscr{F}/\mathscr{F}'$ are both the
sheafification of the the quotient presheaf.

**1.6.b.**
Since $\mathscr{F}'\hookrightarrow\mathscr{F}$ we may identify $\mathscr{F}'$
with a subsheaf of $\mathscr{F}.$ Thus by (a) we have
$\mathscr{F}''\cong\mathscr{F}/\mathscr{F}'.$

**1.7.a.** Observe that by Exercise 1.6.a we have an exact sequence
$$
    0\to\text{ker}(\varphi)\to\mathscr{F}\to\text{im}(\varphi)\to 0.
$$
Thus by Exercise 1.6.b we have $\text{im}\varphi\cong\mathscr{F}/\text{ker}
\varphi.$

**1.7.b.**
This also follows immediately from Exercise 1.6, or by noting that the stalks
are isomorphic.

**1.1.8.** Suppose that
$$
    0\to\mathscr{F}'\xrightarrow{\varphi}\mathscr{F}\xrightarrow{\psi}\mathscr{F}''
$$
is an exact sequence of sheaves on $X.$ In order to show that $\Gamma(U,\cdot)$
is right exact, it suffices to show that
$$
    0\to\mathscr{F}'(U)\xrightarrow{\varphi(U)}\mathscr{F}(U)
    \xrightarrow{\psi(U)}\mathscr{F}''(U)
$$
is exact for each open $U\subseteq X.$ Equivalently, we must show that $\varphi(U)$
is injective and that $\text{im}(\varphi(U))=\text{ker}(\psi(U)).$
Observe that since $\varphi$ is injective, it immediately follows (from
Eisenbud-Harris exercise I-$9$ for example) that
$\varphi(U):\mathscr{F}'(U)\to\mathscr{F}(U)$
is injective for each $U\subseteq X.$ Thus
$0\to\mathscr{F}'(U)\to\mathscr{F}(U)$ is exact for each $U.$

It remains to show that $\text{im}(\varphi(U))=\text{ker}(\psi(U))$ for any
opens subset $U.$ Observe that by the exact sequence of sheaves we have that
$\text{im}(\varphi)=\text{ker}(\psi)$ (by Corollary 2 below we may view
$\text{im}(\varphi)$ as a subsheaf of $\mathscr{F}$ and then the surjectivity of
the maps
$\varphi_x:\mathscr{F}_x'\to\text{ker}(\psi)_x$ gives us that
$\text{im}(\varphi)=\text{ker}(\psi)$). Thus the morphism
$\varphi|_{\mathscr{F}'}:\mathscr{F}'\to\text{ker}(\psi)$ is surjective.
Moreover,
$\varphi$ is bijective since $\text{ker}(\varphi)=0.$
Therefore $\varphi|_{\mathscr{F}'}(U):\mathscr{F}'(U)\to\text{ker}(\psi)(U)$ is
bijective for each $U$ (see Eisenbud-Harris exercise I-$9$).

**1.9.** *Products*.
Clearly $\mathscr{F}\oplus\mathscr{G}$ is a presheaf with restriction maps
given by the direct product of the restriction maps on
$\mathscr{F},\mathscr{G}.$

Suppose that $(s_1,s_2)\in\mathscr{F}\oplus\mathscr{G}(U)$ and
$(t_1,t_2)\in\mathscr{F}\oplus\mathscr{G}(V)$ such that $(s_1,s_2)|_{U\cap
V}=(t_1,t_2)|_{U\cap V}.$ Then $s_i|_{U\cap V}=t_i|_{U\cap V}$, $i=1,2$,
and thus there exists a unique $r_1\in\mathscr{F}(U\cup V)$ and
$r_2\in\mathscr{G}(U\cup V)$ such that $r_i|_U=s_i$ and $r_i|_V=t_i.$ Thus
$\mathscr{F}\oplus\mathscr{G}$ satisfies the sheaf axiom.

It remains to show that $\mathscr{F}\oplus\mathscr{G}$ satisfies the universal
properties of direct sum and direct product. Let $i_1,i_2$ be the inclusion
moprhims and $p_1,p_2$ the projection moprhims of $\mathscr{F},\mathscr{G}$
respectively.

Suppose $\mathscr{H}$ is a sheaf
and $\varphi_1:\mathscr{H}\to\mathscr{F}$ and
$\varphi_2:\mathscr{H}\to\mathscr{F}$ morphisms. Then observe that
$\psi:\mathscr{H}\to\mathscr{F}\oplus\mathscr{G}$ defined by
$\psi(U)=(i_1\circ\varphi_1)(U)\oplus
(i_2\circ\varphi_2)(U)$ is a map satisfying $p_1\circ\psi=\varphi_1$ and
$p_2\circ\psi=\varphi_2.$

Suppose $\mathscr{H}$ is a sheaf and $\varphi_1:\mathscr{F}\to\mathscr{H}$ and
$\varphi_2:\mathscr{G}\to\mathscr{H}$ are morphisms. Then
$\psi:\mathscr{F}\oplus\mathscr{G}\to\mathscr{H}$ defined by
$\psi(U)=\varphi_1(U)\oplus\varphi_2(U)$ is a morphism satisfying $\psi\circ
i_1=\varphi_1$ and $\psi\circ i_2=\varphi_2.$

**1.10.** *Direct limits*.
Given the directed system $g_i:\mathscr{F}_i\to\mathscr{G}$, we locally have
maps $f_i(U):\mathscr{F}_i(U)\to\varinjlim\mathscr{F}_i(U)$ and a map
$\varphi(U):\varinjlim\mathscr{F}_i(U)\to\mathscr{G}(U)$ such that
$g_i(U)=\varphi(U)\circ f_i(U).$ These define morphisms of presheaves showing
that the presheaf $U\mapsto\varinjlim\mathscr{F}_i(U)$ is a direct limit in the
category of presheaves. Thus the sheafification $\varinjlim \mathscr{F}_i$
is a direct limit in the category of sheaves.

**1.11.** *Inverse limits*.
Suppose that $s\in\varinjlim\mathscr{F}_i(U)$, $t\in\varinjlim\mathscr{F}_i(V)$
such that $s|_{U\cap V}=t|_{U\cap V}.$ Then by the definition of direct limit,
there exists a $j$ such that $s'\in\mathscr{F}_j(U)$, $t'\in\mathscr{F}_j(V)$
with $s'\mapsto s$, $t'\mapsto t$, and $s'|_{U\cap V}=t'|_{U\cap V}.$ Thus
by the sheaf axiom on $\mathscr{F}_j$ there esists $r'\in\mathscr{F}_j(U\cup
V)$ such that $r'|_U=s'$ and $r'|_V=t'.$ Therefore $r'\mapsto
r\in\varinjlim\mathscr{F}_i(U\cup V).$ Since the restriction map
$\varinjlim\mathscr{F}_i(U\cup V)\to\varinjlim\mathscr{F}_i(U)$ commutes with
the restriction map $\mathscr{F}_j(U\cup V)\to\mathscr{F}_j(U)$ we see that
$r|_U=s$ since $r'|_U=s'.$ Similarly $r|_V=t.$

Now since $X$ is Noetherian, any open cover $\{U_\alpha\}$ of an open set
$U\subset X$ has a finite subcover $\{U_1,\ldots,U_r\}.$ Thus we may glue
sections of arbitrary open covers iteratively via the pairwise gluing outlined
above. Hence the direct limit presheaf is in fact a sheaf.

**1.12.**
Suppose that $\{U_\alpha\}$ is an open cover of $U\subset X$ and
$s_\alpha\in\varprojlim\mathscr{F}_i(U_\alpha)$ such that the $s_\alpha$ are
agree on intersections. Then for each projection map
$\pi_k:\varprojlim\mathscr{F}_i(U)\to\mathscr{F}_k(U)$ the sections
$\pi_k(s_\alpha)\in\mathscr{F}_k(U_\alpha)$ agree on intersections as well.
Thus for each $k$ the sections $\pi_k(s_\alpha)$ glue to a section
$s^k\in\mathscr{F}_k(U).$ Considering $\varprojlim\mathscr{F}_i(U)$ as
the set
$$
    \left\{(a_i)\in\prod\mathscr{F}_i(U) : \varphi_{ij}(U)(a_i)=a_j \text{ for
    all } i,j\right\}
$$
where $\varphi_{ij}:\mathscr{F}_i\to\mathscr{F}_j$ are the maps of the directed
system of sheaves. Thus,
since $\varphi_{ij}(s^i)=s^j$ for all $i,j$, we have in fact constructed an element
$s=(s^k)\in\varprojlim\mathscr{F}_i(U).$ Moreover, $s|_{U_\alpha}=s_\alpha$ as
desired. Thus $\varprojlim\mathscr{F}_i$ is a sheaf.

This object clearly satisfies the properties of an inversel limit in the
category of sheaves since it satisfies the inverse limit property locally for
each open set.

**1.13.** *Espace Étale of a Presheaf*. See the corresponding Eisenbud
exercise I-8.

**1.14.** *Support.*
Suppose $Q\in U\setminus\text{Supp}(s).$ Then $s_Q=0$ so there exists an open
neighborhood $V\subset U$ of $Q$ such that $s|_{V}=0.$ So $V\subset
U\setminus\text{Supp}(s).$ Thus $\text{Supp}(s)$ is open.

**1.15.**
Observe that if $\varphi,\psi:\mathscr{F}|_U\to\mathscr{G}|_U$ is a morphism
then so are
$(\varphi+\psi),(-\varphi):\mathscr{F}|_U\to\mathscr{G}|_U$ defined locally
for each open $U\subset V$ by $(\varphi+\psi)(V)
=\varphi(V)+\psi(V)$ and $(-\varphi)(V)=-\varphi(U)$, respectively.
Thus $\text{Hom}(\mathscr{F}|_U,\mathscr{G}|_U)$ is an
abelian group with addition defined as above and the zero morphism as the
identity.

**1.16.a.**
Let us first recall that if $X$ is irreducible then any nonempty subset $U\subseteq X$ is
connected. To see this, observe that if $U=U_1\cup U_2$ with $U_1,U_2$
nonempty proper open subsets,
then $X\setminus(U_1\cap U_2)=(X\setminus U_1)\cup (X\setminus U_2).$ Therefore
$U_1\cap U_2\ne\emptyset$ since $X\setminus U_1$ and $X\setminus U_2$ are
nonempty proper closed subsets of $X.$

Let $\mathscr{F}$ be the constant sheaf associated to a group $A$ on an
irreducible space $X.$ Observe
that since each nonempty open $U\subset X$ is connected, $\mathscr{F}(U)=A.$
Moreover, all restriction maps must be the identity (unless restricting to
$\mathscr{F}(\emptyset)=0$). Hence restriction maps are surjective and
$\mathscr{F}$ is flasque.

**Notation.** Let us use $\varphi$ to denote the morphism $\mathscr{F}'\to\mathscr{F}$ and
$\psi$ to denote the morphism $\mathscr{F}\to\mathscr{F}''.$

**1.16.b.** By Exercise 1.1.8 it suffices to show that the map
$\psi(U):\mathscr{F}(U)\to\mathscr{F}''(U)$ is surjective.

**Claim 1.** Let $s\in\mathscr{F}''(U).$
Suppose that $U_1,U_2\subset U$ are open such that there exist
$t_1\in\mathscr{F}(U_1)$ and $t_2\in\mathscr{F}(U_2)$ with $\psi(U_1)(t_1)=s|_{U_1}$
and $\psi(U_2)(t_2)=s|_{U_2}.$ Then there exists $t\in\mathscr{F}(U_1\cup U_2)$ such
that $\psi(U_1\cup U_2)(t)=s|_{U_1\cup U_2}.$

<div class="proof">
Let $U_1,U_2\subset U$ be open sets such that there exist
$t_1\in\mathscr{F}(U_1)$ and $t_2\in\mathscr{F}(U_2)$ with $\psi(U_1)(t_1)=s|_{U_1}$
and $\psi(U_2)(t_2)=s|_{U_2}.$ Then
$$
    t_1|_{U_1\cap U_2}-t_2|_{U_1\cap U_2}=u\in\text{ker}(\psi(U_1\cap U_2))=
\text{im}(\varphi(U_1\cap U_2)).
$$
Since $\mathscr{F}'$ is flasque, there exists $u_1\in\mathscr{F}'(U_1)$ such
that $u_1|_{U_1\cap U_2}=u.$ Thus $\psi(U_1)(t_1-\varphi(u_1))=\psi(U_1)(t_1)$ and
$(t_1-\varphi(u_1))|_{U_1\cap U_2}=t_2|_{U_1\cap U_2}.$ So by the sheaf axiom
$t_1-\varphi(u_1)$ and $t_2$ lift to a section $t\in\mathscr{F}(U_1\cup U_2)$
such that $\psi(U_1\cup U_2)(t)=s|_{U_1\cup U_2}.$
</div>

**Claim 2.** The map $\psi(U)$ is surjective.

<div class="proof">
Let $s\in\mathscr{F}''(U).$ Since $\psi$ is surjective there exists an open
cover $\{V_\alpha\}_{\alpha\in A}$ of $U$ and
$t_\alpha\in\mathscr{F}(V_\alpha)$ such that
$\psi(V_\alpha)(t_\alpha)=s|_{V_\alpha}$ (see Corollary 3 below).

For any subset $B\subseteq A$ define $U_B=\bigcup_{\beta\in B}V_\beta.$
Auppose that $\{V_\alpha\}$ is a minimal cover, i.e., $U_B\ne U$ if $B\ne
A.$

Let us define the set
$$
    S = \left\{(t,B) \, : \, B\subset A, \, t\in\mathscr{F}(U_B),
    \psi(U_B)(t)=s|_{U_B}\right\}.
$$
Observe that $S$ is nonempty since $(t_\alpha,\{\alpha\})\in S$ for each
$\alpha\in A.$ Give $S$ a partial order by $(u_1,B_1)\le (u_2,B_2)$ if
$B_1\subseteq B_2$ and $u_2|_{U_{B_1}}=u_1.$

Let $\{(u_i, B_i)\}_{i\in I}$ be a chain in $S$, i.e., $I$ is some totally ordered
set and
$(u_i,B_i)\le (u_j,B_j)$ whenever $i\le j.$ We will show that every such chain
has an upper bound, with the objective of applying Zorn's lemma to construct a
maximal element of $S.$

Observe that $V=\bigcup_{i\in I}U_{B_i}$ is
an open subset of $V$ covered by $\{U_{B_i}\}_{i\in I}.$ Moreover,
$u_i|_{U_i\cap U_j}=u_j|_{U_i\cap U_j}$ for all $i,j\in I.$ Thus by the sheaf
axiom, there exists $u\in\mathscr{F}(V)$ such that $u|_{U_{B_i}}=u_i$ for all
$i\in I.$ The element $(u,\bigcup_{i\in I}B_i)$ is an upper bound for the chain
$\{(u_i, B_i)\}$ in $S.$

By Zorn's lemma, there exists a maximal element $(u,B)$ in $S.$ Suppose that
$B\ne A$, i.e., $U_B\ne U.$ Then pick $\alpha$ such that $V_\alpha\not\subset
U_B$ and apply Claim 1 to obtain a section $u'\in\mathscr{F}(U_B\cup V_\alpha)$
such that $\psi(U_B\cup V_\alpha)(u')=s|_{U_B\cup V_\alpha}.$ Then
$(u',B\cup\{\alpha\})\not\le (u,B)$, a contradiciton. So it must be
that $B=A$ and $u\in\mathscr{F}(U)$ such that $\psi(U)(u)=s.$
</div>

**1.16.c.**
Let $V\subset U\subset X$ be open subsets. Let $s\in\mathscr{F}''(V).$ Let us
use $\psi$ to denote the surjective morphism $\mathscr{F}\to\mathscr{F}''.$
By part (b) the map $\psi(V):\mathscr{F}(V)\to\mathscr{F}''(V)$ is surjective.
Since $\mathscr{F}$ is flasque, the restriction map
$\mathscr{F}(U)\to\mathscr{F}(V)$ is surjective. Thus there exists
$t\in\mathscr{F}(U)$ such that $\psi((t|_{V})=s.$ But
$\psi(t|_{V})=\psi(t)|_{V}$ and thus $\psi(t)\in\mathscr{F}''(U)$ is such that
$\psi(t)|_V=s.$ Hence the restriction map $\mathscr{F}''(U)\to\mathscr{F}''(V)$
is surjective.

**1.16.d.** 
Suppose $V\subseteq U\subseteq Y$ are open sets. Then since $\mathscr{F}$ is
flasque and $f^{-1}(V)\subseteq f^{-1}(U)\subseteq X$ are open,
the restriction map $\mathscr{F}(f^{-1}(U))\to\mathscr{F}(f^{-1}(V))$ is surjective.
Thus the restriction map
$f_*\mathscr{F}(U)=\mathscr{F}(f^{-1}(U))\to\mathscr{F}(f^{-1}(V))
=f_*\mathscr{F}(V).$

**1.16.e.**
By exercise I-$8$ in Eisenbud we may view $\mathscr{F}$ as the sheaf of
germs of continuous sections of the projection map
$\pi:\overline{\mathscr{F}}\to X$ where
$\overline{\mathscr{F}}=\bigsqcup_{x\in X}\mathscr{F}_x=\{(x,s_x):x\in X,
s_x\in\mathscr{F}_x\}.$ Thus the sheaf of discontinuous functions
defined above simply removes the restriction that sections be
continuous. Thus it is easy to see that $\mathscr{G}$ is a sheaf and that there
is a natural inclusion of $\mathscr{F}$ in $\mathscr{G}.$

It remains to see that $\mathscr{G}$ is flasque. Suppose that $V\subseteq
U\subseteq X$ are open and $s\in\mathscr{G}(V).$ Then we may define
$t\in\mathscr{G}(U)$ by $t(P)=s(P)$ if $P\in U$ and $t(P)=0$ otherwise. Since
$t|_{V}=s$, this shows that the restriction map is surjective.

**1.17.**
Suppose $Q\in\overline{\{P\}}.$ Suppose that $s_1\in i_P(U_1)$ and $s_2\in i_P(U_2)$ for
$U_1,U_2$ containing $Q$, then $Q\in U_1\cap U_2$ implies that $P\in U_1\cap
U_2$ by our choice of $Q.$ Thus we may consider $s_1,s_2$ as elements of $A$
and $s_1=s_2$ in $A$ if and only if $s_1|_{U_1\cap U_2}=s_2|_{U_1\cap U_2}.$ So
$\mathscr{F}_Q=A.$ If, on the other hand, $Q\not\in\overline{\{P\}}$, then
there exists an open neighborood $U$ of $Q$ with $P\not\in U.$ Thus
$\mathscr{F}_Q=0.$

By definition of a pushforward, this is exactly the sheaf given by taking the
pushforward $i_*(A)$ under the given inclusion map $i.$

**1.18.** *Adjoint Property of $f^{-1}$*.

**Note.** *Explicit representation of direct limits.*
We use the following construction of direct limits:
if $A_\alpha$ is a directed system of groups with maps
$\phi_{\alpha,\beta}:A_{\alpha}\to A_{\beta}$, then
$\varinjlim A_\alpha=\{(s,A_\alpha) : s\in A_\alpha\}/\sim$ where
$(s,A_\alpha)\sim (t,A_\beta)$ if there exists $\gamma$ such that
$\phi_{\alpha,\gamma}(s)=\phi_{\beta,\gamma}(t).$ In this way
we may also define addition
$[(s,A_{\alpha})]+[(t,A_{\beta})]=[(\phi_{\alpha,\gamma}(s)+\phi_{\beta,\gamma}(t),
A_{\gamma}]$ where $\gamma\ge \alpha,\beta.$

<div class="proof">
For a given topological space $X$ let
$\text{PSh}_X$ be the category of presheaves on $X$.

We will show below that $f^{-1}$ without sheafification is a left adjoint to $f_*$
in the category of presheaves. Thus there will be a bijection between
the set of presheaf maps $\mathscr{G}\to f_*\mathscr{F}$ and the set of
presheaf maps $f^{-1}\mathscr{G}\to\mathscr{F}$. 
Recall that for a sheaf $\mathscr{G}$ and a presheaf $\mathscr{F}$ there is a
bijection between the set of presheaf maps $\mathscr{F}\to\mathscr{G}$ and the
set of sheaf maps $\mathscr{F}^+\to\mathscr{G}$ where $\mathscr{F}^+$ is the
sheafification of $\mathscr{F}$. Thus after the sheafification of $f^{-1}$
we will have the adjoint bijection in the category of sheaves.

Recall that to show $f^{-1}$ is a left adjoint to $f_*$ it suffices to
define unit and counit natural transformations
$\epsilon:f^{-1}f_*\to1_{\text{PSh}_X}$ and $\eta:1_{\text{PSh}_Y}
\to f_*f^{-1}$, and show that for any $\mathscr{F}\in\text{PSh}_X$ and
$\mathscr{G}\in\text{PSh}_Y$ we have
$f_*\epsilon_{\mathscr{F}}\circ\eta_{f_*\mathscr{F}}=1_{f_*\mathscr{F}}$ and
$\epsilon_{f^{-1}\mathscr{G}}\circ
f^{-1}\eta_{\mathscr{G}}=1_{f^{-1}\mathscr{G}}.$

Observe that
$$
\begin{aligned}[t]
f^{-1}f_*\mathcal{F}(U)
&=\varinjlim_{V\supset f(U)}f_*\mathcal{F}(U)\\
&=\varinjlim_{V\supset f(U)}\mathcal{F}(f^{-1}(V)).
\end{aligned}
$$
Therefore we have a natural map $f^{-1}f_*\mathcal{F}(U)\to \mathcal{F}(U)$
defined by $[(s,\mathcal{F}(f^{-1}(V)))]\mapsto s|_{U}].$ This collection of
maps defines a map of presheaves, as restriction on each presheaf commutes.
For a given presheaf $\mathscr{F}$ on $X$ we will denote this map as
$\epsilon_{\mathscr{F}}:f^{-1}f_*\mathscr{F}\to\mathscr{F}$.
The collection of presheaf maps
will define our counit natural transformation $\epsilon$.

Next, we explcitly write out
$$
\begin{aligned}[t]
    f_*f^{-1}\mathscr{G}(U)&=f^{-1}\mathscr{G}(f^{-1}(U))\\
    &=\varinjlim_{V\supset f(f^{-1}(U))}\mathscr{G}(V).
\end{aligned}
$$
Since $U\supset f(f^{-1}(U))$ we have the natural map $\mathscr{G}(U)\to
f_*f^{-1}\mathscr{G}(U)$ defined by $s\mapsto [(s,\mathscr{G}(U))].$ Again this
commutes with restriction and hence defines a map of presheaves.
Again for a given (pre)sheaf $\mathscr{G}$ on
$Y$ we will denote the this map as $\eta_{\mathscr{G}}:\mathscr{G}\to
f_*f^{-1}\mathscr{G}$ and this collection of presheaf maps will define our
unit natural transformation $\eta$.

It is routine to verify that $\epsilon,\eta$ defined above are natural
transformations. Let us work this out explicitly for $\epsilon$. If
$\varphi:\mathscr{F}_1\to\mathscr{F}_2$ is a map of presheaves on $X$ then
we need to show that the following diagram commutes:
$$
\xymatrix{
    f^{-1}f_*\mathscr{F}_1(U) \ar[r]^{\epsilon_{\mathscr{F}_1}(U)}
    \ar[d]^{(f^{-1}f_*\varphi)(U)}
        & \mathscr{F}_1(U) \ar[d]^{\varphi(U)}\\
    f^{-1}f_*\mathscr{F}_2(U) \ar[r]^{\epsilon_{\mathscr{F}_2}(U)}
        & \mathscr{F}_2(U)
}
$$
Tracing a particular element $[(s,f^{-1}(V))]\in f^{-1}f_*\mathscr{F}_1(U)$,
with $U\subset X$ open,
through the diagram we find
$$
\xymatrix{
    [(s,f^{-1}(V)] \ar[r]
    \ar[d]
        & s|_U \ar[d]\\
    [(\varphi(f^{-1}(V))(s),f^{-1}(V))] \ar[r]
        & \varphi(U)(s|_U)
}
$$
since $\varphi(f^{-1}(V))(s)|_U=\varphi(U)(s|_U)$ by the commutativity of
presheaf maps with presheaf restriction. A similar diagram chase shows that
$\eta$ is also a natural transformation.

It now remains to show that for any presheaf $\mathscr{F}$ on $X$ and
any presheaf $\mathscr{G}$ on $Y$ we have
$f_*\epsilon_{\mathscr{F}}\circ\eta_{f_*\mathscr{F}}=1_{f_*\mathscr{F}}$ and
$\epsilon_{f^{-1}\mathscr{G}}\circ
f^{-1}\eta_{\mathscr{G}}=1_{f^{-1}\mathscr{G}}.$
Again diagram chasing suffices. Let $s\in
f_*\mathscr{F}(U)=\mathscr{F}(f^{-1}(U))$ for $U\subset Y$ open. We have a map
$$
    f_*\mathscr{F}\overset{\eta_{f_*\mathscr{F}}}{\to}
    f_*f^{-1}f_*\mathscr{F}\overset{f_*\epsilon_{\mathscr{F}}}{\to}
    f_*\mathscr{F}.
$$
Diagram chasing we find that (the maps below should technically
have $``(U)"$ in front of
them to indicate that they are the local maps, but this was omitted to
attempt to make things less cluttered)
$$
\begin{aligned}[t]
    (f_*\epsilon_{\mathscr{F}})(\eta_{f_*\mathscr{F}}(s))
        &= (f_*\epsilon_{\mathscr{F}})\left([(s,U)]\right)\\
        &= s|_U.
\end{aligned}
$$
If this is not clear it might help to work it out
explicitly and understand the object $f_*f^{-1}f_*\mathscr{F}$ as
$$
\begin{aligned}[t]
f_*f^{-1}f_*\mathscr{F} &= f^{-1}f_*\mathscr{F}(f^{-1}(U))\\
    &= \varinjlim_{V\supseteq f(f^{-1}(U))}f_*\mathscr{F}(V)\\
    &= \varinjlim_{f^{-1}(V)\supseteq f^{-1}(f(f^{-1}(U)))}\mathscr{F}(f^{-1}(V))\\
    &=  \varinjlim_{f^{-1}(V)\supseteq f^{-1}(U)}\mathscr{F}(f^{-1}(V)).
\end{aligned}
$$
Next, we have the sequence of maps
$$
    f^{-1}\mathscr{G}\overset{f^{-1}\eta_{\mathscr{G}}}{\to}
        f^{-1}f_*f^{-1}\mathscr{G}\overset{\epsilon_{f^{-1}\mathscr{G}}}{\to}
        f^{-1}\mathscr{G}.
$$
Again a diagram chase shows us that for $[(s,V)]\in
f^{-1}\mathscr{G}(U)=\varinjlim_{V\supseteq f(U)}\mathscr{G}(V)$, with
$U\subset X$ open, we have
$$
    \begin{aligned}[t]
        \epsilon_{f^{-1}\mathscr{G}}((f^{-1}\eta_{\mathscr{G}})([(s,V)]))
            &= \epsilon_{f^{-1}\mathscr{G}}([([(s,V)], U])\\
            &= [(s,V)].
    \end{aligned}
$$
It may help to understand the object $f^{-1}f_*f^{-1}\mathscr{G}(U)$
explicitly as
$$
\begin{aligned}[t]
    f^{-1}f_*f^{-1}\mathscr{G}(U) &=
        \varinjlim_{V\supset f(U)}\left(f_*f^{-1}\mathscr{G}(V)\right)\\
        &= \varinjlim_{V\supseteq f(U)}\left(
            \varinjlim_{W\supseteq f(f^{-1}(V))}\mathscr{G}(W)\right).
\end{aligned}
$$
</div>

**1.19.** *Extending a Sheaf by Zero.*

**1.19.a.**

**1.19.b.**

**1.19.c.**

**1.20.** *Subsheaf with Supports*

**1.20.a.**

**1.20.b.** 

**1.21.** *Some Examples of Sheaves on Varieties.* 

**1.21.a.** 

**1.21.b.**

**1.21.c.**

**1.21.d.** 

**1.21.e.** 

**1.22.** *Glueing Sheaves.*
