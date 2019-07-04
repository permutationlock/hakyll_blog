---
documentclass: article
title: Hartshorne II.2
geometry: margin=1in
fontsize: 12pt
published: June 16, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.1.** Let $\varphi:A\to A_f$ be the natural homomorphism given by $a\mapsto
a/1.$ Observe that $\varphi$ induces a bijection between the ideals of $A$ not
containing $f$ and the ideals of $A_f$ via the identification
$\mathfrak{a}\mapsto\mathfrak{a}A_f$ and
$\varphi^{-1}(\mathfrak{b})\leftarrow\mathfrak{b}.$ This identification also
identifies prime ideals with prime ideals and therefore defines a bijection
$\psi:D(f)\to\text{Spec}(A_f)$. Moreover, by our observations above,
$\psi$ and $\psi^{-1}$ each send closed sets to closed sets. Hence $\psi$ is a
homeomorphism.

It remains to show that we have a morphism
$\psi^{\#}:\mathscr{O}_{\text{Spec}(A_f)}\to\mathscr{O}_X|_{D(f)}.$ Observe
that for $g/f^{n}\in A_f$ the set $D(g/f^{n})\subset\text{Spec}(A_f)$ is
equal to $D(g/1)\subset\text{Spec}(A_f)$ and $(A_{f})_{g/f^n}\cong A_{fg}.$
Note $D(g/1)=D(f)$ if $g\in (f).$
The basic open sets of $D(f)$ are precisely the sets $D(g)\cap D(f)$
for $g\in A$ and $D(g)\cap D(f)=D(f)$ when
$g\in (f).$ Thus $\psi$ identifies the basic open sets of $D(f)$ with the basic
open sets of $\text{Spec}(A_f).$ Moreover, $\mathscr{O}_X|_{D(f)}(D(g))\cong
A_{fg}\cong\mathscr{O}_{\text{Spec}(A_f)}(D(g/1))$ and these isomorphisms are
uniquely determined from the natural isomorphism
$\Gamma(D(f),\mathscr{O}_X|_{D(f)})\cong
\Gamma(\text{Spec}(A_f),\mathscr{O}_{\text{Spec}(A_f)}.$ These isomorphisms
define
$\psi^{\#}(D(g)):\mathscr{O}_{\text{Spec}(A_f)}(D(g/1))\to
\psi_*(\mathscr{O}_X|_{D(f)})(D(1/g)=\mathscr{O}_X|_{D(f)}(D(g))$ and
thus they glue to define $\psi^{\#}(U)$ for all open sets. Since each is an
isomorphism, $\psi^{\#}_{[\mathfrak{a}]}$ is clearly a local homomorphism of local
rings for each $[\mathfrak{a}]\in\text{Spec}(A_f).$

**2.2.** Let $\{U_\alpha\}$ be an affine open cover for $X$
with $U_\alpha\cong\text{Spec}(A^\alpha).$ Observe that
$U\cap U_\alpha$ is an open subset of $U_\alpha$ and hence $U\cap
U_\alpha=\bigcup_{\beta} U_{\alpha,\beta}$ such that for each $\beta$ we have
$U_{\alpha,\beta}\cong D(f_{\alpha,\beta})\subseteq\text{Spec}(A_{f_\alpha})$
with $f_{\alpha,\beta}\in A^\alpha.$ From proposition 2.2 we know
$D(f_{\alpha,\beta})\cong\text{Spec}(A^\alpha_{f_{\alpha,\beta}}).$
Therefore $U=\bigcup_{\alpha,\beta}U_{\alpha,\beta}$ and $\{U_{\alpha,\beta}\}$
is an affine open cover of $U.$ Therefore $(U,\mathscr{O}_X|_U)$ is a scheme.


**2.3.**

**2.4.** Let us first show the result for affine schemes.

Suppose that $\varphi:A\to B$ is a ring homomorphism. Then we may define a map
$f:\text{Spec}(B)\to\text{Spec}(A)$ by
$\mathfrak{p}\mapsto\varphi^{-1}(\mathfrak{p}).$ Observe that
$f^{-1}(V(\mathfrak{a}))=V(\varphi(\mathfrak{a}))$ so $f$ is continuous.
Observe that for $D(g)\subset\text{Spec}(A)$ we have
$\mathscr{O}_{\text{Spec}(A)}(D(g))\cong A_f.$ Moreover,
$$
    \begin{aligned}[t]
        f_*\mathscr{O}_{\text{Spec}(B)}(D(g))
            &=\mathscr{O}_{\text{Spec}(B)}(f^{-1}(D(g))\\
            &=\mathscr{O}_{\text{Spec}(B)}(D(\varphi(g)))\\
            &\cong B_{\varphi(g)}.
    \end{aligned}
$$
We therefore define $f^{\#}(D_f):A_f\to B_{\varphi(g)}$ to be the map induced
by $\varphi:A\to B.$ These glue to define a morphism
$f^{\#}:\mathscr{O}_{\text{Spec}(A)}\to f_*\mathscr{O}_{\text{Spec}(B)}$ with
$f^{\#}(\text{Spec}(A))=\varphi.$

It remains to show that
$f^{\#}_{f([\mathfrak{p}])}:\mathscr{O}_{text{Spec}(A),f([\mathfrak{p}])}\to
\mathscr{O}_{text{Spec}(B),[\mathfrak{p}]}$ is a local homomorphism of local
rings for each prime ideal $\mathfrak{p}\subseteq B.$ Observe that
$\mathscr{O}_{\text{Spec}(A),f([\mathfrak{p}])}\cong A_{\varphi^{-1}(\mathfrak{p})},$
$\mathscr{O}_{\text{Spec}(B),[\mathfrak{p}]}\cong B_{\mathfrak{p}},$ and
$f^{\#}_{f([\mathfrak{p}])}:A_{\varphi^{-1}(\mathfrak{p})}\to B_{\mathfrak{p}}$
is given by $s/t\mapsto\varphi(s)/\varphi(t).$ Thus
$$
    \begin{aligned}[t]
    (f^{\#}_{f([\mathfrak{p}])})^{-1}(\mathfrak{m}_{\mathfrak{p}})&=
        \{s/t\in A_{\varphi^{-1}(\mathfrak{p})} \, : \,
        \varphi(s)/\varphi(t)\in\mathfrak{m}_{\mathfrak{p}}\}\\
        &=\{s/t\in A_{\varphi^{-1}(\mathfrak{p})} \, : \,
            s\in\varphi^{-1}(\mathfrak{p})\}\\
        &=\mathfrak{m}_{\varphi^{-1}(\mathfrak{p})}.
    \end{aligned}
$$
So $f^{\#}_{f([\mathfrak{p}])}$ is a local homomorphism of local rings for each
$[\mathfrak{p}]\in\text{Spec}(B)$ and $f^{\#}$ is a morphism of schemes.

Finally, note that if $f:\text{Spec}(B)\to\text{Spec}(A)$ is a morphism of
schemes, the first part of our work above shows that the homomorphism
$f^{\#}(\text{Spec}(A)):A\to B$ determines the morphism $f^{\#}.$ So there is a
bijection $\text{Hom}(\text{Spec}(B),\text{Spec}(A))\cong\text{Hom}(A,B).$

Now suppose that $X$ is an arbitrary scheme and
$\varphi:A\to\Gamma(X,\mathscr{O}_X)$ is a homomorphism.
Let $\{U_\alpha\}$ be an affine open cover of $X.$ The map
$\varphi:A\to\Gamma(X,\mathscr{O}_X)$ composed with the
restriction map
$\text{res}_{X,U_\alpha}:\Gamma(X,\mathscr{O}_X)\to\Gamma(U_\alpha,\mathscr{O}_X)$
is a ring homomorphism $\text{res}_{X,U_\alpha}\circ\varphi:A\to\Gamma(U_\alpha,
\mathscr{O}_X).$ Therefore $\text{res}_{X,U_\alpha}\circ\varphi$
determines a morphism of affine schemes
$f_\alpha:U_\alpha\to\text{Spec}(A).$
Now let $\alpha,\beta$ be distinct indexes. We observe that $f_\alpha|_{U_\alpha\cap
U_\beta}=f_\beta|_{U_\alpha\cap U_\beta}$ because for each affine open set
$V\subset U_\alpha\cap U_\beta$ we have
$f^{\#}_\alpha|_V(\text{Spec}(A))=\text{res}_{X,V}\circ\varphi
=f^{\#}_\beta|_V(\text{Spec}(A))$ and we know this ring homomorphism
determines $f_\alpha|_V$ and $f_\beta|_V$ since $V$ is affine. Thus the $f_\alpha$ glue
to a morphism of schemes $f:X\to\text{Spec}(A).$

The argument also shows that any morphism $f:X\to\text{Spec}(A)$ is determined
by $f^{\#}(\text{Spec}(A)):A\to\Gamma(X,\mathscr{O}_X).$ Thus there is a
bijection
$\text{Hom}(X,\text{Spec}(A))\cong\text{Hom}(A,\Gamma(X,\mathcal{O}_X)).$
