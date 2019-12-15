---
documentclass: article
title: Transcendental Brauer-Manin obstruction on a surface with a fibration in degree four curves
geometry: margin=1in
fontsize: 12pt
published: August 27, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...  

In $2004$ Olivier Wittenberg described an example of a transcendental
Brauer-Manin obstruction to weak approximation on an elliptic surface over
$\mathbb{Q}$.

The Hasse principle is the idea that if
$X/\mathbb{Q}$ has a $\mathbb{Q}_v$-rational point for every
completion $\mathbb{Q}_v$, then it should have
a $\mathbb{Q}$-rational point. 

Unfortunately this doesn't always hold.
The Brauer-Manin obstruction is a way to explain some failures of the Hasse
principle. 

Each variety $X/K$ has an associated Brauer group
$\text{Br}(X)$. 

One way to think of $\text{Br}(X)$ is as
$$ 
    \{ \text{unramified central simple algebras over} \ K(X) \} / \{
    \text{matrix algebras} \}
$$
where ``unramified'' means that at every point $x\in
X(\overline{K})$
we could evaluate and get an algebra over the residue field $k(x)$.

The adele ring $\mathbb{A}_\mathbb{Q}=\widehat{\prod}_v\mathbb{Q}_v$
is the restricted product of the
completions $\mathbb{Q}_v$ of $\mathbb{Q}$. A failure of the Hasse
principle is then when
$$
    X(\mathbb{Q})=\emptyset \quad \text{but} \quad
    X(\mathbb{A}_\mathbb{Q})\ne\emptyset.
$$

Each element $\alpha\in\text{Br}(X)$ defines an associated set
$X(\mathbb{A}_{\mathbb{Q}})^\alpha$ satisfying
$$
    X(\mathbb{Q})\subseteq X(\mathbb{A}_{\mathbb{Q}})^\alpha
    \subseteq X(\mathbb{A}_{\mathbb{Q}}).
$$

If $X(\mathbb{A}_{\mathbb{Q}})^\alpha=\emptyset$ but
$X(\mathbb{A}_\mathbb{Q})\ne\emptyset$ we say that $\alpha$ provides a
Brauer-Manin obstruction to the Hasse principle.

If $X(\mathbb{A}_{\mathbb{Q}})^\alpha$ is a proper subset of
$X(\mathbb{A}_{\mathbb{Q}})$ then we say $\alpha$ provides an obstruction
to weak approximation.

The algebraic Brauer group $\text{Br}_1(X)$ of $X/K$ is the kernel of the
base change to $\overline{K}$ map
$$
    \text{Br}(X)\to\text{Br}(\overline{X}).
$$

The transcendental elements of the Brauer group are then those that don't
belong to
$\text{Br}_1(X)$.

The Brauer group of a curve is always
entirely algebraic.

If $E/K$ is a split elliptic curve with
$K$-rational $2$-torsion there is an
explicit description of the
$2$-torsion Brauer group $\text{Br}(E)[2]$.

Wittenberg looks at an elliptic surface
over $\mathcal{E}/\mathbb{Q}$,
i.e. a surface with a fibration
$\pi:\mathcal{E}\to\mathbb{P}^1_\mathbb{Q}$
and generic fiber isomorphic to an elliptic curve $E/\mathbb{Q}(t)$.

There is an
inclusion
$$
    \text{Br}(\mathcal{E}/\mathbb{Q})[2]\subset\text{Br}(E/\mathbb{Q}(t))[2].
$$
So as long as an element in $\text{Br}(\mathcal{E})[2]$ doesn't become
trivial in $\text{Br}(E/\overline{\mathbb{Q}}(t))$ it will be a transcendental element
of $\text{Br}(\mathcal{E})[2]$.

Moreover, to check whether an element of $\text{Br}(E/\mathbb{Q}(t))[2]$
lies in $\text{Br}(\mathcal{E})$ it is
enough to check ramification along the finite number of special
fibers.

In Wittenberg's paper he finds a particular elliptic surface where this all works
out to find a transcendental element of $\text{Br}(\mathcal{E})$
that gives an obstruction
to weak approximation.

### References

(1) Wittenberg, Olivier, Transcendental Brauer-Manin obstruction on a pencil
    of elliptic curves. *Arithmetic of higher-dimensional algebraic varieties
    (Palo Alto, CA, 2002)*, 259–267, Progr. Math., 226, Birkhäuser Boston,
    Boston, MA, 2004.
    Available:
    [https://arxiv.org/abs/1502.04386](https://arxiv.org/abs/1502.04386)
