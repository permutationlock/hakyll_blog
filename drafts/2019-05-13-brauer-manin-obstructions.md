---
documentclass: article
title: Rational points on varieties
tags: math number theory arithmetic algbraic geometry
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}\usepackage[all]{xy}
---

One of the main objectives of algebraic number theory is determining whether a system
of polynomial equations has solutions in $\mathbb{Z}$, $\mathbb{Q}$, or a
number field $K$.
In the terminology of algebraic geometry this amounts to determining whether a variety,
defined over $\mathbb{Q}$ has a $\mathbb{Q}$-rational point, or a $K$-rational
point for some number field $K$.

Unfortunately, it is incredibly difficult to do this for general varieties. In the
case of the integers, it has been proven it is impossible produce an algorithm to
determine if a polynomial has integer solutions
([see Hilbert's 10th problem](https://en.wikipedia.org/wiki/Hilbert%27s_tenth_problem)).
One of the next things it is natural to look at then is in what cases does a
variety fail to have rational points.

One way to show that a variety $X$ does not have any rational points is to show
that it does not have any points over one of the completions of $\mathbb{Q},$
i.e., show that $X(\mathbb{R})=\emptyset$ or that $X(\mathbb{Q}_p)=\emptyset$ for
some prime $p.$ Over $\mathbb{Q}_p$ this can essentially be reduced to working
modulo $p$ using Hensel's lemma.

The Hasse principle is the idea that a variety that has a point on every
completion of $\mathbb{Q}$ should have a rational point as well.
Unfortunately, it has been shown that there exist varieties where the Hasse
principle fails, i.e., there exist varieties that have points on
every completion of $\mathbb{Q}$ and yet have no rational points.

Naturally, the next step in the quest to determine which varieties have
rational points is to classify when the Hasse principle holds.
The most well understood reason for failure of the Hasse principle is
the so called Brauer-Manin obstruction arising from the
Brauer-Grothendieck group of a variety. This group is
a generalization of the
Brauer group of a field $K$ which is the group of central simple algebras over
$K$ modulo matrix algebras. My masters project focuses on producing an example
of a Brauer-Manin obstruction to rational points on a surface.
