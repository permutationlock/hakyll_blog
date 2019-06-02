---
documentclass: article
title: The Brauer-Manin Obstruction
tags: math number theory arithmetic algbraic geometry
header-includes: \usepackage{amsmath,amsthm,amssymb,amsfonts}\usepackage[all]{xy}
---

One of the core objectives of algebraic number theory is determining whether a system
of polynomial equations has solutions in $\mathbb{Z}$, $\mathbb{Q}$, or a
number field $K$.
In the terminology of algebraic geometry this amounts to determining whether a variety
defined over $\mathbb{Q}$ has a $\mathbb{Q}$-rational point (or a $K$-rational
point for some number field $K$).

$$ 
    \xymatrix{ A \ar[r] & B \ar[d] \\
               D \ar[u] & C \ar[l] }
$$

Unfortunately, it is incredibly difficult to do this for general varieties; in the
case of the integers, it has been proven it is impossible produce an algorithm to
determine if a polynomial has integer solutions
([see Hilbert's 10th problem](https://en.wikipedia.org/wiki/Hilbert%27s_tenth_problem)).
One of the next things it is natural to look at then is in what cases does a
curve fail to have rational points.
