---
documentclass: article
title: Neukirch
geometry: margin=1in
fontsize: 12pt
published: June 1, 2019
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}
...

Here are my exercise solutions and notes for Neukirch's *Algebraic Number
Theory*.

**1.1.** Recall that $(ab)(\overline{ab})=(a\overline{a})(b\overline{b})$ for
$a,b\in\mathbb{C}$. Therefore $N(ab)=N(a)N(b)$ for $a,b\in\mathbb{Z}[i]$.
Moreover, $N(a)\in\mathbb{Z}_{\ge 0}$ for all $a\in\mathbb{Z}[i]$. Thus if
$ab=1$ we must have $N(a)N(b)=1$ and therefore $N(a)=N(b)=1$.

**1.2.** Suppose that $\alpha\beta=\epsilon\gamma^n$ for
$\alpha,\beta,\gamma,\epsilon\in\mathbb{Z}[i]$ with $\epsilon$ a unit and
$\alpha,\beta$ relatively prime. Since
$\mathbb{Z}[i]$ is a UFD we may write out the prime factorizations
$\alpha=\epsilon'p_1^{a_1}\ldots p_r^{a_r}$ and $\beta=\epsilon''q_1^{b_1}
\ldots q_{\ell}^{b_\ell}$ with $\epsilon',\epsilon''$ units and the $p_i$'s and
$q_j$'s prime. Since $\alpha,\beta$ are relatively prime, the
$p_i$'s and $q_j$'s are all distinct primes. Therefore, since
$\alpha\beta=\epsilon\gamma^n$, we must have $n\mid a_i$
and $n\mid b_j$ for all $i,j$. I.e., we have $\alpha=\epsilon'\xi^n$ and
$\beta=\epsilon''\eta$ where $\xi=p_1^{a_1/n}\ldots p_r^{a_r/n}$ and
$\eta=q_1^{b_1/n}\ldots q_\ell^{b_\ell/n}$.

**1.3.** Observe that if $x^2+y^2=z^2$ for $x,y,z\in\mathbb{Z}$, then
$\alpha\overline{\alpha}=z^2$ for $\alpha=x+iy$. Therefore by Exercise 1.2 we
have $x+iy=\epsilon\xi^2$ with $\epsilon$ a unit and $\xi=u+iv$. Therefore
$x+iy=\epsilon(u^2-v^2+2uvi)$. So $x=\epsilon(u^2-v^2)$ and $y=\epsilon 2uv$.
From Exercise 1 we see that the only units in $\mathbb{Z}[i]$ are $\pm
1,\pm i$. We cannot have $\epsilon=\pm i$ as $x,y\in\mathbb{Z}$. Thus we
may ignore the $\epsilon$ as it just swaps the sign.

Observe that if $(u,v)>1$ then we we would have $(x,y)>1$, a contradiction. If
$u,v$ are both odd, then $u^2-v^2\equiv 0(\text{mod } 3)$ and $2uv\equiv
2(\text{mod } 3)$. Thus $z^2\equiv 2 (\text{mod } 3)$, a contradiction.

**1.4.** Recall that an ordering on a ring satisfies $a\le b$ implies $a+c\le
b+c$ for all $a,b,c\in R$, as well as $0\le a$ and $0\le b$ implies $0\le ab$
for all $a,b\in R$. The first implication shows that $-a\le 0\le a$ for all
$a\ge 0$; in particular $-1\le 0\le 1$. If $i\ge 0$, then $i^2=-1\le 0$, a
contradiction. However, if $i\le 0$ then $-i\ge 0$ and $(-i)^2=-1\le 0$.

**1.5.** Observe that we may define a norm on $\mathbb{Z}[\sqrt{-d}]$ by
$N(\alpha)=\alpha\overline{\alpha}$ where if $\alpha=a+b\sqrt{-d}$ we have
$\overline{\alpha}=a-b\sqrt{-d}$. This gives $N(a+b\sqrt{-d})=a^2+b^2d$. Again
we see that $N(\alpha\beta)=N(\alpha)N(\beta)$ and thus an element $\alpha$ is
a unit if and only if $N(\alpha)=1$. The only elements of
$\mathbb{Z}[\sqrt{-d}]$ with
norm $1$ are $\pm 1$.

**1.6.** After struggling for a while and digging a bit on math stack exchange,
it appears that this question is fairly difficult for its place in the book.
Perhaps I will revisit this question after progressing further.
