---
documentclass: article
title: Fulton 2.5
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.23.\*** Show that the order function on $K$ is independent of the choice
of uniformizing parameter.

<div class="proof">
Suppose $t,s$ are uniformizing parameters on $R.$ Note that $s=ut,$ with $u$ a
unit. Let $z\in K.$ Observe that $z=vt^n,$ $n\in\mathbb{Z},$ $v$ a unit in $R.$ So
$z=vu^{-n}(ut)^n=vu^{-n}s^n.$
</div>

**2.24.\*** Let $V=\mathbb{A}^1,$ $\Gamma(V)=k[X],$ $K=k(V)=k(X).$

\(a\) For each $a\in k=V,$ show that $\mathcal{O}_{a}(V)$ is a DVR, with uniformizing
parameter $t=X-a.$

<div class="proof">
Recall that all local rings of points are Noetherian and local. Moreover,
$\mathfrak{m}_{a}(V)=(X-a),$ so the maximal ideal of $\mathfrak{m}_{a}(V)$ is principal with
generator $X-a.$
</div>

\(b\) Show that $\mathcal{O}_{\infty}=\{F/G\in k(X) \ | \ \deg(G)\ge\deg(F)\}$ is also a
DVR, with uniformizing parameter $t=1/X.$

<div class="proof">
Observe that $F/G\in\mathcal{O}_{\infty}$ is a unit if and only if $\deg(F)=\deg(G).$
Let $F/G\in\mathcal{O}_{\infty},$ $n=\deg(G)-\deg(F).$ Then $(1/X^n)(FX^n/G)=F/G$ and
$FX^n/G$ is a unit. Therefore $1/X$ is a uniformizing parameter for
$\mathcal{O}_{\infty}.$
</div>

**2.25.** Let $p\in\mathbb{Z}$ be a prime number. Show $\{r\in\mathbb{Q} \ | \ r=a/b, \ 
a,b\in\mathbb{Z}, \ p\nmid b\}$ is a DVR with quotient field $\mathbb{Q}.$

<div class="proof">
Let $r=a/b\in\mathbb{Q}$ such that $p\nmid b.$ Let $c\in\mathbb{Z}$ such that $a=p^nc,$
$n\ge 0,$ $p\nmid c.$ Then $r=p^nc/b$ and $c/b$ is a unit. Thus the set is a
DVR with uniformizing parameter $p.$ Moreover, the quotient field is
$\{p^na/b \ | \ n,a,b\in\mathbb{Z}, \ p\nmid a, \ p\nmid b\}=\mathbb{Q}.$
</div>

**2.26.\*** Let $R$ be a DVR with quotient field $K$; let $\mathfrak{m}$ be the
maximal ideal of $R.$

\(a\) Show that if $z\in K,$ $z\not\in R,$ $z^{-1}\in\mathfrak{m}.$

<div class="proof">
We may write $z=ut^n,$ with $u$ a unit and $(t)=\mathfrak{m}\subset R.$ Since
$z\not\in R,$ $n<0.$ Therefore $z^{-1}= u^{-1}z^{-n}\in R.$
</div>

\(b\) Suppose $R\subset S\subset K,$ and $S$ is also a DVR. Suppose the maximal
ideal of $S$ contains $\mathfrak{m}.$ Show that $S=R.$

<div class="proof">
Let $\mathfrak{m}'=(s)$ be the maximal ideal of $S.$ Suppose $s\in R.$
Then since $\mathfrak{m}\subset\mathfrak{m}',$ $\mathfrak{m}'= \mathfrak{m}$ and $S=R.$ Suppose $s\not\in R.$ Then by part (a),
$s^{-1}\in R\subset S$ and $s$ is a unit in $S,$ a contradiction.
</div>

**2.27.** Show that the DVR's of Problem 2.24 are the only DVR's with
quotient field $k(X)$ that contain $k.$

<div class="proof">
Suppose $R\supset k$ is a DVR with field of fractions $k(X).$ Let $(t)=\mathfrak{m}\subset R$
be the maximal ideal of $R.$ Observe that we may treat $R$ as a subring of its
field of fractions $k(X).$

Suppose $X\in R.$ Then the inclusion map $i:k[X]\hookrightarrow R$ is a natural
injective homomorphism. Moreover,  So $i^{-1}(\mathfrak{m})$ is either $k[X]$ or a maximal ideal of $k[X]$ by
Problem 1.22. Thus $\mathfrak{m}=(X-a)$ for some $a\in k.$

Suppose $X\not\in R.$ Then $X=ut^{-n}$ for some $n>0.$ Since $X$ is not a square,
$n=1.$ Therefore $1/X=ut$ and $\mathfrak{m}=(1/X).$
</div>

Show that those of Problem 2.25 are the only DVR's with quotient field $\mathbb{Q}.$

<div class="proof">
Suppose $R$ is a DVR with field of fractions $\mathbb{Q}.$ Observe that $\mathbb{Z}\subset R
\subset\mathbb{Q}.$ Thus the inclusion map $i:\mathbb{Z}\to R$ is a natural injective
homomorphism. So $i^{-1}(\mathfrak{m})$ is a maximal ideal in $\mathbb{Z}.$ Thus
$\mathfrak{m}=(p)$ for some prime $p.$
</div>

**2.28.\*** An *order function* on a field $K$ is a function
$\varphi$ from $K$ onto $\mathbb{Z}\cup\{\infty\},$ satisfying:
 (1) $\varphi(a)=\infty$ if and only if $a=0.$
 (2) $\varphi(ab)=\varphi(a)+\varphi(b).$
 (3) $\varphi(a+b)\ge\min(\varphi(a),\varphi(b)).$
Show that $R=\{z\in K \ | \ \varphi(z)\ge 0\}$ is a DVR with maximal ideal
$\mathfrak{m}=\{z \ | \ \varphi(z)>0\},$ and quotient field $K.$ Conversely, show that
if $R$ is a DVR with quotient field $K,$ then the function $\text{ord}:K\to Z
\cup\{\infty\}$ is an order function on $K.$ Giving a DVR with a quotient field
$K$ is equivalent to defining an order function on $K.$

<div class="proof">
Suppose $\varphi$ is an order function on a field $K$ and $R$ is the set
defined above. Observe that $\varphi(1)=\varphi(1\cdot 1)=\varphi(1)+\varphi(1).$
Thus $\varphi(1)=0.$ Similarly $\varphi(-1)=0.$ Thus for any $a\in K,$
$\varphi(a)=\varphi(-a).$ Thus $R$ is closed under multiplication and
addition. Moreover, $a$ is a unit in $R,$ that is $a^{-1}\in R,$ if and only if
$\varphi(a)=0.$

Next we will show that $\mathfrak{m}$ is an ideal. Let $a,b\in\mathfrak{m}.$ Observe
that $\varphi(a-b)\ge\min(\varphi(a),\varphi(b))>0.$ So $\mathfrak{m}$ is an additive
group. Moreover, if $a\in\mathfrak{m}, r\in R,$ then $\varphi(ar)>\varphi(a).$ So
$ar\in\mathfrak{m}$ and $\mathfrak{m}$ is an ideal.

It remains to show that $\mathfrak{m}$ is principal.
If $\mathfrak{m}=\{0\}$ then we are done. Suppose $\mathfrak{m}\ne\{0\}.$ Then $\phi(\mathfrak{m}\setminus\{0\})\ne\emptyset$
and, by the
well ordering principle, we may select $p\in\mathfrak{m}\setminus\{0\}$ such that for
all $a\in \mathfrak{m},$ $\varphi(p)\le \varphi(a).$
Then for $a\in\mathfrak{m},$ $\varphi(ap^{-1})\ge 0.$ So $a=p(ap^{-1})$ and $p\mid a.$
Thus $\mathfrak{m}=(p).$ So $R$ is a DVR.

Converseley suppose that $R$ is a DVR with quotient field $K.$ It is immediate
that $\text{ord}$ satisfies (i) and (ii). To see that ord satisfies (iii), see Problem 2.29(a).
</div>

**2.29.\*** Let $R$ be a DVR with quotient field $K,$ $\text{ord}$ the order
function on $K.$

\(a\) If $\text{ord}(a)<\text{ord}(b),$ show that $\text{ord}(a+b)=\text{ord}(a).$

<div class="proof">
Let $t$ be the uniformizing parameter for $R.$ Then $a=ut^n,b=vt^m$ with
$n,m\in\mathbb{Z}$ and $u,v$ units of $R.$ Suppose without loss of generality that
$n\le m.$ Then $a+b=t^n(u+vt^{m-n})$ and $\text{ord}(a+b)=n=\text{ord}(a).$
</div>

\(b\) If $a_1,\ldots,a_n\in K,$ and for some $i,$ $\text{ord}(a_i)<\text{ord}(a_j)$ for all
$j\ne i,$ then $a_1+\ldots+a_n\ne 0.$

<div class="proof">
It suffices to show that for any
$\text{ord}(a+1+\ldots+a_n)=\min_{1\le i\le n}(\text{ord}(a_i)).$
If $n=2,$ the statement follows from part (a). Suppose that for $n\ge 2,$
$\text{ord}(a_1+\ldots+a_n)=\min_{1\le i\le n}(\text{ord}(a_i))=m\in\mathbb{Z}\cup\{\infty\}.$ Let $a_{n+1}\in K.$
By part (a), $\text{ord}(a_1+\ldots+a_{n+1})=\min(m,\text{ord}(a_{n+1}))=
\min_{1\le i\le n+1}(\text{ord}(a_i)).$
</div>

**2.30.\*** Let $R$ be a DVR with maximal ideal $\mathfrak{m},$ and quotient field
$K.$ Suppose $k$ is a subfield of $R,$ and that the composition
$k\to R\to R/\mathfrak{m}$ is an isomorphism.

\(a\) For any $z\in R,$ show that there is a unique $\lambda\in k$ such that
$z-\lambda\in\mathfrak{m}.$

<div class="proof">
Let $z\in R.$ Let $\pi:R\to R/\mathfrak{m}$ be the natural homomorphism. Since the
composition $k\to R\to R/\mathfrak{m}$ is an isomorphism, there is a unique $\lambda\in
k$ such that $\pi(z)=\pi(\lambda),$ that is, $z-\lambda\in\mathfrak{m}.$
</div>

\(b\) Let $t$ be the uniformizing parameter for $R,$ $z\in R.$ Show that for any
$n\ge 0$ there are unique $\lambda_0,\ldots,\lambda_n\in k$ and $z_n\in R$ such
that $z=\lambda_0+\lambda_1t+\lambda_2t^2+\ldots+\lambda_nt^n+z_nt^{n+1}.$

<div class="proof">
Let $z\in R.$ By part \(a\) there exists a unique $\lambda_0\in k$ such that
$z-\lambda_0\in\mathfrak{m},$ that is, $z=\lambda_0+z_0t$ for some $z_0\in R.$ So the
statement holds for $n=0.$

Suppose for $n\ge 0$ that $z=\lambda_0+\ldots+\lambda_nt^n+z_nt^{n+1}.$ By
part \(a\) there exists $\lambda_{n+1}\in k,$ $z_{n+1}\in R$ such that
$z_n=\lambda_{n+1}+z_{n+1}t.$ Thus
$z=\lambda_0+\ldots+\lambda_nt^n+\lambda_{n+1}t^{n+1}+z_{n+1}t^{n+2}.$

It remains to show uniqueness. Suppose $z=\lambda_0+\ldots+\lambda_nt^n
+z_nt^{n+1}=\sigma_0+\ldots+\sigma_nt^n+w_nt^{n+1},$ with
$\lambda_i,\sigma_i\in k$ for each $i,$ and $z_n,w_n\in R.$ Then
$0=(\lambda_0-\sigma_0)+\ldots+(\lambda_n-\sigma_n)t^n+(z_n-w_n)t^{n+1}.$ By
2.29b, $\lambda_i-\sigma_i=0$ for each $i,$ and $z_n-w_n=0.$
</div>

**2.31.** Let $k$ be a field. The ring of formal power series over $k,$
written $k[[X]],$ is defined to be $\{\sum_{i=0}^\infty a_iX^i \, | \, a_i\in k\}.$
Define the sum by $\sum a_i X^i+\sum b_iX^i=\sum (a_i+b_i)X^i,$ and
the product $(\sum a_i X^i)(\sum b_i X^i)=\sum c_i X^i,$ where $c_i=\sum_{j+k=i}a_jb_k.$
Show that $k[[X]]$ is a ring containing $k[X]$ as a subring.

<div class="proof">
It is simple to verify that $k[[X]]$ is an additive group with additive identity
$0,$ and $-\sum a_iX^i=\sum(-a_i)X^i.$ Note that multiplication is a well defined
binary operation on $k[[X]]$ with identity $1.$
It remains to show that multiplication is associative and distributes over
addition. To show associativity observe that
$$
\begin{aligned}[t]
	\left(\sum a_iX^i\right)\left(\left(\sum b_iX^i\right)\left(\sum c_iX^i\right)\right)
		&=\left(\sum a_iX^i\right)\left(\sum \left(\sum_{j+k=i}b_jc_k\right)X^i\right)\\
		&=\left(\sum\left(\sum_{j+k=i}a_j\left(\sum_{l+d=k}b_lc_d\right)\right)X^i\right)\\
		&=\left(\sum\left(\sum_{j+l+d=i}a_jb_lc_d\right)X^i\right)\\
		&=\left(\sum\left(\sum_{j+k=i}c_j\left(\sum_{l+d=k}a_lb_d\right)\right)X^i\right)\\
		&=\left(\sum\left(\sum_{j+k=i}a_jb_k\right)X^i\right)\left(\sum c_iX^i\right)\\
		&=\left(\left(\sum a_iX^i\right)\left(\sum b_iX^i\right)\right)\left(\sum c_iX^i\right).
\end{aligned}
$$
To show multiplication distributes observe that
$$
\begin{aligned}[t]
\left(\sum a_iX^i\right)
	\left(\sum b_iX^i + \sum c_iX^i\right)
	&= \sum\left(\sum_{k=0}^i a_k(b_{i-k}+c_{i-k})\right)X^i\\
	&= \sum\left(\sum_{k=0}^i(a_kb_{i-k} + a_kc_{i-k})\right)X^i\\
	&= \sum\left(\sum_{k=0}^ia_kb_{i-k}X^i+\sum_{k=0}^ia_kc_{i-k}X^i\right)\\
	&= \sum\left(\sum_{k=0}^ia_kb_{i-k}\right)X^i +
		\sum\left(\sum_{k=0}^ia_kc_{i-k}\right)X^i\\
	&= \left(\sum a_iX^i\right)\left(\sum b_iX^i\right)
		+ \left(\sum a_iX^i\right)\left(\sum c_iX^i\right).
\end{aligned}
$$
Finally, note that the definition of multiplication for series agrees with the
definition of multiplication for finite sums. Thus we can define an inclusion
map $k[X]\hookrightarrow k[[x]]$
by $\sum_{i=0}^na_iX^i\mapsto\sum_{i=0}^\infty a_iX^i$ with $a_i=0$ for all
$i> n.$ So $k[[X]]$ contains a copy of $k[X]$ as a subring.
</div>

Show that $k[[X]]$ is a DVR with uniformizing parameter $X.$
Its quotientfield is denoted $k((X)).$

<div class="proof">
We will show that every element of $k[[X]]$ may be written as $uX^t$ where $u$
is a unit in $k[[X]].$ It suffices to show that $\sum a_iX^i$ is a unit if and
only if $a_0\ne 0.$

Suppose $\sum a_iX^i\in k[[X]]$ is a unit, that is, $\left(\sum a_iX^i\right)\left(\sum b_iX^i\right)=1.$
Then it must be that $a_0b_0=1,$ thus $a_0,b_0$ are each nonzero. Conversely
suppose $\sum a_iX^i\in k[[X]],$ $a_0\ne 0.$ Define $\sum b_iX^i\in k[[X]]$ by
$b_0=a_0^{-1}$ and $b_i=-a_0^{-1}\sum_{k=1}^i a_kb_{i-k}$ for $i>0.$ Observe that
$$
\begin{aligned}[t]
\left(\sum a_iX^i\right)\left(\sum b_iX^i\right)
	&= \sum_{i=0}^\infty\left(\sum_{k=0}^i a_kb_{i-k}\right)X^i\\
	&=a_0a_0^{-1} + \sum_{i=1}^\infty\left(a_0b_i + \sum_{k=1}^i a_kb_{i-k}\right)X^i\\
	&= 1 + \sum_{i=1}^\infty\left(a_0\left(-a_0^{-1}\sum_{k=1}^i a_kb_{i-k}\right)
		+\sum_{k=1}^i a_kb_{i-k}\right)X^i\\
	&= 1 + \sum_{i=1}^\infty\left(-\sum_{k=1}^i a_kb_{i-k}
		+\sum_{k=1}^i a_kb_{i-k}\right)X^i\\
	&= 1.
\end{aligned}
$$
</div>

**2.32.** Let $R$ be a DVR satisfying the conditions of Problem 2.30. Any
$z\in R$ then determines a power series $\lambda_iX^i,$ if $\lambda_0,\lambda_1,
\ldots$ are deetermined as in Problem 2.30(b).

\(a\) Show that the map $z\mapsto\sum\lambda_i X^i$ is a one-to-one ring homomorphism
of $R$ into $k[[X]].$ We often write $z=\sum\lambda t^i,$ and call this the *power
series expansion* of $z$ in terms of $t.$

<div class="proof">
Let $\varphi:R\to k[[X]]$ be the map defined above.
Let $z,w\in R,$ $\varphi(z)=\sum\lambda_i X^i,$ $\varphi(w)=\sum\sigma_i X^i.$ If
$z=w$ then $\lambda_i=\sigma_i$ for each $i.$ Conversely, if $\lambda_k\ne\sigma_k$
for some $k$ then $z\ne w$; in fact, $z-w=z_kt^{k},$ $z_k\in R,$ and $\text{ord}(z-w)=k.$ Thus
the map is well defined and one-to-one.

It remains to show that $\varphi$ is a homomorphism. Let $z,w\in R,$ and write
$z=\lambda_0+\ldots+\lambda_kt^k+z_{k+1}t^{k+1},$ $w=\sigma_0+\ldots+\sigma_kt^k+w_{k+1}t^{k+1},$
with $\lambda_0,\ldots,\lambda_k,\sigma_0,\ldots,\sigma_k\in k$ and $z_{k+1},w_{k+1}\in R.$
Then
$$
\begin{aligned}[t]
zw &= (\lambda_0+\ldots+\lambda_kt^k+z_{k+1}t^{k+1})(\sigma_0+\ldots+\sigma_kt^k+w_{k+1}t^{k+1})\\
	&= \lambda_0\sigma_0+\left(\sum_{j+l=1}\lambda_j\sigma_l\right)t+\ldots+\left(\sum_{j+l=k}\lambda_j\sigma_l\right)t^k+(\ldots)t^{k+1}.
\end{aligned}
$$
By the uniqueness of coefficients in Problem 2.30, this shows that if
$zw=\gamma_0+\ldots+\gamma_kt^k+h_{k+1}t^{k+1},$ with $\gamma_0,\ldots,\gamma_k\in k,$
$h_{k+1}\in R,$ then $\gamma_i=\sum_{j+l=i}\lambda_j\sigma_l.$ So
$\varphi(zw)=\varphi(z)\varphi(w).$
</div>

\(b\) Show that the homomorphism extends to a homomorphism of $K$ into $k((X)),$
and that the order function on $k((X))$ restricts to that on $K.$

<div class="proof">
Let $z\in K\setminus R.$ Since $z^{-1}\in R,$ we naturally extend $\phi$ by
defining $\phi(z)=\phi(z^{-1})^{-1}.$ Since $\phi$ is injective on $R,$
this extension is a well defined injective homomorphism
$K\hookrightarrow k((X)).$

It suffices to show $\text{ord}_R(z)=\text{ord}_{k[[X]]}(\phi(z))$ for all $z\in R.$ But
this is clealy the case since $\text{ord}_R(z)=k,$ $\text{ord}_{k[[X]]}(\phi(z))=k,$ where
$k$ is the least integer such that $\lambda_k\ne 0.$
</div>

\(c\) Let $a=0$ in Problem 2.24, $t=X.$ Find the power series expansion of $z=(1-X)^{-1}$
and of $(1-X)(1+X^2)^{-1}$ in terms of $t.$

Note that $(1-X), (1-X)(1+X^)\not\in\mathfrak{m}_{0}=(X),$ so they are invertible. By
our explicit construction of the power series inverses in Problem 2.31,
we find
$$
	(1-X)^{-1}=\sum X^i, \ (1+X^2)^{-1}=\sum(-1)^iX^{2i}.
$$
Therefore
$$
	(1-X)(1+X^2)^{-1}=\sum(-1)^iX^{2i}-\sum(-1)^iX^{2i+1}
		=\sum (-1)^{\lceil i/2\rceil}X^i.
$$
