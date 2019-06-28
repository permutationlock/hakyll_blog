---
documentclass: article
title: Fulton 1.10
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**Proposition 4.** If a field $L$ is a ring finite over a subfield $K,$
then $L$ is module finite, and hence algebraic over $K.$

We provide a detailed proof below to clarify Fulton's proof.

<div class="proof">
Suppose a field $L$ is ring finite over a subfield $K,$ that is, $L=K[v_1,\ldots,v_n]$
for $v_1,\ldots,v_n\in L.$ We proceed by induction on $n,$ the ring dimension of
$L$ over $K.$

Suppose $n=1,$ that is $L=K[v].$ Let us define $\varphi:K[X]\to K[v]$ by
$X\mapsto v.$  Since $K[X]$ is a PID, $\ker\varphi=(F)$ for some $F\in K[X].$ Since
$K[X]/(F)=L$ is a field, $(F)$ is maximal. if $F=0,$ $K[X]\cong K[v]=L$ and
therefore $K(X)\cong K[X],$ a contradiction by Problem 1.44. Thus $F\ne 0$ and
$F(v)=0.$ So $v$ is algebraic over $K$ and $L=K[v]$ is module finite over $K$
by Proposition 3.

Suppose the proposition holds for all $m$ such that $1\le m <n.$ Observe that
$$
    L=K[v_1,\ldots,v_{n+1}]= K(v_{n+1})[v_1,\ldots,v_n].
$$
Thus by the inductive hypothesis, $L$ is module finite over $K(v_{n+1}).$ It
suffices to show $v_{n+1}$ is algebraic over $K.$

Observe that for
each $i\in\{1,\ldots,n\}$ there exists
$F_i\in K(v_{n+1})[X],$ with $F_i(X)=X^{m_i}+a_{i,m_i-1}X^{m_i-1}+\ldots+a_{i,0},$ such that
$F_i(v_i)=0.$ Observe that there exists $a\in K[v_{n+1}]$ such that $aa_{i,j}\in K[v_{n+1}]$
for all $i,j.$ For each $i$ note that
$$
    0=a^{m_i}F_i(v_i)
        =(av_i)^{m_i}+aa_{i,m_i-1}(av_i)^{m_i-1}+\ldots+a^{m_i}a_{i,0}=G(av_i).
$$
By our selection of $a,$ observe that $G\in K[v_{n+1}][X].$ So $av_i$ is integral
over $K[v_{n+1}]$ for each $i.$ Therefore $K[av_1,\ldots,av_n,v_{n+1}]$ is integral
over $K[v_{n+1}].$ Moreover, for each $z\in L=K[v_1,\ldots,v_{n+1}]$ there exists
$m>0$ such that $a^mz\in K[av_1,\ldots,av_n,v_{n+1}].$ In particular this is
true for $K(v_{n+1})\subset L.$

Suppose $v_{n+1}$ is transcendental over $K.$ Then $K(v_{n+1})\cong K(X).$ But
the existence of $a$ is a contradiction to Problem 1.49(b). Thus $v_{n+1}$ is
algebraic over $K$ and $L$ is module finite over $K.$
</div>

**1.51.\*** Let $K$ be a field, $F\in K[X]$ an irreducible polynomial of
degree $n>0.$

\(a\) Show that $L=K[X]/(F)$ is a field, and if $x$ is residue of $X$ in $L,$ then
$F(x)=0.$

<div class="proof">
Since $K[X]$ is a PID, $(F)$ is maximal and $K[X]/(F)$ is a field. Let
$\varphi$ be the homomorphism $K[X]\to K[X]/(F).$ Observe that
$F(x)=F(\varphi(X))=\varphi(F(X))=0.$
</div>

\(b\) Suppose $L'$ is a field extension of $K,$ $y\in L'$ such that $F(y)=0.$
Show the homomorphism $\varphi:K[X]\to L'$ defined by $X\mapsto y$ induces an
isomorphism $L\to K(y).$

<div class="proof">
Since $F(y)=0,$ $(F)\subset\ker\varphi.$ Thus by [Lemma 1.1](/pages/fulton_1-lemmas.html)
there exists an induced
homomorphism $\widetilde{\varphi}:L\to K(y).$ Now suppose $\ker\varphi\ne (F).$
Then $\ker\varphi=K[X]$ since $(F)$ is maximal, a contradiction. Thus
$\ker\varphi=(F)$ and thus $\widetilde{\varphi}$ is an injective homomorphism. Moroever,
$\widetilde{\varphi}(L)=\varphi(K[X])$ is a subfield of $K(y)$ containing $K$ and $y.$
So $\widetilde{\varphi}(L)=K(y)$ and $\widetilde{\varphi}$ is an isomorphism.
</div>

\(c\) With $L',$ $y$ as in (b), suppose $G\in K[X]$ and $G(y)=0.$ Show that $F$
divides $G.$

<div class="proof">
Observe that $G\in\ker\varphi=(F).$ So $F\mid G.$
</div>

\(d\) Show that $F=(X-x)F_1, F_1\in L[X].$

<div class="proof">
Observe that $X-x$ is irreducible in $L[X]$ and $F(x)=0.$ By (c), $X-x\mid F.$
</div>

**1.52.** Let $K$ be a field and $F\in K[X].$ Show there is a field $L$
containing $K$ such that $F=\prod_{i=1}^n(X-x_i)\in L[X].$

<div class="proof">
Assume $F$ is monic.
Let us proceed by induction on $n,$ the degree of $F.$

If $n=1$ then $F=X-x,$ $x\in K.$

Suppose the statement is true for all monic polynomials in $K[X]$ of degree
less than $n.$ If $F$ is reducible, the statement follows by the inductive
hyptothesis. Suppose $F$ is irreducible. Then $L=K[X]/(F)$ is a field. Let
$x$ be the image of $X$ in $L.$ Then $F(x)=0$ in $L,$ so $F=G(X-x)$ for some
$G\in L[X]$ of degree less than $n.$ Thus by the inductive hypothesis, there
exists an extension field $M$ of $L$ such that the statemenet holds. But since
$L$ is an extension field of $K,$ $M$ is also an extension field of $K.$
</div>

**1.53.\*** Suppose $K$ is a field of characteristic zero, and $F$ is an
irreducible polynomial in $K[X]$ of degree $n>0.$ Let $L$ be a splitting field
of $F,$ so $F=\prod_{i=1}^n(X-x_i),$ $x_i\in L.$ Show the $x_i$ are distinct.

<div class="proof">
Suppose some of the $x_i$'s are equal, that is, there exists $x\in L$ such that
$(X-x)^m\mid F$ for some $m>1.$ Then $F=(X-x)^mG$ for some $G\in L[X].$ Observe
$F_X=m(X-x)^{m-1}G+(X-x)^mG_X.$ Since $K$ is of characteristic zero, $F_X\ne 0.$
Observe $F_X(X)=0.$ By Problem 1.51(c), this implies $F\mid F_X,$ a contradiction
since $0<\deg F_X<\deg F.$
</div>

**1.54.\*** Let $R$ be a domain with quotient field $K,$ and let $L$ be a
finite algebraic extension of $K.$

\(a\) For any $v\in L$ show there is a nonzero $a\in R$ such that $av$ is integral
over $R.$

<div class="proof">
Let $v\in L.$ Then $v^n+a_{n-1}v^{n-1}+\ldots+a_0=0,$ for $a_i\in K.$ Let $a$ be
the common denominator of $a_0,\ldots,a_{n-1}.$ Then $(av)^n+aa_{n-1}(av)^{n-1}+
\ldots+a^na_0=0.$ Thus $av$ is integral over $R.$
</div>

\(b\) Show that there is a basis $v_1,\ldots,v_n$ or $L$ over $K$ such that each
$v_i$ is integral over $R.$

<div class="proof">
Let $w_1,\ldots,w_n$ be a basis for $L$ over $K.$ By \(a\) there exist $a_1,\ldots,
a_n$ such that $a_1w_1,\ldots,a_nw_n$ are all integral over $R.$ Let $v\in L.$
Then $v=\sum_{i=1}^nb_iw_i,$ for $b_i\in K.$ So $v=\sum_{i=1}^nb_ia_i^{-1}(a_iw_i).$
So $a_1w_1,\ldots,a_nw_n$ spans $L$ and hence is a basis for $L$ over $K.$
</div>
