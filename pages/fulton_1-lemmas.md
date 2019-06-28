---
documentclass: article
title: Fulton 1 - Lemmas
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**Lemma 1.**
Suppose $R,S,T$ are groups (or rings, modules) and $\pi:R\to S$, $\varphi:R\to T$
are group (or ring, module) homomorphisms such that $\ker\pi\subset\ker\varphi$.
Then there exists an
induced homomorphism $\widetilde{\varphi}:\pi(R)\to T$ such that
$\widetilde{\varphi}\circ\pi=\varphi$. Moreover, $\ker\widetilde{\varphi}=\pi(\ker\varphi)$.

<div class="proof">
Suppose $a,b\in R$ such that $\pi(a)=\pi(b)$. Then $a-b\in\ker\pi\subset\ker\varphi$.
So $a+\ker\varphi=b+\ker\varphi$ and $\varphi(a)=\varphi(b)$. Thus the map
$\widetilde{\varphi}:\pi(R)\to T$ defined by $\pi(a)\mapsto\varphi(a)$ is well defined
homomorphism.
</div>
