---
documentclass: article
title: Fulton 2.9
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.47.\*** Suppose $R$ is a ring containing $k,$ and $R$ is finite dimensional
over $k.$ Show that $R$ is isomorphic to a direct product of local rings.

<div class="proof">
Let $\{v_1,\ldots,v_n\}$ be a basis for $R$ over $k.$ Let
$\varphi:k[X_1,\ldots,X_n]\to R$ be the natural map sending $X_i\mapsto v_i.$
Then $\varphi$ is a surjective ring homomorphism (and thus also a surjective
vector space homomorphism over $k$). Let $I=\ker\phi.$ Note that
$\dim_k(R)=\dim_k(k[X_1,\ldots,X_n]/I).$ By [Lemma
2.14](/pages/fulton_2-lemmas.html)
$V(I)$ is finite. Then by Proposition 6,
$$
    R\cong k[X_1,\ldots,X_n]/I\cong \prod_{i=1}^N\mathcal{O}_i/I\mathcal{O}_i.
$$
</div>
