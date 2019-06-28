---
documentclass: article
title: Fulton 2.7
geometry: margin=1in
fontsize: 12pt
published: November 15, 2017
header-includes: \usepackage{amsmath,amsthm,amssymb,mathrsfs}\usepackage[all]{xy}
...

**2.38.\*** Show that if $k\subset R_i,$ and each $R_i$ is finite dimensional
over $k,$ then $\dim(\prod R_i)=\sum\dim R_i.$

<div class="proof">
Let $R,S$ be rings, $\{r_i\},\{s_j\}$ finite basis for $R,S$ over $k.$ Then
$\{(r_i,0)\}\cup\{(0,s_j)\}$ is clearly a linearly independent spanning set
for $R\times S.$ Thus $\dim(R\times S)=\dim R+\dim S.$ The result then follows
by induction.
</div>

**2.39.\*** Prove the following relations among ideals $I_i, J$ in a ring
$R$:

\(a\) $(I_1+I_2)J=I_1J+I_2J.$

<div class="proof">
Suppose $x=(i_1+i_2)j\in (I_1+I_2)J.$ Then $x=i_1j+i_2j\in I_1J+I_2J.$ Suppose
$x=i_1j_1+i_2j_2\in I_1J+I_2J.$ Then $i_1j_1,i_2j_2\in (I_1+I_2)J$ and thus
$x\in (I_1+I_2)J.$ So $(I_1+I_2)J=I_1J+I_2J.$
</div>

\(b\) $(I_1\ldots I_N)^n=I_1^n\ldots I_N^n.$

<div class="proof">
Observe that $(a_1\ldots a_N)^n=a_1^n\ldots a_N^n$ where $a_k\in I_k.$
Thus $(I_1\ldots I_N)^n=I_1^n\ldots I_N^n.$
</div>
