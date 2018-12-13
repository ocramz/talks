% Topic Models
% Marco Zocca
% December 13, 2018



# Outline

An overview of three landmark methods from the topic modeling literature

- LSI
- pLSI
- LDA



# Latent Semantic Indexing (LSI)


\begin{table}[h]
\begin{tabular}{ r|c|c|c }
\multicolumn{1}{r}{}
 &  \multicolumn{1}{c}{Word1} & \multicolumn{1}{c}{Word2} & $\cdots$ \\
\cline{2-4}
Document1 & 3 & 1 & $\cdots$ \\
\cline{2-4}
Document2 & 0 & 2 & $\cdots$ \\
\cline{2-4}
Document3 & 1 & 0 & $\cdots$ \\
\cline{2-4}
$\cdots$ & $\cdots$ & $\cdots$ & $\cdots$ \\
\end{tabular}
\caption{Word counts for each document}
\end{table}

LSI : Identify a subspace that captures most of the variance in the document collection

Rank-$r$ SVD factorization of the count data : $A \approx A' \triangleq U^\top \Sigma V$ where $\Sigma \triangleq diag(\sigma_1, \cdots \sigma_r)$

Document similarity : project test document onto column of $U$ that corresponds to largest singular value.



# Probabilistic LSI (pLSI)

Introduce _topics_ $z_k$

Word-document _joint distribution_ $$p(d, w) \triangleq \sum\limits_k p(d|z_k) p(z_k) p(w|z_k)$$

Inference by iterative approximation of the factors (such as EM).

Caution : both in LSI and pLSI the number of parameters is linear in the size of the corpus. 





# Latent Dirichlet Allocation (LDA)

One level further up : documents represented as mixtures of topics

Generative model:

* For each latent topic $k \in \{1 \cdots K \}$
    * Sample word mixture $\phi_k \sim Dirichlet(\beta)$

* For each document $d \in D$ :
    * Sample document topic mixture $\theta_d \sim Dirichlet(\alpha)$
    * For each word $i \in d$ :
        * Sample topic $z_i \sim Discrete(\theta_d)$
        * Sample word $w_i \sim Discrete(\phi_{z_i})$

Hp:

- Topic dimensionality $K$ fixed
- $\beta \in \mathbb{R}_+$ and $\alpha \in \mathbb{R}_+$ hyperparameters

The one above is a version based on the _symmetric_ Dirichlet distribution, from (Darling 2011)


# Dirichlet distribution

![Dirichlet PDF (K = 3)](img/dirichlet.png){ width=250px }

$\bar{x} \sim Dirichlet(K, \bar{\alpha})$

$p(\bar{x} | K, \bar{\alpha}) = p(x_1, \cdots x_K | \alpha_1, \cdots \alpha_K) \triangleq \frac{1}{B(\bar{\alpha})} \prod\limits_{i=1}^K x_i^{\alpha_i - 1}$

$x_i \in (0, 1), \sum\limits_i x_i = 1$



# LDA

Joint p.d.f. : $p(\theta, \phi, z, w|\alpha, \beta) \triangleq p(\theta|\alpha) p(\phi|\beta) \prod\limits_n p(z_n|\theta) p(w_n|\phi)$

Posterior p.d.f. by rewriting the above via Bayes' Theorem :

$$p(\theta, \phi, z|w, \alpha, \beta) = \frac{p(\theta, \phi, z, w | \alpha, \beta)}{p(w|\alpha, \beta)}$$  

where 

$p(w|\alpha, \beta)$ is obtained by marginalizing the JPDF over $z$, $\phi$ and $\theta$:

$$p(w|\alpha, \beta) \triangleq \iint p(\theta|\alpha) p(\phi|\beta) \left( \prod\limits_n \sum\limits_{z_n} p(z_n|\theta)p(w_n|\phi)\right) d\phi d\theta $$  \qquad \tiny{GNARLY}


# LDA

Inference:

- Original approach (Blei 2003) uses a variational formulation
    - minimize relative entropy between approximate variational family and true posterior

- MCMC based: collapsed Gibbs sampling (Griffiths 2004)
    - $z$ is a sufficient statistic for $\theta$ and $\phi$
    - Refer to (Darling 2011) for full details
    - Streaming and sparsity-based optimizations in later literature



# References

- T. Hofmann, Probabilistic Latent Semantic Analysis

- D. M. Blei, A. Y. Ng, M. I. Jordan, Latent Dirichlet Allocation, JMLR 2003

- T. L. Griffiths, M. Steyvers, Finding Scientific Topics, PNAS 2004

- W. M. Darling, A theoretical and practical implementation tutorial on topic modeling and Gibbs sampling, 2011


