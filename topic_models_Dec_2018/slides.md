% Topic Models
% Marco Zocca
% December 13, 2018



# Outline

- LSI
- pLSI
- LDA



# Latent Semantic Indexing (LSI)


\begin{table}[h]
\begin{tabular}{ r|c|c|c }
\multicolumn{1}{r}{}
 &  \multicolumn{1}{c}{Word1} & \multicolumn{1}{c}{Word2} & $\cdots$ \\
\cline{2-4}
Document1 & 0 & 2 & $\cdots$ \\
\cline{2-4}
Document2 & 1 & 0 & $\cdots$ \\
\cline{2-4}
$\cdots$ & $\cdots$ & $\cdots$ & $\cdots$ \\
\end{tabular}
\caption{Word counts for each document}
\end{table}

LSI : Identify a subspace that captures most of the variance in the document collection

Rank $r$ SVD factorization of the count data : $A \approx A' \triangleq U \Sigma V^\top$ where $\Sigma \triangleq diag(\sigma_1, \cdots \sigma_r)$

Document similarity : project test document onto column of $V^\top$ that corresponds to largest singular value



# Probabilistic LSI (pLSI)

Introduce _topics_ $z_k$

Each word is sampled from a mixture model $p(w) \triangleq \sum\limits_k p(w|z_k) p(z_k)$

Word-document _joint distribution_ $p(w, d) \triangleq \sum\limits_k p(w|z_k) p(d|z_k) p(z_k)$

Overparametrized $\rightarrow$ inference by iterative approximation (such as EM).

Caution : number of parameters is linear in the size of the corpus. 





# Latent Dirichlet Allocation (LDA)

One level further up : documents represented as mixtures of topics

- Sample $\theta \sim Dirichlet(\alpha)$
- For each word $\{w_i \cdots w_N \}$ :
    - Sample $z_n \sim Multinomial(\theta)$
    - Sample word $w_n \sim p(w_n|z_n, \beta)$, multinomial conditioned on topic $z_n$


# Dirichlet process

![Dirichlet PDF (K = 3)](img/dirichlet.png){ width=250px }

$\bar{\theta} \sim Dirichlet(\bar{\alpha})$

$p(\bar{\theta} | \bar{\alpha}) = p(\theta_1, \cdots \theta_K | \alpha_1, \cdots \alpha_K) = \frac{1}{B(\bar{\alpha})} \prod\limits_{i=1}^K \theta_i^{\alpha_i - 1}$

$\theta_i \in (0, 1), \sum\limits_i \theta_i = 1$





# References

T. Hofmann, Probabilistic Latent Semantic Analysis

D. M. Blei, A. Y. Ng, M. I. Jordan, Latent Dirichlet Allocation, JMLR 2003


