
## ----------------------------------------------------------------------------
## Name: 02-point-estimation.R
## Description: Script for Chapter 2 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.0.0
## ----------------------------------------------------------------------------

## ---- cons-norm, echo = FALSE, fig.cap = '(ref:cons-norm-title)'---------------
n <- 1:50
eps <- c(1.00, 0.5, 0.25, 0.10)
sigma <- 2
col <- rainbow(length(eps))
plot(n, 2 * pnorm(eps[1] / (sigma / sqrt(n)), lower.tail = FALSE), type = "o",
     pch = 16, col = col[1], xlab = latex2exp::TeX("Sample size $n$"),
     ylab = latex2exp::TeX("$P\\left(|\\bar{X}_n-\\mu | \\geq\\epsilon\\right)$"),
     main = latex2exp::TeX("Probabilites as a function of $n$ and $\\epsilon$"),
     ylim = c(0, 1.1), mgp = c(2.5, 1, 0))
for (i in 1:(length(eps) - 1)) {
  lines(n, 2 * pnorm(eps[i + 1] / (sigma / sqrt(n)), lower.tail = FALSE),
        type = "o", pch = 16, col = col[i + 1])
}
legend("topright", legend = latex2exp::TeX(paste0("$\\epsilon = ", eps, "$")),
       col = col, lwd = 2)

