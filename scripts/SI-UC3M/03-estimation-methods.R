
## ----------------------------------------------------------------------------
## Name: 03-estimation-methods.R
## Description: Script for Chapter 3 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.0.0
## ----------------------------------------------------------------------------

## ---- mle-unif, echo = FALSE, fig.cap = '(ref:mle-unif-title)'-----------------
theta <- 1
th <- seq(0, 2, l = 1e3)
set.seed(987202226)
n <- 3
samp_1 <- runif(n, 0, theta)
samp_2 <- runif(n, 0, theta)
plot(th, 1/th^n * (max(samp_1) <= th), type = "l", col = 2,
     xlab = latex2exp::TeX("$\\theta$"),
     ylab = latex2exp::TeX("$L(\\theta;x_1,\\ldots,x_n)$"),
     main = "Likelihood function", xlim = c(0, 2), ylim = c(0, 5))
rug(samp_1, col = 2)
abline(v = max(samp_1), col = 2, lty = 2)
lines(th, 1/th^n * (max(samp_2) <= th), type = "l", col = 4)
rug(samp_2, col = 4)
abline(v = max(samp_2), col = 4, lty = 2)
abline(v = 1)
legend("topleft", legend = c("Sample 1", "Sample 2"), col = c(2, 4), lwd = 2)

