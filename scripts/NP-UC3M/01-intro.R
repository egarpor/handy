
## ------------------------------------------------------------------------
## Name: 01-intro.R
## Description: Script for Chapter 1 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- ohpsfig, echo = FALSE, fig.cap = '(ref:ohpsfig-title)', fig.margin = FALSE, purl = TRUE--------------------
# Simulate sequence of random variables
set.seed(42)
n <- 1:500
Xn <- rnorm(n = length(n), mean = 0, sd = 1 / sqrt(n))
ic <- function(n, an, alpha = 0.05) {

  qnorm(1 - alpha / 2) / cbind(-sqrt(n), sqrt(n)) / an

}

# Xn = o_P(1), since 1 * Xn converges to 0 in probability
par(mfrow = c(2, 2))
ylim <- c(-1, 1) * 2
an <- 1
plot(Xn / an, type = "l", xlab = "", ylab = "", ylim = ylim,
     main = expression(list(X[n] == o[P](1), X[n] == O[P](1))))
mtext(expression(1 %.% X[n]), side = 2, line = 2.5, cex = 0.75)
mtext(expression(n), side = 1, line = 2.5, cex = 0.75)
abline(h = 0, col = "gray")
matlines(n, ic(n = n, an = an), col = 2, lty = 1)

# Xn = o_P(n^{-1/3}), since n^{1/3} * Xn converges to 0 in probability
an <- n^(-1/3)
plot(Xn / an, type = "l", xlab = "", ylab = "", ylim = ylim,
     main = expression(list(X[n] == o[P](n^{-1/3}), X[n] == O[P](n^{-1/3}))))
mtext(expression(n^{1/3} %.% X[n]), side = 2, line = 2.5, cex = 0.75)
mtext(expression(n), side = 1, line = 2.5, cex = 0.75)
abline(h = 0, col = "gray")
matlines(n, ic(n = n, an = an), col = 2, lty = 1)

# Xn != o_P(n^{-1/2}) since n^{1/2} * Xn does *not* converge to 0 in probability
# n^{1/2} * Xn converges to another variable that is not degenerate at 0!
# Xn = O_P(n^{-1/2}) and the rate n^{1/2} stabilizes the limit behavior of Xn
an <- n^(-1/2)
plot(Xn / an, type = "l", xlab = "", ylab = "", ylim = ylim,
     main = expression(list(X[n] != o[P](n^{-1/2}), X[n] == O[P](n^{-1/2}))))
mtext(expression(n^{1/2} %.% X[n]), side = 2, line = 2.5, cex = 0.75)
mtext(expression(n), side = 1, line = 2.5, cex = 0.75)
abline(h = 0, col = "gray")
matlines(n, ic(n = n, an = an), col = 2, lty = 1)

# Xn != o_P(n^{-2/3}) since n^{2/3} * Xn does *not* converge to 0 in probability
# Actually, n^{2/3} * Xn is diverging!
# Xn != O_P(n^{-2/3}) -> We do not stabilize at a random variable,
# n^{2/3} * Xn is unbounded in probability
an <- n^(-2/3)
ylim <- c(-7, 7)
plot(Xn / an, type = "l", xlab = "", ylab = "", ylim = ylim,
     main = expression(list(X[n] != o[P](n^{-2/3}), X[n] != O[P](n^{-2/3}))))
mtext(expression(n^{2/3} %.% X[n]), side = 2, line = 2.5, cex = 0.75)
mtext(expression(n), side = 1, line = 2.5, cex = 0.75)
abline(h = 0, col = "gray")
matlines(n, ic(n = n, an = an), col = 2, lty = 1)

