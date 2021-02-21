
## ------------------------------------------------------------------------
## Name: 01-intro.R
## Description: Script for Chapter 1 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- ohsfig, echo = FALSE, fig.cap = '(ref:ohsfig-title)', fig.margin = FALSE, purl = TRUE, fig.asp = 3/2, fig.pos = 't!'----
# Sequence bn
n <- 1:200
bn <- 1 / log(n)

# Sequences an that decrease faster than bn
a1n <- 2 / n + 50 / n^2
a2n <- (sin(0.2 * n) + 2) / n^1.25
a3n <- 3 * (1 + 5 * exp(-(n - 55.5)^2 / 200)) / n

# Sequences an that decrease at the same pace as bn
a4n <- 0.5 / log10(n) * ((n + 3) / (2 * n)) + 0.5 * a3n
a5n <- 0.25 / log2(0.5 * n)
a6n <- 1 / log(n^2 + n)

# Sequences an that decrease *slower* than bn
a7n <- 0.5 * log(5 * n + 3)^(-0.25)
a8n <- 0.25 / log(log(10 * n + 2))
a9n <- 0.5 / log(log(n^2 + 10 * n + 2))

# Plot function
bigO_plot <- function(An, ylim1 = c(0, 1), ylim2 = c(0, 3.5), kind = 1) {

  C <- c(seq(1, 0.25, by = -0.25), seq(1.25, 10, by = 0.25))
  lty <- c(1, rep(3, length(C) - 1))
  lwd <- c(2, rep(1, length(C) - 1))
  main1 <- switch(as.character(kind),
    "1" = expression(list(a[list(i, n)] == o(b[n]),
                          a[list(i, n)] == O(b[n]))),
    "2" = expression(list(a[list(i, n)] != o(b[n]),
                          a[list(i, n)] == O(b[n]))),
    "3" = expression(list(a[list(i, n)] != o(b[n]),
                          a[list(i, n)] != O(b[n]))),
  )
  main2 <- switch(as.character(kind),
    "1" = expression(list(a[list(i, n)] / b[n] == o(1),
                          a[list(i, n)] / b[n] == O(1))),
    "2" = expression(list(a[list(i, n)] / b[n] != o(1),
                          a[list(i, n)] / b[n] == O(1))),
    "3" = expression(list(a[list(i, n)] / b[n] != o(1),
                          a[list(i, n)] / b[n] != O(1))),
  )
  matplot(n, An, type = "l", col = 2:5, ylim = ylim1, lty = 1, lwd = 2,
          xlab = expression(n),
          ylab = expression(a[list(i, n)] * " vs. " * b[n]), main = main1)
  matlines(n, bn %o% C, col = 1, lwd = lwd, lty = lty)
  abline(h = 0, col = "gray")
  leg <- c(expression(b[n], C * b[n]),
           substitute(a[list(i, n)], list(i = 3 * (kind - 1) + 1)),
           substitute(a[list(i, n)], list(i = 3 * (kind - 1) + 2)),
           substitute(a[list(i, n)], list(i = 3 * (kind - 1) + 3)))
  legend("top", legend = leg, lwd = c(2, 1, rep(2, 3)),
         lty = c(1, 3, rep(1, 3)), col = c(1, 1, 2:4), horiz = TRUE,
         x.intersp = 0.5, cex = 0.9, adj = c(0, 0.35), bg = "white")
  matplot(n, An / bn, type = "l", col = 2:5, ylim = ylim2, lty = 1, lwd = 2,
          xlab = expression(n), ylab = expression(a[list(i, n)] / b[n]),
          main = main2)
  matlines(n, rep(1, length(n)) %o% C, col = 1, lty = lty, lwd = lwd)
  leg <- c(expression(1, C),
           substitute(frac(a[list(i, n)], b[n]), list(i = 3 * (kind - 1) + 1)),
           substitute(frac(a[list(i, n)], b[n]), list(i = 3 * (kind - 1) + 2)),
           substitute(frac(a[list(i, n)], b[n]), list(i = 3 * (kind - 1) + 3)))
  abline(h = 0, col = "gray")
  legend("top", legend = leg, lwd = c(2, 1, rep(2, 3)),
         lty = c(1, 3, rep(1, 3)), col = c(1, 1, 2:4), horiz = TRUE,
         x.intersp = 0.5, cex = 0.9, adj = c(0, 0.35), bg = "white")

}

# an = o(bn), an = O(bn)
par(mfrow = c(3, 2))
An <- cbind(a1n, a2n, a3n)
bigO_plot(An, ylim1 = c(0, 0.5), ylim2 = c(0, 2), kind = 1)

# an != o(bn), an = O(bn)
An <- cbind(a4n, a5n, a6n)
bigO_plot(An, ylim1 = c(0, 0.5), ylim2 = c(0, 2), kind = 2)

# an != o(bn), an != O(bn)
An <- cbind(a7n, a8n, a9n)
bigO_plot(An, ylim1 = c(0, 0.5), ylim2 = c(0, 2), kind = 3)
















## ---- ohpsfig, echo = FALSE, fig.cap = '(ref:ohpsfig-title)', fig.margin = FALSE, purl = TRUE----
# Simulate sequence of random variables
set.seed(42)
n <- 1:500
Xn <- rnorm(n = length(n), mean = 0, sd = 1 / sqrt(n))
ic <- function(n, an, alpha = 0.05) {

  qnorm(1 - alpha / 2) / cbind(-sqrt(n), sqrt(n)) / an

}

# Plot function
bigOP_plot <- function(an, ylim, ylab, main) {

  plot(Xn / an, type = "l", xlab = "", ylab = "", ylim = ylim,
       main = main)
  mtext(ylab, side = 2, line = 2.5, cex = 0.75)
  mtext(expression(n), side = 1, line = 2.5, cex = 0.75)
  matlines(n, ic(n = n, an = an), col = 2, lty = 1)
  abline(h = 0, col = "gray")

}

# Xn = o_P(1), since 1 * Xn converges to 0 in probability
par(mfrow = c(2, 2))
ylim <- c(-1, 1) * 2
an <- 1
bigOP_plot(an = an, ylim = ylim, ylab = expression(1 %.% X[n]), 
           main = expression(list(X[n] == o[P](1),
                                  X[n] == O[P](1))))

# Xn = o_P(n^{-1/3}), since n^{1/3} * Xn converges to 0 in probability
an <- n^(-1/3)
bigOP_plot(an = an, ylim = ylim, ylab = expression(n^{1/3} %.% X[n]), 
           main = expression(list(X[n] == o[P](n^{-1/3}),
                                  X[n] == O[P](n^{-1/3}))))

# Xn != o_P(n^{-1/2}) since n^{1/2} * Xn does *not* converge to 0 in probability
# n^{1/2} * Xn converges to another variable that is not degenerate at 0!
# Xn = O_P(n^{-1/2}) and the rate n^{1/2} stabilizes the limit behavior of Xn
an <- n^(-1/2)
bigOP_plot(an = an, ylim = ylim, ylab = expression(n^{1/2} %.% X[n]), 
           main = expression(list(X[n] != o[P](n^{-1/2}),
                                  X[n] == O[P](n^{-1/2}))))

# Xn != o_P(n^{-2/3}) since n^{2/3} * Xn does *not* converge to 0 in probability
# Actually, n^{2/3} * Xn is diverging!
# Xn != O_P(n^{-2/3}) -> We do not stabilize at a random variable,
# n^{2/3} * Xn is unbounded in probability
an <- n^(-2/3)
ylim <- c(-7, 7)
bigOP_plot(an = an, ylim = ylim, ylab = expression(n^{2/3} %.% X[n]), 
           main = expression(list(X[n] != o[P](n^{-2/3}),
                                  X[n] != O[P](n^{-2/3}))))

