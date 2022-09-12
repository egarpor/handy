
## ----------------------------------------------------------------------------
## Name: 01-intro-inference.R
## Description: Script for Chapter 1 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.0.0
## ----------------------------------------------------------------------------

## ---- normal-prob--------------------------------------------------------------
# Computation of P(Z > k)
k <- 0.9
1 - pnorm(k) # 1 - P(Z <= k)
pnorm(k, lower.tail = FALSE) # Alternatively

## ---- normal-prob-2------------------------------------------------------------
alpha <- 0.05
qnorm(1 - alpha / 2) # LOWER (1 - beta)-quantile = UPPER beta-quantile
qnorm(alpha / 2, lower.tail = FALSE) # Alternatively, lower.tail = FALSE
# computes the upper quantile and lower.tail = TRUE (the default) computes the
# lower quantile

## ---- chi2, echo = FALSE, fig.cap = '(ref:chi2-title)'-------------------------
nu <- c(3, 5, 7, 10)
x <- seq(0, max(nu) * 1.5, l = 500)
col <- rainbow(length(nu) + 2)
plot(x, dchisq(x = x, df = 1), ylab = "Density", xlab = latex2exp::TeX("$x$"),
     type = "l", col = col[1], ylim = c(0, 1), axes = FALSE,
     main = latex2exp::TeX("$\\chi^2_{\\nu}$ densities"))
box(); axis(1, at = 0:15); axis(2)
lines(x, dchisq(x = x, df = 2), col = col[2])
for (i in seq_along(nu)) {
  lines(x, dchisq(x = x, df = nu[i]), col = col[i + 2])
  segments(x0 = nu[i] - 2, y0 = 0, x1 = nu[i] - 2,
           y1 = dchisq(x = nu[i] - 2, df = nu[i]), lty = 3, col = col[i + 2])
}
legend("topright", legend = latex2exp::TeX(paste0("$\\nu = ", c(1:2, nu), "$")),
       lwd = 2, col = col)

## ---- chi-prob-1---------------------------------------------------------------
alpha <- 0.05
qchisq(1 - alpha, df = 6) # df stands for the degrees of freedom
qchisq(alpha, df = 6, lower.tail = FALSE) # Alternatively

## ---- chi-prob-2---------------------------------------------------------------
alpha <- 0.10
qchisq(1 - alpha / 2, df = 9, lower.tail = FALSE) # a1
qchisq(alpha / 2, df = 9, lower.tail = FALSE) # a2

## ---- t, echo = FALSE, fig.cap = '(ref:t-title)'-------------------------------
nu <- c(1, 2, 3, 5, 10, 20, 30, 50)
x <- seq(-5, 5, l = 500)
col <- rainbow(length(nu))
plot(x, dnorm(x = x), ylab = "Density", xlab = latex2exp::TeX("$x$"),
     type = "l", col = 1, ylim = c(0, 0.5), lwd = 2,
     main = latex2exp::TeX("$t_{\\nu}$ densities"))
for (i in seq_along(nu)) {
  lines(x, dt(x = x, df = nu[i]), col = col[i])
}
legend("topright", legend = latex2exp::TeX(c(paste0("$\\nu = ", nu, "$"),
                                             "$N(0,1)$")),
       lwd = 2, col = c(col, 1))

## ---- t-prob-------------------------------------------------------------------
pt(-2, df = 5)

## ---- F, echo = FALSE, fig.cap = '(ref:F-title)'-------------------------------
nu <- c(1, 3)
nu <- as.matrix(expand.grid(nu, nu))
nu <- rbind(nu, c(5, 5))
x <- seq(0, 5, l = 500)
col <- rainbow(nrow(nu))
plot(x, df(x = x, df1 = nu[1, 1], df2 = nu[1, 2]), ylab = "Density",
     xlab = latex2exp::TeX("$x$"), type = "l", col = col[1], ylim = c(0, 1),
     main = latex2exp::TeX("$F_{n_1,n_2}$ densities"))
for (i in 1:(nrow(nu) - 1)) {
  lines(x, df(x = x, df1 = nu[i + 1, 1], df2 = nu[i + 1, 2]), col = col[i + 1])
}
legend("topright", legend = latex2exp::TeX(paste0("$n_1 = ", nu[, 1],
                                                  ",$ $n_2 = ", nu[, 2], "$")),
       lwd = 2, col = col)

## ---- F-prob-------------------------------------------------------------------
qf(0.05, df1 = 5, df2 = 9, lower.tail = FALSE)

## ---- prob-clt-----------------------------------------------------------------
pnorm(-2.5)
pnorm(58, mean = 60, sd = sqrt(64/100)) # Alternatively

## ---- prob-clt-2---------------------------------------------------------------
pnorm(-3)

## ---- prob-clt-7---------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6))

## ---- prob-clt-8---------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6)) - pnorm(7.5, mean = 10, sd = sqrt(6))

## ---- prob-clt-4---------------------------------------------------------------
1 - pbinom(54, size = 100, prob = 0.5)
pbinom(54, size = 100, prob = 0.5, lower.tail = FALSE) # Alternatively

## ---- prob-clt-5---------------------------------------------------------------
1 - pnorm(0.55, mean = 0.5, sd = sqrt(0.25 / 100))

## ---- prob-clt-6---------------------------------------------------------------
1 - pnorm(54.5, mean = 50, sd = sqrt(25))

