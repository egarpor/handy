
## ----------------------------------------------------------------------------
## Name: 04-confidence-intervals.R
## Description: Script for Chapter 4 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.0.0
## ----------------------------------------------------------------------------

## ---- ci-mu-1---------------------------------------------------
# Sample
X <- c(3005, 2925, 2935, 2965, 2995, 3005, 2937, 2905)

# n, mean, sigma, alpha, z_{\alpha/2}
n <- length(X)
X_bar <- mean(X)
sigma <- 39
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)

# CI
X_bar + c(-1, 1) * z * sigma / sqrt(n)

## ---- ci-mu-2---------------------------------------------------
# Sample
X <- c(3005, 2925, 2935, 2965, 2995, 3005, 2937, 2905)

# n, mean, S', alpha, z_{\alpha/2}
n <- length(X)
X_bar <- mean(X)
S_prime <- sd(X)
alpha <- 0.05
t <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)

# CI
X_bar + c(-1, 1) * t * S_prime / sqrt(n)

## ---- ci-sigma2-------------------------------------------------
# Sample
X <- c(4.1, 5.2, 10.2)

# n, S'^2, alpha, c1, c2
n <- length(X)
S2_prime <- var(X)
alpha <- 0.10
c1 <- qchisq(1 - alpha / 2, df = n - 1, lower.tail = FALSE)
c2 <- qchisq(alpha / 2, df = n - 1, lower.tail = FALSE)

# CI
(n - 1) * S2_prime / c(c2, c1)

## ---- ci-diff-mu-1----------------------------------------------
# Samples
X_1 <- c(32, 37, 35, 28, 41, 44, 35, 31, 34)
X_2 <- c(35, 31, 29, 25, 34, 40, 27, 32, 31)

# n1, n2, Xbar1, Xbar2, sigma2_1, sigma2_2, alpha, z_{alpha/2}
n_1 <- length(X_1)
n_2 <- length(X_2)
X_bar_1 <- mean(X_1)
X_bar_2 <- mean(X_2)
sigma2_1 <- sigma2_2 <- 22
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)

# CI
(X_bar_1 - X_bar_2) + c(-1, 1) * z * sqrt(sigma2_1 / n_1 + sigma2_2 / n_2)

## ---- ci-diff-mu-2----------------------------------------------
# Samples
X_1 <- c(32, 37, 35, 28, 41, 44, 35, 31, 34)
X_2 <- c(35, 31, 29, 25, 34, 40, 27, 32, 31)

# n1, n2, Xbar1, Xbar2, S^2, alpha, z_{alpha/2}
n_1 <- length(X_1)
n_2 <- length(X_2)
X_bar_1 <- mean(X_1)
X_bar_2 <- mean(X_2)
S2_prime_1 <- var(X_1)
S2_prime_2 <- var(X_2)
S <- sqrt(((n_1 - 1) * S2_prime_1 + (n_2 - 1) * S2_prime_2) / (n_1 + n_2 - 2))
alpha <- 0.05
t <- qt(alpha / 2, df = n_1 + n_2 - 2, lower.tail = FALSE)

# CI
(X_bar_1 - X_bar_2) + c(-1, 1) * t * S * sqrt(1 / n_1 + 1 / n_2)

## ---- ratio-vars------------------------------------------------
# n1, n2, S1'^2, S2'^2, alpha, c1, c2
n_1 <- 11
n_2 <- 14
S2_prime_1 <- 52
S2_prime_2 <- 71
alpha <- 0.05
c1 <- qf(1 - alpha / 2, df1 = n_1 - 1, df2 = n_2 - 1, lower.tail = FALSE)
c2 <- qf(alpha / 2, df1 = n_1 - 1, df2 = n_2 - 1, lower.tail = FALSE)

# CI
(S2_prime_1 / S2_prime_2) / c(c2, c1)

