
## ----------------------------------------------------------------------------
## Name: 05-confidence-intervals.R
## Description: Script for Chapter 5 of "A First Course on Statistical Inference"
## Link: https://egarpor.github.io/SI-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 2.4.4
## ----------------------------------------------------------------------------

## ----ci-mu-1----------------------------------------------------------------------------------------------------------------------------------
# Sample
X <- c(916, 892, 895, 904, 913, 916, 895, 885)

# n, mean, sigma, alpha, z_{\alpha/2}
n <- length(X)
X_bar <- mean(X)
sigma <- 12
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)

# CI
X_bar + c(-1, 1) * z * sigma / sqrt(n)

## ----ci-mu-2----------------------------------------------------------------------------------------------------------------------------------
# Sample
X <- c(916, 892, 895, 904, 913, 916, 895, 885)

# n, mean, S', alpha, t_{\alpha/2;n-1}
n <- length(X)
X_bar <- mean(X)
S_prime <- sd(X)
alpha <- 0.05
t <- qt(alpha / 2, df = n - 1, lower.tail = FALSE)

# CI
X_bar + c(-1, 1) * t * S_prime / sqrt(n)

## ----ci-sigma2--------------------------------------------------------------------------------------------------------------------------------
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

## ----ci-diff-mu-1-----------------------------------------------------------------------------------------------------------------------------
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

## ----ci-diff-mu-2-----------------------------------------------------------------------------------------------------------------------------
# Samples
X_1 <- c(32, 37, 35, 28, 41, 44, 35, 31, 34)
X_2 <- c(35, 31, 29, 25, 34, 40, 27, 32, 31)

# n1, n2, Xbar1, Xbar2, S^2, alpha, t_{alpha/2;n1-n2-2}
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

## ----ratio-vars-1-----------------------------------------------------------------------------------------------------------------------------
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

## ----ratio-vars-2-----------------------------------------------------------------------------------------------------------------------------
# Xbar1, Xbar2, S^2, t_{alpha/2;n1-n2-2}
X_bar_1 <- 64
X_bar_2 <- 69
S2 <- ((n_1 - 1) * S2_prime_1 + (n_2 - 1) * S2_prime_2) / (n_1 + n_2 - 2)
t <- qt(1 - alpha / 2, df = n_1 + n_2 - 2, lower.tail = TRUE)

# CI
(X_bar_1 - X_bar_2) + c(-1, 1) * t * sqrt(S2 * (1 / n_1 + 1 / n_2))

## ----aci-1------------------------------------------------------------------------------------------------------------------------------------
# Sample from Exp(2)
set.seed(123456)
n <- 100
x <- rexp(n = n, rate = 2)

# MLE
lambda_mle <- 1 / mean(x)

# Asymptotic confidence interval
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)
lambda_mle + c(-1, 1) * z * lambda_mle / sqrt(n)

## ----ci-1-------------------------------------------------------------------------------------------------------------------------------------
# Computation of percentile bootstrap confidence intervals. It requires a
# sample x and an estimator theta_hat() (must be a function!). In ... you
# can pass parameters to hist(), such as xlim.
boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.05, plot_boot = TRUE,
                    ...) {

  # Check that theta_hat is a proper function
  stopifnot(is.function(theta_hat))

  # Creates convenience statistic for boot::boot()
  stat <- function(x, indexes) theta_hat(x[indexes])

  # Perform bootstrap resampling (step 2) with the aid of boot::boot()
  boot_obj <- boot::boot(data = x, statistic = stat, sim = "ordinary", R = B)

  # Extract bootstrapped statistics (theta_hat_star's) from the boot object
  theta_hat_star <- boot_obj$t

  # Confidence intervals
  ci <- quantile(theta_hat_star, probs = c(alpha / 2, 1 - alpha / 2))

  # Plot the distribution of bootstrap estimators and the confidence intervals?
  # Draw also theta_hat, the statistic from the original sample, in boot_obj$t0
  if (plot_boot) {

    hist(theta_hat_star, probability = TRUE, main = "",
         xlab = latex2exp::TeX("$\\hat{\\theta}^*$"), ...)
    rug(theta_hat_star)
    abline(v = ci, col = 2, lwd = 2)
    abline(v = boot_obj$t0, col = 3, lwd = 2)

  }

  # Return confidence intervals
  return(ci)

}

## ----ci-2-------------------------------------------------------------------------------------------------------------------------------------
# Sample from Exp(2)
set.seed(123456)
n <- 200
x <- rexp(n = n, rate = 2)

## ----ci-3-------------------------------------------------------------------------------------------------------------------------------------
# Bootstrap confidence interval
lambda_hat_exp <- function(x) 1 / mean(x)
boot_ci(x = x, B = 5e3, theta_hat = lambda_hat_exp)

## ----ci-4-------------------------------------------------------------------------------------------------------------------------------------
# Asymptotic confidence interval
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)
lambda_mle <- 1 / mean(x)
lambda_mle + c(-1, 1) * z * lambda_mle / sqrt(n)

## ----ci-5-------------------------------------------------------------------------------------------------------------------------------------
# Sample
X <- c(916, 892, 895, 904, 913, 916, 895, 885)

# Bootstrap confidence interval
boot_ci(x = X, B = 5e3, theta_hat = mean)

