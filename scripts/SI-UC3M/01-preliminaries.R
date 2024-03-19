
## ----------------------------------------------------------------------------
## Name: 01-preliminaries.R
## Description: Script for Chapter 1 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 2.4.1
## ----------------------------------------------------------------------------

## ----toss-coin-freq-1, echo = FALSE------------------------------------------------------------------------
set.seed(123456)
n <- c(10, 20, 30, 100, 1000)
toss <- sample(c("heads", "tails"), size = max(n), replace = TRUE)
p <- cumsum(toss == "heads") / (1:max(n))
knitr::kable(x = cbind(n, p[n], 1 - p[n]),
             col.names = c("$n$", "Heads", "Tails"), booktabs = TRUE,
             caption = paste("Relative frequencies of \"heads\" and",
                             "\"tails\" for $n$ random experiments."),
             digits = 3, align = c("lcc"), escape = FALSE)

## ----car-accidents-1, echo = FALSE, cache = TRUE-----------------------------------------------------------
set.seed(123456)
lambda <- 4
n <- c(10, 20, 30, 100, 1000, 10000)
accidents <- pmin(rpois(max(n), lambda), 6)
rel_freq_accidents <- sapply(0:6,
                            function(k) cumsum(k == accidents) / (1:max(n)))
knitr::kable(x = cbind(n, rel_freq_accidents[n, ]),
             col.names = c("$n$", paste0("$", 0:5, "$"), "$\\geq 6$"),
             caption = paste("Relative frequencies of car accidents",
                             "in Spain for $n$ hours."),
             booktabs = TRUE, digits = 3, align = c("lccccccccc"),
             escape = FALSE)

## ----pedestrian-1, echo = FALSE, cache = TRUE--------------------------------------------------------------
set.seed(123456)
n <- c(10, 20, 30, 100, 1000, 5000)
weights <- rnorm(max(n), mean = 49, sd = 5)
intervals <- cut(weights, breaks = c(0, 35, 45, 55, 65, Inf))
rel_freq_intervals <- sapply(levels(intervals), function(w)
  cumsum(w == intervals) / (1:max(n)))
knitr::kable(x = cbind(n, rel_freq_intervals[n, ]),
             col.names = c("$n$", "$[0, 35)$", "$[35, 45)$", "$[45, 55)$",
                           "$[55, 65)$", "$[65, \\infty)$"),
             caption = paste("Relative frequencies of weight intervals",
                             "for $n$ measured pedestrians."),
             booktabs = TRUE, digits = 3, align = c("lccccc"),
             escape = FALSE)

## ----normal-prob-------------------------------------------------------------------------------------------
# Computation of P(Z > k)
k <- 0.96
1 - pnorm(k) # 1 - P(Z <= k)
pnorm(k, lower.tail = FALSE) # Alternatively

## ----normal-prob-2-----------------------------------------------------------------------------------------
alpha <- 0.05
qnorm(1 - alpha / 2) # LOWER (1 - beta)-quantile = UPPER beta-quantile
qnorm(alpha / 2, lower.tail = FALSE) # Alternatively, lower.tail = FALSE
# computes the upper quantile and lower.tail = TRUE (the default) computes the
# lower quantile

## ----chi-prob-1--------------------------------------------------------------------------------------------
alpha <- 0.05
qchisq(1 - alpha, df = 6) # df stands for the degrees of freedom
qchisq(alpha, df = 6, lower.tail = FALSE) # Alternatively

## ----chi-prob-2--------------------------------------------------------------------------------------------
alpha <- 0.10
qchisq(1 - alpha / 2, df = 9, lower.tail = FALSE) # a1
qchisq(alpha / 2, df = 9, lower.tail = FALSE) # a2

## ----t-prob------------------------------------------------------------------------------------------------
pt(-2, df = 5)

## ----F-prob------------------------------------------------------------------------------------------------
qf(0.05, df1 = 5, df2 = 9, lower.tail = FALSE)

## ----prob-clt----------------------------------------------------------------------------------------------
pnorm(-2.4658)
pnorm(5.5, mean = 6.1, sd = 1.5 / sqrt(38)) # Alternatively

## ----prob-clt-2--------------------------------------------------------------------------------------------
pnorm(-3)

## ----prob-clt-7--------------------------------------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6))

## ----prob-clt-8--------------------------------------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6)) - pnorm(7.5, mean = 10, sd = sqrt(6))

## ----prob-clt-9--------------------------------------------------------------------------------------------
pbinom(8, size = 25, prob = 0.4)
dbinom(8, size = 25, prob = 0.4)

## ----prob-clt-4--------------------------------------------------------------------------------------------
1 - pbinom(54, size = 100, prob = 0.5)
pbinom(54, size = 100, prob = 0.5, lower.tail = FALSE) # Alternatively

## ----prob-clt-5--------------------------------------------------------------------------------------------
1 - pnorm(0.55, mean = 0.5, sd = sqrt(0.25 / 100))

## ----prob-clt-6--------------------------------------------------------------------------------------------
1 - pnorm(54.5, mean = 50, sd = sqrt(25))

## ----ci-mu-1-----------------------------------------------------------------------------------------------
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

## ----ci-mu-2-----------------------------------------------------------------------------------------------
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

## ----ci-sigma2---------------------------------------------------------------------------------------------
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

## ----ci-diff-mu-1------------------------------------------------------------------------------------------
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

## ----ci-diff-mu-2------------------------------------------------------------------------------------------
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

## ----ratio-vars-1------------------------------------------------------------------------------------------
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

## ----ratio-vars-2------------------------------------------------------------------------------------------
# Xbar1, Xbar2, S^2, t_{alpha/2;n1-n2-2}
X_bar_1 <- 64
X_bar_2 <- 69
S2 <- ((n_1 - 1) * S2_prime_1 + (n_2 - 1) * S2_prime_2) / (n_1 + n_2 - 2)
t <- qt(1 - alpha / 2, df = n_1 + n_2 - 2, lower.tail = TRUE)

# CI
(X_bar_1 - X_bar_2) + c(-1, 1) * t * sqrt(S2 * (1 / n_1 + 1 / n_2))

## ----aci-1-------------------------------------------------------------------------------------------------
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

## ----ci-1--------------------------------------------------------------------------------------------------
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

## ----ci-2--------------------------------------------------------------------------------------------------
# Sample from Exp(2)
set.seed(123456)
n <- 200
x <- rexp(n = n, rate = 2)

## ----ci-3--------------------------------------------------------------------------------------------------
# Bootstrap confidence interval
lambda_hat_exp <- function(x) 1 / mean(x)
boot_ci(x = x, B = 5e3, theta_hat = lambda_hat_exp)

## ----ci-4--------------------------------------------------------------------------------------------------
# Asymptotic confidence interval
alpha <- 0.05
z <- qnorm(alpha / 2, lower.tail = FALSE)
lambda_mle <- 1 / mean(x)
lambda_mle + c(-1, 1) * z * lambda_mle / sqrt(n)

## ----ci-5--------------------------------------------------------------------------------------------------
# Sample
X <- c(916, 892, 895, 904, 913, 916, 895, 885)

# Bootstrap confidence interval
boot_ci(x = X, B = 5e3, theta_hat = mean)

## ----qt-2--------------------------------------------------------------------------------------------------
qt(0.025, df = 7, lower.tail = FALSE)

## ----t-test-1-2--------------------------------------------------------------------------------------------
# Apply t.test() with H1: mu < 915
t.test(x = c(910, 921, 918, 896, 909, 905, 895, 903), mu = 915,
       alternative = "less")

## ----t-test-2-2--------------------------------------------------------------------------------------------
# Apply t.test() with equal variances and H1: mu1 != mu2
std <- c(32, 37, 35, 28, 41, 44, 35, 31, 34)
new <- c(35, 31, 29, 25, 34, 40, 27, 32, 31)
t.test(x = std, y = new, alternative = "two.sided", var.equal = TRUE,
       paired = FALSE)

## ----var-test-0, echo = FALSE------------------------------------------------------------------------------
men <- c(16.0, 13.4, 17.7, 10.2, 13.1, 15.4, 15.9, 11.9, 13.9, 15.5, 15.9,
         12.5, 16.5, 16.5)
wom <- c(5.8, 6.4, 13.1, 7.2, 12.8, 9.8, 10.5, 18.9, 13.7, 13.7, 9.8, 11.5)

## ----var-test-2--------------------------------------------------------------------------------------------
qf(0.025, df1 = 13, df2 = 11, lower.tail = FALSE)
qf(0.975, df1 = 13, df2 = 11, lower.tail = FALSE)

## ----var-test-4--------------------------------------------------------------------------------------------
# Apply var.test() with H1: sigma_1^2 != sigma_2^2
men <- c(16.0, 13.4, 17.7, 10.2, 13.1, 15.4, 15.9, 11.9, 13.9, 15.5, 15.9,
         12.5, 16.5, 16.5)
wom <- c(5.8, 6.4, 13.1, 7.2, 12.8, 9.8, 10.5, 18.9, 13.7, 13.7, 9.8, 11.5)
var.test(x = men, y = wom, alternative = "two.sided")

## ----pbiom-------------------------------------------------------------------------------------------------
pbinom(3, size = 15, prob = 0.5)

## ----pt----------------------------------------------------------------------------------------------------
pt(-1.992, df = 119)

## ----lrt-1-------------------------------------------------------------------------------------------------
# Data
xbar_1 <- 20
xbar_2 <- 22
n_1 <- 87
n_2 <- 61

# MLE's under H0 and H1
theta_hat_0 <- (n_1 * xbar_1 + n_2 * xbar_2) / (n_1 + n_2)
theta_hat_1 <- xbar_1
theta_hat_2 <- xbar_2

# Log-likelihood ratio statistic
log_lamba_n <- (n_1 * xbar_1 + n_2 * xbar_2) * log(theta_hat_0) -
  (n_1 * xbar_1) * log(theta_hat_1) - (n_2 * xbar_2) * log(theta_hat_2) -
  (n_1 + n_2) * theta_hat_0 + (n_1 * theta_hat_1 + n_2 * theta_hat_2)
-2 * log_lamba_n

## ----lrt-2-------------------------------------------------------------------------------------------------
# p-value of the test
pchisq(-2 * log_lamba_n, df = 1, lower.tail = FALSE)

## ----lrt-3-------------------------------------------------------------------------------------------------
# Log-likelihood
log_lik <- function(x, theta) {
  n <- length(x)
  -n * log(theta) - n * mean(x) / theta
}

# Estimator function
theta_hat <- function(x) {
  mean(x)
}

# theta_0
theta_0 <- 1.5

# Statistic and p-value for first sample
x <- c(1.69, 1.15, 2.66, 0.06, 0.11)
(log_lambda_n <- -2 * (log_lik(x = x, theta = theta_0) -
                         log_lik(x = x, theta = theta_hat(x = x))))
pchisq(q = log_lambda_n, df = 1, lower.tail = FALSE)

# Statistic and p-value for second sample
x <- c(1.05, 0.72, 1.66, 0.04, 0.07, 0.4, 0.39)
(log_lambda_n <- -2 * (log_lik(x = x, theta = theta_0) -
                         log_lik(x = x, theta = theta_hat(x = x))))
pchisq(q = log_lambda_n, df = 1, lower.tail = FALSE)

