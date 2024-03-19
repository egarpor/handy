
## ----------------------------------------------------------------------------
## Name: 06-hypothesis-tests.R
## Description: Script for Chapter 6 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 2.4.1
## ----------------------------------------------------------------------------

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

