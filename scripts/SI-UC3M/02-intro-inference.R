
## ----------------------------------------------------------------------------
## Name: 02-intro-inference.R
## Description: Script for Chapter 2 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 2.4.3
## ----------------------------------------------------------------------------

## ----normal-prob---------------------------------------------------------------------------
# Computation of P(Z > k)
k <- 0.96
1 - pnorm(k) # 1 - P(Z <= k)
pnorm(k, lower.tail = FALSE) # Alternatively

## ----normal-prob-2-------------------------------------------------------------------------
alpha <- 0.05
qnorm(1 - alpha / 2) # LOWER (1 - beta)-quantile = UPPER beta-quantile
qnorm(alpha / 2, lower.tail = FALSE) # Alternatively, lower.tail = FALSE
# computes the upper quantile and lower.tail = TRUE (the default) computes the
# lower quantile

## ----chi-prob-1----------------------------------------------------------------------------
alpha <- 0.05
qchisq(1 - alpha, df = 6) # df stands for the degrees of freedom
qchisq(alpha, df = 6, lower.tail = FALSE) # Alternatively

## ----chi-prob-2----------------------------------------------------------------------------
alpha <- 0.10
qchisq(1 - alpha / 2, df = 9, lower.tail = FALSE) # a1
qchisq(alpha / 2, df = 9, lower.tail = FALSE) # a2

## ----t-prob--------------------------------------------------------------------------------
pt(-2, df = 5)

## ----F-prob--------------------------------------------------------------------------------
qf(0.05, df1 = 5, df2 = 9, lower.tail = FALSE)

## ----prob-clt------------------------------------------------------------------------------
pnorm(-2.4658)
pnorm(5.5, mean = 6.1, sd = 1.5 / sqrt(38)) # Alternatively

## ----prob-clt-2----------------------------------------------------------------------------
pnorm(-3)

## ----prob-clt-7----------------------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6))

## ----prob-clt-8----------------------------------------------------------------------------
pnorm(8.5, mean = 10, sd = sqrt(6)) - pnorm(7.5, mean = 10, sd = sqrt(6))

## ----prob-clt-9----------------------------------------------------------------------------
pbinom(8, size = 25, prob = 0.4)
dbinom(8, size = 25, prob = 0.4)

## ----prob-clt-4----------------------------------------------------------------------------
1 - pbinom(54, size = 100, prob = 0.5)
pbinom(54, size = 100, prob = 0.5, lower.tail = FALSE) # Alternatively

## ----prob-clt-5----------------------------------------------------------------------------
1 - pnorm(0.55, mean = 0.5, sd = sqrt(0.25 / 100))

## ----prob-clt-6----------------------------------------------------------------------------
1 - pnorm(54.5, mean = 50, sd = sqrt(25))

