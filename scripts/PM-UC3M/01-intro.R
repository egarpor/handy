
## ------------------------------------------------------------------------
## Name: 01-intro.R
## Description: Script for Chapter 1 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- install, eval = FALSE------------------------------------------------------
## # Installation of required packages
## packages <- c("MASS", "car", "readxl", "rgl", "rmarkdown", "nortest",
##               "latex2exp", "pca3d", "ISLR", "pls", "corrplot", "glmnet",
##               "mvtnorm", "biglm", "leaps", "lme4", "viridis", "ffbase",
##               "ks", "KernSmooth", "nor1mix", "np", "locfit",
##               "manipulate", "mice", "VIM", "nnet")
## install.packages(packages)

## ---- library, eval = FALSE------------------------------------------------------
## # Load packages
## lapply(packages, library, character.only = TRUE)

## ---- speedfuel, fig.cap = '(ref:speedfuel-title)'-------------------------------
x <- c(64, 20, 14, 64, 44, 39, 25, 53, 48, 9, 100, 112, 78, 105, 116, 94, 71,
       71, 101, 109)
y <- c(4, 6, 6.4, 4.1, 4.9, 4.4, 6.6, 4.4, 3.8, 7, 7.4, 8.4, 5.2, 7.6, 9.8,
       6.4, 5.1, 4.8, 8.2, 8.7)
plot(x, y, xlab = "Speed", ylab = "Fuel consumption")

## ---- abcx2----------------------------------------------------------------------
# Estimates for a, b, and c
lm(y ~ x + I(x^2))

## ---- fitparabola, fig.cap = '(ref:fitparabola-title)'---------------------------
plot(x, y, xlab = "Speed", ylab = "Fuel consumption")
curve(8.512421 - 0.153291 * x + 0.001408 * x^2, add = TRUE, col = 2)

