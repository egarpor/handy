## ---- install, eval = FALSE, purl = TRUE---------------------------------
## # Installation of required packages
## packages <- c("MASS", "car", "readxl", "rgl", "nortest", "pca3d", "ISLR",
##               "pls", "glmnet", "biglm", "leaps", "lme4", "viridis", "ffbase",
##               "KernSmooth", "np", "locfit", "manipulate", "mice", "VIM",
##               "nnet", "lubridate")
## install.packages(packages)

## ---- packages-hidden, echo = FALSE, purl = TRUE-------------------------
packages <- c("MASS", "car", "readxl", "rgl", "nortest", "pca3d", "ISLR", 
              "pls", "glmnet", "biglm", "leaps", "lme4", "viridis", "ffbase", 
              "KernSmooth", "np", "locfit", "manipulate", "mice", "VIM", 
              "nnet", "lubridate")

## ---- library, results = 'hide', warning = FALSE, message = FALSE, purl = TRUE----
# Load packages
lapply(packages, library, character.only = TRUE)

## ---- speedfuel, out.width = '70%', fig.asp = 1, purl = TRUE-------------
x <- c(64, 20, 14, 64, 44, 39, 25, 53, 48, 9,
       100, 112, 78, 105, 116, 94, 71, 71, 101, 109)
y <- c(4, 6, 6.4, 4.1, 4.9, 4.4, 6.6, 4.4, 3.8, 7,
       7.4, 8.4, 5.2, 7.6, 9.8, 6.4, 5.1, 4.8, 8.2, 8.7)
plot(x, y, xlab = "Speed", ylab = "Fuel consumption")

## ---- abcx2, purl = TRUE-------------------------------------------------
# Estimates for a, b, and c
lm(y ~ x + I(x^2))

## ---- fitparabola, fig.asp = 1, out.width = '70%', purl = TRUE-----------
plot(x, y, xlab = "Speed", ylab = "Fuel consumption")
curve(8.512421 - 0.153291 * x + 0.001408 * x^2, add = TRUE, col = 2)

