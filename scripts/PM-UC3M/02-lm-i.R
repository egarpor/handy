
## ------------------------------------------------------------------------
## Name: 02-lm-i.R
## Description: Script for Chapter 2 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- lscheck-1-----------------------------------------------------------------------------------------------------
# Generates 50 points from a N(0, 1): predictor and error
set.seed(34567)
x <- rnorm(n = 50)
eps <- rnorm(n = 50)

# Responses
yLin <- -0.5 + 1.5 * x + eps
yQua <- -0.5 + 1.5 * x^2 + eps
yExp <- -0.5 + 1.5 * 2^x + eps

# Data
leastSquares <- data.frame(x = x, yLin = yLin, yQua = yQua, yExp = yExp)

## ---- lscheck-2, fig.cap = '(ref:lscheck-2-title)'------------------------------------------------------------------
# Call lm
lm(yLin ~ x, data = leastSquares)
lm(yQua ~ x, data = leastSquares)
lm(yExp ~ x, data = leastSquares)

# The lm object
mod <- lm(yLin ~ x, data = leastSquares)
mod

# We can produce a plot with the linear fit easily
plot(x, yLin)
abline(coef = mod$coefficients, col = 2)

# Access coefficients with $coefficients
mod$coefficients

# Compute the minimized RSS
sum((yLin - mod$coefficients[1] - mod$coefficients[2] * x)^2)
sum(mod$residuals^2)

# mod is a list of objects whose names are
names(mod)

## ---- lmcov---------------------------------------------------------------------------------------------------------
# Covariance
Sxy <- cov(x, yLin)

# Variance
Sx2 <- var(x)

# Coefficients
beta1 <- Sxy / Sx2
beta0 <- mean(yLin) - beta1 * mean(x)
c(beta0, beta1)

# Output from lm
mod <- lm(yLin ~ x, data = leastSquares)
mod$coefficients

## ---- wintab-1, eval = FALSE----------------------------------------------------------------------------------------
## # Read data
## wine <- read.table(file = "wine.csv", header = TRUE, sep = ",")

## ---- wintab-2, fig.margin = FALSE, fig.fullwidth = TRUE, out.width = .tex_web('85%', '100%'), fig.cap = '(ref:wintab-2-title)'----
# Numerical -- marginal distributions
summary(wine)

# Graphical -- pairwise relations with linear and "smooth" regressions
car::scatterplotMatrix(wine, col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))

## ---- wintab-3------------------------------------------------------------------------------------------------------
# Set row names to Year -- useful for outlier identification
row.names(wine) <- wine$Year
wine$Year <- NULL

## ---- pred----------------------------------------------------------------------------------------------------------
# Price ~ AGST
modAGST <- lm(Price ~ AGST, data = wine)

# Summary of the model
summary(modAGST)

# The summary is also an object
sumModAGST <- summary(modAGST)
names(sumModAGST)

# R^2
sumModAGST$r.squared

## ---- mods, echo = FALSE--------------------------------------------------------------------------------------------
modFrancePop <- lm(Price ~ FrancePop, data = wine)
modAge <- lm(Price ~ Age, data = wine)
modWinterRain <- lm(Price ~ WinterRain, data = wine)
modHarvestRain <- lm(Price ~ HarvestRain, data = wine)

## ---- lscheck3d-1, echo = FALSE-------------------------------------------------------------------------------------
# Generates 50 points from a N(0, 1): predictors and error
set.seed(34567) # Fixes the seed for the random generator
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- x1 + rnorm(50, sd = 0.05) # Make variables dependent
eps <- rnorm(50)

# Responses
yLin <- -0.5 + 0.5 * x1 + 0.5 * x2 + eps
yQua <- -0.5 + x1^2 + 0.5 * x2 + eps
yExp <- -0.5 + 0.5 * exp(x2) + x3 + eps

# Data
leastSquares3D <- data.frame(x1 = x1, x2 = x2, yLin = yLin,
                             yQua = yQua, yExp = yExp)

## ---- lscheck3d-2, eval = FALSE-------------------------------------------------------------------------------------
## load(file = "least-Squares-3D.RData")

## ---- ls3dcheck-3---------------------------------------------------------------------------------------------------
# Output from lm
mod <- lm(yLin ~ x1 + x2, data = leastSquares3D)
mod$coefficients

# Matrix X
X <- cbind(1, leastSquares3D$x1, leastSquares3D$x2)

# Vector Y
Y <- leastSquares3D$yLin

# Coefficients
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
# %*% multiplies matrices
# solve() computes the inverse of a matrix
# t() transposes a matrix
beta

## ---- fitres, eval = FALSE------------------------------------------------------------------------------------------
## # Fitted values
## mod$fitted.values
## 
## # Residuals
## mod$residuals

## ---- datamarg------------------------------------------------------------------------------------------------------
set.seed(212542)
n <- 100
x1 <- rnorm(n, sd = 2)
x2 <- rnorm(n, mean = x1, sd = 3)
y <- 1 + 2 * x1 - x2 + rnorm(n, sd = 1)
data <- data.frame(x1 = x1, x2 = x2, y = y)

## ---- case1-1-------------------------------------------------------------------------------------------------------
# Regression on all the predictors
modWine1 <- lm(Price ~ Age + AGST + FrancePop + HarvestRain + WinterRain,
               data = wine)

# A shortcut
modWine1 <- lm(Price ~ ., data = wine)
modWine1

# Summary
summary(modWine1)

## ---- case1-2-------------------------------------------------------------------------------------------------------
# Fit
modWine1 <- lm(Price ~ ., data = wine)

# Summary
sumModWine1 <- summary(modWine1)
sumModWine1

# Contains the estimation of sigma ("Residual standard error")
sumModWine1$sigma

# Which is the same as
sqrt(sum(modWine1$residuals^2) / modWine1$df.residual)

## ---- case1-3-------------------------------------------------------------------------------------------------------
modWine2 <- lm(Price ~ . - FrancePop, data = wine)
summary(modWine2)

## ---- case1-4-------------------------------------------------------------------------------------------------------
car::compareCoefs(modWine1, modWine2)

## ---- case1-5-------------------------------------------------------------------------------------------------------
# Fit a new model
modWine3 <- lm(Price ~ Age + WinterRain, data = wine)
summary(modWine3)

# Confidence intervals at 95%
# CI: (lwr, upr)
confint(modWine3)

# Confidence intervals at other levels
confint(modWine3, level = 0.90)
confint(modWine3, level = 0.99)

# Compare with previous models
confint(modWine1)
confint(modWine2)
confint(modWine3)

## ---- case1-5b------------------------------------------------------------------------------------------------------
# By default, scale centers (subtracts the mean) and scales (divides by the
# standard deviation) the columns of a matrix
wineCen <- data.frame(scale(wine, center = TRUE, scale = FALSE))

# Regression with centered response and predictors
modWine3Cen <- lm(Price ~ Age + WinterRain, data = wineCen)

# Summary
summary(modWine3Cen)

## ---- case1-6, error = TRUE-----------------------------------------------------------------------------------------
# Fit a linear model for the price on WinterRain, HarvestRain, and AGST
modWine4 <- lm(Price ~ WinterRain + HarvestRain + AGST, data = wine)
summary(modWine4)

# Data for which we want a prediction
# Important! You have to name the column with the predictor's name!
weather <- data.frame(WinterRain = 500, HarvestRain = 123, AGST = 18)
weatherBad <- data.frame(500, 123, 18)

# Prediction of the mean

# Prediction of the mean at 95% -- the defaults
predict(modWine4, newdata = weather)
predict(modWine4, newdata = weatherBad) # Error

# Prediction of the mean with 95% confidence interval (the default)
# CI: (lwr, upr)
predict(modWine4, newdata = weather, interval = "confidence")
predict(modWine4, newdata = weather, interval = "confidence", level = 0.95)

# Other levels
predict(modWine4, newdata = weather, interval = "confidence", level = 0.90)
predict(modWine4, newdata = weather, interval = "confidence", level = 0.99)

# Prediction of the response

# Prediction of the mean at 95% -- the defaults
predict(modWine4, newdata = weather)

# Prediction of the response with 95% confidence interval
# CI: (lwr, upr)
predict(modWine4, newdata = weather, interval = "prediction")
predict(modWine4, newdata = weather, interval = "prediction", level = 0.95)

# Other levels
predict(modWine4, newdata = weather, interval = "prediction", level = 0.90)
predict(modWine4, newdata = weather, interval = "prediction", level = 0.99)

# Predictions for several values
weather2 <- data.frame(WinterRain = c(500, 200), HarvestRain = c(123, 200),
                       AGST = c(17, 18))
predict(modWine4, newdata = weather2, interval = "prediction")

## ---- simpleAnova---------------------------------------------------------------------------------------------------
# This function computes the simplified anova from a linear model
simpleAnova <- function(object, ...) {

  # Compute anova table
  tab <- anova(object, ...)

  # Obtain number of predictors
  p <- nrow(tab) - 1

  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])

  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)

  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)

}

## ---- case1-7-------------------------------------------------------------------------------------------------------
# Models
modWine1 <- lm(Price ~ ., data = wine)
modWine2 <- lm(Price ~ . - FrancePop, data = wine)

# Simplified table
simpleAnova(modWine1)
simpleAnova(modWine2)
# The null hypothesis of no linear dependence is emphatically rejected in
# both models

# R's ANOVA table -- warning this is not what we saw in lessons
anova(modWine1)

## ---- R2bad-1, fig.cap = '(ref:R2bad-1-title)'----------------------------------------------------------------------
# Simple linear model

# Create data that:
# 1) does not follow a linear model
# 2) the error is heteroskedastic
x <- seq(0.15, 1, l = 100)
set.seed(123456)
eps <- rnorm(n = 100, sd = 0.25 * x^2)
y <- 1 - 2 * x * (1 + 0.25 * sin(4 * pi * x)) + eps

# Great R^2!?
reg <- lm(y ~ x)
summary(reg)

# scatterplot is a quick alternative to
# plot(x, y)
# abline(coef = reg$coef, col = 3)

# But prediction is obviously problematic
car::scatterplot(y ~ x, col = 1, regLine = list(col = 2), smooth = FALSE)

## ---- R2bad-2-------------------------------------------------------------------------------------------------------
# Multiple linear model

# Create data that:
# 1) does not follow a linear model
# 2) the error is heteroskedastic
x1 <- seq(0.15, 1, l = 100)
set.seed(123456)
x2 <- runif(100, -3, 3)
eps <- rnorm(n = 100, sd = 0.25 * x1^2)
y <- 1 - 3 * x1 * (1 + 0.25 * sin(4 * pi * x1)) + 0.25 * cos(x2) + eps

# Great R^2!?
reg <- lm(y ~ x1 + x2)
summary(reg)

## ---- R2bad-4, eval = knitr:::is_html_output()----------------------------------------------------------------------
## # But prediction is obviously problematic
## car::scatter3d(y ~ x1 + x2, fit = "linear")
## rgl::rglwidget()

## ---- R2bad-3, eval = FALSE-----------------------------------------------------------------------------------------
## # Generate data
## p <- 198
## n <- 200
## set.seed(3456732)
## beta <- c(0.5, -0.5, rep(0, p - 2))
## X <- matrix(rnorm(n * p), nrow = n, ncol = p)
## Y <- drop(X %*% beta + rnorm(n, sd = 3))
## data <- data.frame(y = Y, x = X)
## 
## # Regression on the two meaningful predictors
## summary(lm(y ~ x.1 + x.2, data = data))
## 
## # Adding 20 "garbage" variables
## # R^2 increases and adjusted R^2 decreases
## summary(lm(y ~ X[, 1:22], data = data))

## ---- R2Adjneg------------------------------------------------------------------------------------------------------
# Three independent variables
set.seed(234599)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 1 + rnorm(100)

# Negative adjusted R^2
summary(lm(y ~ x1 + x2))

## ---- R2danger-2----------------------------------------------------------------------------------------------------
# Model with intercept
mod1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
mod1

# Model without intercept
mod0 <- lm(Sepal.Length ~ 0 + Petal.Width, data = iris)
mod0

# Recall the different way of obtaining the estimators
X1 <- cbind(1, iris$Petal.Width)
X0 <- cbind(iris$Petal.Width) # No column of ones!
Y <- iris$Sepal.Length
betaHat1 <- solve(crossprod(X1)) %*% t(X1) %*% Y
betaHat0 <- solve(crossprod(X0)) %*% t(X0) %*% Y
betaHat1
betaHat0

# Summaries: higher R^2 for the model with no intercept!?
summary(mod1)
summary(mod0)

# Wait a moment... let's see the actual fit
plot(Sepal.Length ~ Petal.Width, data = iris)
abline(mod1, col = 2) # Obviously, much better
abline(mod0, col = 3)

# Compute the R^2 manually for mod1
SSE1 <- sum((mod1$residuals - mean(mod1$residuals))^2)
SST1 <- sum((mod1$model$Sepal.Length - mean(mod1$model$Sepal.Length))^2)
1 - SSE1 / SST1

# Compute the R^2 manually for mod0
SSE0 <- sum((mod0$residuals - mean(mod0$residuals))^2)
SST0 <- sum((mod0$model$Sepal.Length - mean(mod0$model$Sepal.Length))^2)
1 - SSE0 / SST0
# It is negative!

# Recall that the mean of the residuals is not zero!
mean(mod0$residuals)

# What summary really returns if there is no intercept
n <- nrow(iris)
p <- 1
R0 <- 1 - sum(mod0$residuals^2) / sum(mod0$model$Sepal.Length^2)
R0Adj <- 1 - sum(mod0$residuals^2) / sum(mod0$model$Sepal.Length^2) *
  (n - 1) / (n - p - 1)
R0
R0Adj

# What if we centered the data previously?
irisCen <- data.frame(scale(iris[, -5], center = TRUE, scale = FALSE))
modCen1 <- lm(Sepal.Length ~ Petal.Width, data = irisCen)
modCen0 <- lm(Sepal.Length ~ 0 + Petal.Width, data = irisCen)

# No problem, "correct" R^2
summary(modCen1)
summary(modCen0)

# But only if we center predictor and response...
summary(lm(iris$Sepal.Length ~ 0 + irisCen$Petal.Width))

## ---- case1-8-------------------------------------------------------------------------------------------------------
# Fit models
modWine1 <- lm(Price ~ ., data = wine)
modWine2 <- lm(Price ~ . - FrancePop, data = wine)
modWine3 <- lm(Price ~ Age + WinterRain, data = wine)

# Summaries
summary(modWine1)
summary(modWine2)
summary(modWine3)

