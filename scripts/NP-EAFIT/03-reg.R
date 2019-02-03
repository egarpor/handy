
## ------------------------------------------------------------------------
## Name: 03-reg.R
## Description: Script for Chapter 3 of "A Short Course on Nonparametric Curve Estimation"
## Link: https://bookdown.org/egarpor/NP-EAFIT
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- lm-----------------------------------------------------------------
# Create the data employed in Figure 3.1

# Generates 50 points from a N(0, 1): predictors and error
set.seed(34567)
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- x1 + rnorm(50, sd = 0.05) # Make variables dependent
eps <- rnorm(50)

# Responses
yLin <- -0.5 + 0.5 * x1 + 0.5 * x2 + eps
yQua <- -0.5 + x1^2 + 0.5 * x2 + eps
yExp <- -0.5 + 0.5 * exp(x2) + x3 + eps

# Data
dataAnimation <- data.frame(x1 = x1, x2 = x2, yLin = yLin,
                            yQua = yQua, yExp = yExp)

# Call lm
# lm employs formula = response ~ predictor1 + predictor2 + ...
# (names according to the data frame names) for denoting the regression
# to be done
mod <- lm(yLin ~ x1 + x2, data = dataAnimation)
summary(mod)

# mod is a list with a lot of information
# str(mod) # Long output

# Coefficients
mod$coefficients

# Application of formula (3.4)

# Matrix X
X <- cbind(1, x1, x2)

# Vector Y
Y <- yLin

# Coefficients
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta

## ---- glm-1--------------------------------------------------------------
# Create the data employed in Figure 3.4

# Data
set.seed(34567)
x <- rnorm(50, sd = 1.5)
y1 <- -0.5 + 3 * x
y2 <- 0.5 - 2 * x
y3 <- -2 + 5 * x
y1 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y1)))
y2 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y2)))
y3 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y3)))

# Data
dataMle <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3)

## ---- glm-2--------------------------------------------------------------
# Call glm
# glm employs formula = response ~ predictor1 + predictor2 + ...
# (names according to the data frame names) for denoting the regression
# to be done. We need to specify family = "binomial" to make a
# logistic regression
mod <- glm(y1 ~ x, family = "binomial", data = dataMle)
summary(mod)

# mod is a list with a lot of information
# str(mod) # Long output

# Coefficients
mod$coefficients

# Plot the fitted regression curve
xGrid <- seq(-5, 5, l = 200)
yGrid <- 1 / (1 + exp(-(mod$coefficients[1] + mod$coefficients[2] * xGrid)))
plot(xGrid, yGrid, type = "l", col = 2, xlab = "x", ylab = "y")
points(x, y1)

## ---- nw-1---------------------------------------------------------------
# Nadaraya-Watson
mNW <- function(x, X, Y, h, K = dnorm) {

  # Arguments
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel

  # Matrix of size n x length(x)
  Kx <- sapply(X, function(Xi) K((x - Xi) / h) / h)

  # Weights
  W <- Kx / rowSums(Kx) # Column recycling!

  # Means at x ("drop" to drop the matrix attributes)
  drop(W %*% Y)

}

# Generate some data to test the implementation
set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 * cos(x)
X <- rnorm(n, sd = 2)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# Bandwidth
h <- 0.5

# Plot data
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h), col = 2)
legend("topright", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- nw-2, eval = FALSE-------------------------------------------------
## # Simple plot of N-W for varying h's
## manipulate({
## 
##   # Plot data
##   plot(X, Y)
##   rug(X, side = 1); rug(Y, side = 2)
##   lines(xGrid, m(xGrid), col = 1)
##   lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h), col = 2)
##   legend("topright", legend = c("True regression", "Nadaraya-Watson"),
##          lwd = 2, col = 1:2)
## 
## }, h = slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))
## 

## ---- loc-1--------------------------------------------------------------
# Generate some data to test the implementation
set.seed(123456)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^3 * sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# Bandwidth
h <- 0.25
lp0 <- locpoly(x = X, y = Y, bandwidth = h, degree = 0, range.x = c(-10, 10),
               gridsize = 500)
lp1 <- locpoly(x = X, y = Y, bandwidth = h, degree = 1, range.x = c(-10, 10),
               gridsize = 500)

# Plot data
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(lp0$x, lp0$y, col = 2)
lines(lp1$x, lp1$y, col = 3)
legend("bottom", legend = c("True regression", "Local constant",
                         "Local linear"),
       lwd = 2, col = 1:3)

## ---- loc-2, eval = FALSE------------------------------------------------
## # Simple plot of local polynomials for varying h's
## manipulate({
## 
##   # Plot data
##   lpp <- locpoly(x = X, y = Y, bandwidth = h, degree = p, range.x = c(-10, 10),
##                  gridsize = 500)
##   plot(X, Y)
##   rug(X, side = 1); rug(Y, side = 2)
##   lines(xGrid, m(xGrid), col = 1)
##   lines(lpp$x, lpp$y, col = p + 2)
##   legend("bottom", legend = c("True regression", "Local polynomial fit"),
##          lwd = 2, col = c(1, p + 2))
## 
## }, h = slider(min = 0.01, max = 2, initial = 0.5, step = 0.01),
## p = slider(min = 0, max = 4, initial = 0, step = 1))
## 

## ---- badh---------------------------------------------------------------
# Grid for representing (3.22)
hGrid <- seq(0.1, 1, l = 200)^2
error <- sapply(hGrid, function(h)
  mean((Y - mNW(x = X, X = X, Y = Y, h = h))^2))

# Error curve
plot(hGrid, error, type = "l")
rug(hGrid)
abline(v = hGrid[which.min(error)], col = 2)

## ---- nw-cv--------------------------------------------------------------
# Generate some data to test the implementation
set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 + sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# Objective function
cvNW <- function(X, Y, h, K = dnorm) {
  	sum(((Y - mNW(x = X, X = X, Y = Y, h = h, K = K)) /
  	       (1 - K(0) / colSums(K(outer(X, X, "-") / h))))^2)
}

# Find optimum CV bandwidth, with sensible grid
bw.cv.grid <- function(X, Y,
                       h.grid = diff(range(X)) * (seq(0.1, 1, l = 200))^2,
                       K = dnorm, plot.cv = FALSE) {
	obj <- sapply(h.grid, function(h) cvNW(X = X, Y = Y, h = h, K = K))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Bandwidth
h <- bw.cv.grid(X = X, Y = Y, plot.cv = TRUE)
h

# Plot result
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h), col = 2)
legend("topright", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- loclog-------------------------------------------------------------
# Simulate some data
n <- 200
logistic <- function(x) 1 / (1 + exp(-x))
p <- function(x) logistic(1 - 3 * sin(x))
set.seed(123456)
X <- runif(n = n, -3, 3)
Y <- rbinom(n = n, size = 1, prob = p(X))

# Set bandwidth and evaluation grid
h <- 0.25
x <- seq(-3, 3, l = 501)

# Optimize the weighted log-likelihood through glm's built in procedure
suppressWarnings(
  fitGlm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    glm.fit(x = cbind(1, X - x), y = Y, weights = K,
            family = binomial())$coefficients[1]
  })
)

# Optimize the weighted log-likelihood explicitly
suppressWarnings(
  fitNlm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    nlm(f = function(beta) {
      -sum(K * (Y * (beta[1] + beta[2] * (X - x)) -
                  log(1 + exp(beta[1] + beta[2] * (X - x)))))
      }, p = c(0, 0))$estimate[1]
  })
)

# Using locfit
# Bandwidth can not be controlled explicitly - only though nn in ?lp
library(locfit)
fitLocfit <- locfit(Y ~ lp(X, deg = 1, nn = 0.25), family = "binomial",
                    kern = "gauss")

# Compare fits
plot(x, p(x), ylim = c(0, 1.5), type = "l", lwd = 2)
lines(x, logistic(fitGlm), col = 2)
lines(x,logistic(fitNlm), col = 2, lty = 2)
plot(fitLocfit, add = TRUE, col = 4)
legend("topright", legend = c("p(x)", "glm", "nlm", "locfit"), lwd = 2,
       col = c(1, 2, 2, 4), lty = c(1, 2, 1, 1))

## ---- loclog-cv----------------------------------------------------------
# Exact LCV - recall that we *maximize* the LCV!
h <- seq(0.1, 2, by = 0.1)
suppressWarnings(
  LCV <- sapply(h, function(h) {
  sum(sapply(1:n, function(i) {
    K <- dnorm(x = X[i], mean = X[-i], sd = h)
    nlm(f = function(beta) {
      -sum(K * (Y[-i] * (beta[1] + beta[2] * (X[-i] - X[i])) -
                  log(1 + exp(beta[1] + beta[2] * (X[-i] - X[i])))))
      }, p = c(0, 0))$minimum
    }))
  })
)
plot(h, LCV, type = "o")
abline(v = h[which.max(LCV)], col = 2)

