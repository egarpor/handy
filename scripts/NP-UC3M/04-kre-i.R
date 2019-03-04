
## ------------------------------------------------------------------------
## Name: 04-kre-i.R
## Description: Script for Chapter 4 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- nw-1, fig.cap = '(ref:nw-1title)', fig.margin = FALSE--------------
# A naive implementation of the Nadaraya-Watson estimator
nw <- function(x, X, Y, h, K = dnorm) {

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
# m <- function(x) x - x^2 # Other possible regression function, works 
# equally well
X <- rnorm(n, sd = 2)
Y <- m(X) + eps
x_grid <- seq(-10, 10, l = 500)

# Bandwidth
h <- 0.5

# Plot data
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(x_grid, nw(x = x_grid, X = X, Y = Y, h = h), col = 2)
legend("top", legend = c("True regression", "Nadaraya-Watson"), 
       lwd = 2, col = 1:2)

## ---- nw-2, eval = FALSE-------------------------------------------------
## # Simple plot of N-W for varying h's
## manipulate::manipulate({
## 
##   # Plot data
##   plot(X, Y)
##   rug(X, side = 1); rug(Y, side = 2)
##   lines(x_grid, m(x_grid), col = 1)
##   lines(x_grid, nw(x = x_grid, X = X, Y = Y, h = h), col = 2)
##   legend("topright", legend = c("True regression", "Nadaraya-Watson"),
##          lwd = 2, col = 1:2)
## 
## }, h = manipulate::slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))

## ---- lp-1---------------------------------------------------------------
# Generate some data
set.seed(123456)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^3 * sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
x_grid <- seq(-10, 10, l = 500)

# KernSmooth::locpoly fits
h <- 0.25
lp0 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = 0,
                           range.x = c(-10, 10), gridsize = 500)
lp1 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = 1,
                           range.x = c(-10, 10), gridsize = 500)
# Provide the evaluation points by range.x and gridsize

# loess fits
span <- 0.25 # The default span is 0.75, which works very bad in this scenario
lo0 <- loess(Y ~ X, degree = 0, span = span)
lo1 <- loess(Y ~ X, degree = 1, span = span)
# loess employs an "span" argument that plays the role of an variable bandwidth
# "span" gives the proportion of points of the sample that are taken into
# account for performing the local fit around x and then uses a triweight kernel
# (not a normal kernel) for weighting the contributions. Therefore, the final
# estimate differs from the definition of local polynomial estimator, although
# the principles in which are based are the same

# Prediction at x = 2
x <- 2
lp1$y[which.min(abs(lp1$x - x))] # Prediction by KernSmooth::locpoly
predict(lo1, newdata = data.frame(X = x)) # Prediction by loess
m(x) # Reality

# Plot data
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(lp0$x, lp0$y, col = 2)
lines(lp1$x, lp1$y, col = 3)
lines(x_grid, predict(lo0, newdata = data.frame(X = x_grid)), col = 2, lty = 2)
lines(x_grid, predict(lo1, newdata = data.frame(X = x_grid)), col = 3, lty = 2)
legend("bottom", legend = c("True regression", "Local constant (locpoly)",
                            "Local linear (locpoly)", "Local constant (loess)",
                            "Local linear (loess)"),
       lwd = 2, col = c(1:3, 2:3), lty = c(rep(1, 3), rep(2, 2)))

## ---- lp-2, eval = FALSE-------------------------------------------------
## # Simple plot of local polynomials for varying h's
## manipulate::manipulate({
## 
##   # Plot data
##   lpp <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = p,
##                              range.x = c(-10, 10), gridsize = 500)
##   plot(X, Y)
##   rug(X, side = 1); rug(Y, side = 2)
##   lines(x_grid, m(x_grid), col = 1)
##   lines(lpp$x, lpp$y, col = p + 2)
##   legend("bottom", legend = c("True regression", "Local polynomial fit"),
##          lwd = 2, col = c(1, p + 2))
## 
## p = manipulate::slider(min = 0, max = 4, initial = 0, step = 1))
## }, h = manipulate::slider(min = 0.01, max = 2, initial = 0.5, step = 0.01),

## ---- cv-bad-------------------------------------------------------------
# Grid for representing (3.22)
h_grid <- seq(0.1, 1, l = 200)^2
error <- sapply(h_grid, function(h)
  mean((Y - nw(x = X, X = X, Y = Y, h = h))^2))

# Error curve
plot(h_grid, error, type = "l")
rug(h_grid)
abline(v = h_grid[which.min(error)], col = 2)

## ---- nw-cv--------------------------------------------------------------
# Generate some data to test the implementation
set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 + sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
x_grid <- seq(-10, 10, l = 500)

# Objective function
cv_nw <- function(X, Y, h, K = dnorm) {
  
  	sum(((Y - nw(x = X, X = X, Y = Y, h = h, K = K)) /
  	       (1 - K(0) / colSums(K(outer(X, X, "-") / h))))^2)
  
}

# Find optimum CV bandwidth, with sensible grid
bw_cv_grid <- function(X, Y, 
                       h_grid = diff(range(X)) * (seq(0.05, 0.5, l = 200))^2,
                       K = dnorm, plot_cv = FALSE) {
  
  # Minimize the CV function on a grid
	obj <- sapply(h_grid, function(h) cv_nw(X = X, Y = Y, h = h, K = K))
	h <- h_grid[which.min(obj)]
  
	# Add plot of the CV loss
	if (plot_cv) {
	  
    plot(h_grid, obj, type = "o")
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
    
  }
  
	# CV bandwidth
	return(h)
	
}

# Bandwidth
h <- bw_cv_grid(X = X, Y = Y, plot_cv = TRUE)
h

# Plot result
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(x_grid, nw(x = x_grid, X = X, Y = Y, h = h), col = 2)
legend("topright", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- kre-ex-------------------------------------------------------------
# Data - nonlinear trend
data(Auto, package = "ISLR")
X <- Auto$weight
Y <- Auto$mpg
plot(X, Y, xlab = "weight", ylab = "mpg", pch = 16)

# CV bandwidth
h <- bw_cv_grid(X = X, Y = Y, plot_cv = TRUE)
h

# Add regression
x_grid <- seq(1600, 5200, by = 10)
plot(X, Y, xlab = "weight", ylab = "mpg", pch = 16)
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, nw(x = x_grid, X = X, Y = Y, h = h), col = 2)

