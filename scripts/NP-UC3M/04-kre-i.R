
## ----------------------------------------------------------------------------
## Name: 04-kre-i.R
## Description: Script for Chapter 4 of "Notes for Nonparametric Statistics"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 6.5.9
## ----------------------------------------------------------------------------

## ---- nw-1, fig.cap = '(ref:nw-1title)', fig.margin = FALSE---------------
# A naive implementation of the Nadaraya-Watson estimator
nw <- function(x, X, Y, h, K = dnorm) {

  # Arguments
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel

  # Matrix of size n x length(x) (rbind() is called for ensuring a matrix
  # output if x is a scalar)
  Kx <- rbind(sapply(X, function(Xi) K((x - Xi) / h) / h))

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
# m <- function(x) x - x^2 # Works equally well for other regression function
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

## ---- nw-2, eval = FALSE--------------------------------------------------
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
## }, h = manipulate::slider(min = 0.01, max = 10, initial = 0.5, step = 0.01))

## ---- lp-1----------------------------------------------------------------
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
# Provide the evaluation points through range.x and gridsize

# loess fits
span <- 0.25 # The default span is 0.75, which works very bad in this scenario
lo0 <- loess(Y ~ X, degree = 0, span = span)
lo1 <- loess(Y ~ X, degree = 1, span = span)
# loess employs an "span" argument that plays the role of an variable bandwidth
# "span" gives the proportion of points of the sample that are taken into
# account for performing the local fit about x and then uses a triweight kernel
# (not a normal kernel) for weighting the contributions. Therefore, the final
# estimate differs from the definition of local polynomial estimator, although
# the principles in which are based are the same

# Prediction at x = 2
x <- 2
lp1$y[which.min(abs(lp1$x - x))] # Prediction by KernSmooth::locpoly
predict(lo1, newdata = data.frame(X = x)) # Prediction by loess
m(x) # True regression

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

## ---- lp-2, eval = FALSE--------------------------------------------------
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
## }, p = manipulate::slider(min = 0, max = 4, initial = 0, step = 1),
## h = manipulate::slider(min = 0.01, max = 10, initial = 0.5, step = 0.01))

## ---- bwd-1, fig.cap = '(ref:bwd-1-title)'--------------------------------
# Evaluation grid
x_grid <- seq(0, 5, l = 500)

# Quartic-like regression function
m <- function(x) 5 * dnorm(x, mean = 1.5, sd = 0.25) - x

# # Highly nonlinear regression function
# m <- function(x) x * sin(2 * pi * x)

# # Quartic regression function (but expressed in a orthogonal polynomial)
# coefs <- attr(poly(x_grid / 5 * 3, degree = 4), "coefs")
# m <- function(x) 20 * poly(x, degree = 4, coefs = coefs)[, 4]

# # Seventh orthogonal polynomial
# coefs <- attr(poly(x_grid / 5 * 3, degree = 7), "coefs")
# m <- function(x) 20 * poly(x, degree = 7, coefs = coefs)[, 7]

# Generate some data
set.seed(123456)
n <- 250
eps <- rnorm(n)
X <- abs(rnorm(n))
Y <- m(X) + eps

# Rule-of-thumb selector
h_RT <- function(X, Y) {

  # Quartic fit
  mod_Q <- lm(Y ~ poly(X, raw = TRUE, degree = 4))

  # Estimates of unknown quantities
  int_sigma2_hat <- diff(range(X)) * sum(mod_Q$residuals^2) / mod_Q$df.residual
  theta_22_hat <- mean((2 * mod_Q$coefficients[3] +
                        6 * mod_Q$coefficients[4] * X +
                        12 * mod_Q$coefficients[5] * X^2)^2)

  # h_RT
  R_K <- 0.5 / sqrt(pi)
  ((R_K * int_sigma2_hat) / (theta_22_hat * length(X)))^(1 / 5)

}

# Selected bandwidth
(h_ROT <- h_RT(X = X, Y = Y))

# Local linear fit
lp1_RT <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h_ROT, degree = 1,
                              range.x = c(0, 10), gridsize = 500)

# Quartic fit employed
mod_Q <- lm(Y ~ poly(X, raw = TRUE, degree = 4))

# Plot data
plot(X, Y, ylim = c(-6, 8))
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(lp1_RT$x, lp1_RT$y, col = 2)
lines(x_grid, mod_Q$coefficients[1] +
        poly(x_grid, raw = TRUE, degree = 4) %*% mod_Q$coefficients[-1],
      col = 3)
legend("topright", legend = c("True regression", "Local linear (RT)",
                              "Quartic fit"),
       lwd = 2, col = 1:3)

## ---- bwd-2---------------------------------------------------------------
# Generate some data
set.seed(123456)
n <- 250
eps <- rnorm(n)
X <- abs(rnorm(n))
m <- function(x) x * sin(2 * pi * x)
Y <- m(X) + eps

# Selected bandwidth
(h_ROT <- h_RT(X = X, Y = Y))

# DPI selector
(h_DPI <- KernSmooth::dpill(x = X, y = Y))

# Fits
lp1_RT <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h_ROT, degree = 1,
                              range.x = c(0, 10), gridsize = 500)
lp1_DPI <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h_DPI, degree = 1,
                               range.x = c(0, 10), gridsize = 500)

# Plot data
plot(X, Y, ylim = c(-6, 8))
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(lp1_DPI$x, lp1_DPI$y, col = 2)
lines(lp1_RT$x, lp1_RT$y, col = 3)
legend("topleft", legend = c("True regression", "Local linear (DPI)",
                             "Local linear (RT)"), lwd = 2, col = 1:3)

## ---- bwd-3---------------------------------------------------------------
# Grid for representing (4.22)
h_grid <- seq(0.1, 1, l = 200)^2
error <- sapply(h_grid, function(h) {
  mean((Y - nw(x = X, X = X, Y = Y, h = h))^2)
})

# Error curve
plot(h_grid, error, type = "l")
rug(h_grid)
abline(v = h_grid[which.min(error)], col = 2)

## ---- bw-4----------------------------------------------------------------
# Generate some data to test the implementation
set.seed(12345)
n <- 200
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
legend("top", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- bw-5----------------------------------------------------------------
# Slow objective function
cv_nw_slow <- function(X, Y, h, K = dnorm) {

  sum((Y - sapply(1:length(X), function(i) {
    nw(x = X[i], X = X[-i], Y = Y[-i], h = h, K = K)
    }))^2)

}

# Optimum CV bandwidth, with sensible grid
bw_cv_grid_slow <- function(X, Y, h_grid =
                              diff(range(X)) * (seq(0.05, 0.5, l = 200))^2,
                            K = dnorm, plot_cv = FALSE) {

  # Minimize the CV function on a grid
  obj <- sapply(h_grid, function(h) cv_nw_slow(X = X, Y = Y, h = h, K = K))
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

# Same bandwidth
h <- bw_cv_grid_slow(X = X, Y = Y, plot_cv = TRUE)
h

# # Time comparison, a factor 10 difference
# microbenchmark::microbenchmark(bw_cv_grid(X = X, Y = Y),
#                                bw_cv_grid_slow(X = X, Y = Y),
#                                times = 10)

## ---- bw-6----------------------------------------------------------------
# Data -- nonlinear trend
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

## ---- np-1a---------------------------------------------------------------
# Data -- nonlinear trend
data(Auto, package = "ISLR")
X <- Auto$weight
Y <- Auto$mpg

# np::npregbw computes by default the least squares CV bandwidth associated
# with a local *constant* fit and admits a formula interface (to be exploited
# more in multivariate regression)
bw0 <- np::npregbw(formula = Y ~ X)

# The spinner can be omitted with
options(np.messages = FALSE)

# Multiple initial points can be employed for minimizing the CV function (for
# one predictor, defaults to 1) and avoiding local minima
bw0 <- np::npregbw(formula = Y ~ X, nmulti = 2)

# The "rbandwidth" object contains useful information, see ?np::npregbw for
# all the returned objects
bw0
head(bw0)
# Recall that the fit is very similar to h_CV

# Once the bandwidth is estimated, np::npreg can be directly called with the
# "rbandwidth" object (it encodes the regression to be made, the data, the kind
# of estimator considered, etc). The hard work goes on np::npregbw, not on
# np::npreg
kre0 <- np::npreg(bws = bw0)
kre0

# Plot directly the fit via plot() -- it employs as evaluation points the
# (unsorted!) sample
plot(kre0, col = 2, type = "o")
points(X, Y)
rug(X, side = 1); rug(Y, side = 2)
# lines(kre0$eval$X, kre0$mean, col = 3)

## ---- np-2----------------------------------------------------------------
# Local linear fit -- find first the CV bandwidth
bw1 <- np::npregbw(formula = Y ~ X, regtype = "ll")

# Fit
kre1 <- np::npreg(bws = bw1)

# Plot
plot(kre1, col = 2, type = "o")
points(X, Y)
rug(X, side = 1); rug(Y, side = 2)

## ---- np-3----------------------------------------------------------------
# Summary of the npregression object
summary(kre0)

# Evaluation points (a data.frame) -- by default the sample (unsorted!)
head(kre0$eval)

# The estimation of the regression function at the evaluation points
head(kre0$mean)

# The evaluation points can be changed using "exdat"
x_grid <- seq(1000, 5500, by = 5)
kre0 <- np::npreg(bws = bw0, exdat = x_grid)
kre1 <- np::npreg(bws = bw1, exdat = x_grid)

# Notice how $eval is a data.frame containing x_grid
head(kre0$eval)

# This allows to compare estimators in a more transparent form
plot(X, Y)
lines(kre0$eval$x_grid, kre0$mean, col = 2)
lines(kre1$eval$x_grid, kre1$mean, col = 3)
rug(X, side = 1); rug(Y, side = 2)
legend("top", legend = c("Nadaraya-Watson", "Local linear"),
       lwd = 2, col = 2:3)

## ---- np-4----------------------------------------------------------------
# Generate some data with bimodal density
set.seed(12345)
n <- 100
eps <- rnorm(2 * n, sd = 2)
m <- function(x) x^2 * sin(x)
X <- c(rnorm(n, mean = -2, sd = 0.5), rnorm(n, mean = 2, sd = 0.5))
Y <- m(X) + eps
x_grid <- seq(-10, 10, l = 500)

# Constant bandwidth
bwc <- np::npregbw(formula = Y ~ X, bwtype = "fixed",
                   regtype = "ll")
krec <- np::npreg(bwc, exdat = x_grid)

# Variable bandwidths
bwg <- np::npregbw(formula = Y ~ X, bwtype = "generalized_nn",
                   regtype = "ll")
kreg <- np::npreg(bwg, exdat = x_grid)
bwa <- np::npregbw(formula = Y ~ X, bwtype = "adaptive_nn",
                   regtype = "ll")
krea <- np::npreg(bwa, exdat = x_grid)

# Comparison
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(x_grid, m(x_grid), col = 1)
lines(krec$eval$x_grid, krec$mean, col = 2)
lines(kreg$eval$x_grid, kreg$mean, col = 3)
lines(krea$eval$x_grid, krea$mean, col = 4)
legend("top", legend = c("True regression", "Fixed", "Generalized NN",
                         "Adaptive NN"),
       lwd = 2, col = 1:4)
# Observe how the fixed bandwidth may yield a fit that produces serious
# artifacts in the low-density region. At that region the NN-based bandwidths
# expand to borrow strength from the points in the high-density regions,
# whereas in the high-density regions they shrink to adapt faster to the
# changes of the regression function

