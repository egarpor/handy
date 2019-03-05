
## ------------------------------------------------------------------------
## Name: 06-npreg.R
## Description: Script for Chapter 6 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- hist-1-------------------------------------------------------------
# Duration of eruption
faithE <- faithful$eruptions

# Default histogram: automatically choses bins and uses absolute frequencies
histo <- hist(faithE)

# Bins and bin counts
histo$breaks # Bk's
histo$counts # vk's

# With relative frequencies
hist(faithE, probability = TRUE)

# Choosing the breaks
t0 <- min(faithE)
h <- 0.25
Bk <- seq(t0, max(faithE), by = h)
hist(faithE, probability = TRUE, breaks = Bk)
rug(faithE) # The sample

## ---- hist-2, fig.asp = 1/2, fig.cap = '(ref:hist-2-title)', fig.margin = FALSE----
# Uniform sample
set.seed(1234567)
u <- runif(n = 100)

# t0 = 0, h = 0.2
Bk1 <- seq(0, 1, by = 0.2)

# t0 = -0.1, h = 0.2
Bk2 <- seq(-0.1, 1.1, by = 0.2)

# Comparison
par(mfrow = 1:2)
hist(u, probability = TRUE, breaks = Bk1, ylim = c(0, 1.5),
     main = "t0 = 0, h = 0.2")
rug(u)
abline(h = 1, col = 2)
hist(u, probability = TRUE, breaks = Bk2, ylim = c(0, 1.5),
     main = "t0 = -0.1, h = 0.2")
rug(u)
abline(h = 1, col = 2)

## ---- kdeR, eval = TRUE--------------------------------------------------
# Sample 100 points from a N(0, 1)
set.seed(1234567)
samp <- rnorm(n = 100, mean = 0, sd = 1)

# Quickly compute a kernel density estimator and plot the density object
# Automatically chooses bandwidth and uses normal kernel
plot(density(x = samp))

# Select a particular bandwidth (0.5) and kernel (Epanechnikov)
lines(density(x = samp, bw = 0.5, kernel = "epanechnikov"), col = 2)
# density automatically chooses the interval for plotting the kernel density
# estimator (observe that the black line goes to roughly between -3 and 3)

# This can be tuned using "from" and "to"
plot(density(x = samp, from = -4, to = 4), xlim = c(-5, 5))

# The density object is a list
kde <- density(x = samp, from = -5, to = 5, n = 1024)
str(kde)
# Note that the evaluation grid "x"" is not directly controlled, only through
# "from, "to", and "n" (better use powers of 2). This is because, internally,
# kde employs an efficient Fast Fourier Transform on grids of size 2^m

# Plotting by the returned values of the kde
plot(kde$x, kde$y, type = "l")
curve(dnorm(x), col = 2, add = TRUE) # True density
rug(samp)

## ---- bwnrd--------------------------------------------------------------
# Data
set.seed(667478)
n <- 100
x <- rnorm(n)

# Rule-of-thumb
bw.nrd(x = x)
# bwd.nrd employs 1.34 as an approximation for diff(qnorm(c(0.25, 0.75)))

# Same as
iqr <- diff(quantile(x, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
1.06 * n^(-1/5) * min(sd(x), iqr)

## ---- SJ-----------------------------------------------------------------
# Data
set.seed(672641)
x <- rnorm(100)

# DPI selector
bw.SJ(x = x, method = "dpi")

# Similar to
ks::hpi(x) # Default is two-stages

## ---- bw-ucv-mod---------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# UCV gives a warning
bw.ucv(x = x)

# Extend search interval
bw.ucv(x = x, lower = 0.01, upper = 1)

# bw.ucv.mod replaces the optimization routine of bw.ucv by an exhaustive
# search on "h.grid" (chosen adaptatively from the sample) and optionally
# plots the LSCV curve with "plot.cv"
bw.ucv.mod <- function(x, nb = 1000L,
                       h.grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L)
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n))
    stop("invalid length(x)")
  if (!is.numeric(x))
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L)
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fucv <- function(h) .Call(stats:::C_bw_ucv, n, d, cnt, h)
  ## Original
  # h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
  ## Modification
  obj <- sapply(h.grid, function(h) fucv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the LSCV curve
bw.ucv.mod(x = x, plot.cv = TRUE)

# We can compare with the default bw.ucv output
abline(v = bw.ucv(x = x), col = 3)

## ---- bw-bcv-mod---------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# BCV gives a warning
bw.bcv(x = x)

# Extend search interval
args(bw.bcv)
bw.bcv(x = x, lower = 0.01, upper = 1)

# bw.bcv.mod replaces the optimization routine of bw.bcv by an exhaustive
# search on "h.grid" (chosen adaptatively from the sample) and optionally
# plots the BCV curve with "plot.cv"
bw.bcv.mod <- function(x, nb = 1000L,
                       h.grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L)
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n))
    stop("invalid length(x)")
  if (!is.numeric(x))
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L)
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fbcv <- function(h) .Call(stats:::C_bw_bcv, n, d, cnt, h)
  ## Original code
  # h <- optimize(fbcv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
  ## Modification
  obj <- sapply(h.grid, function(h) fbcv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the BCV curve
bw.bcv.mod(x = x, plot.cv = TRUE)

# We can compare with the default bw.bcv output
abline(v = bw.bcv(x = x), col = 3)

## ---- MW-----------------------------------------------------------------
# Available models
?nor1mix::MarronWand

# Simulating - specify density with MW object
samp <- nor1mix::rnorMix(n = 500, obj = nor1mix::MW.nm9)
hist(samp, freq = FALSE)

# Density evaluation
x <- seq(-4, 4, length.out = 400)
lines(x, nor1mix::dnorMix(x = x, obj = nor1mix::MW.nm9), col = 2)

# Plot a MW object directly
# A normal with the same mean and variance is plotted in dashed lines
par(mfrow = c(2, 2))
plot(nor1mix::MW.nm5)
plot(nor1mix::MW.nm7)
plot(nor1mix::MW.nm10)
plot(nor1mix::MW.nm12)
lines(nor1mix::MW.nm1, col = 2:3) # Also possible

## ---- ks-2d-1------------------------------------------------------------
# DPI selectors
Hpi1 <- ks::Hpi(x = faithful)
Hpi1

# Compute kde (if H is missing, ks::Hpi is called)
kdeHpi1 <- ks::kde(x = faithful, H = Hpi1)

# Different representations
plot(kdeHpi1, display = "slice", cont = c(25, 50, 75))
# "cont" specifies the density contours, which are upper percentages of highest
# density regions. The default contours are at 25%, 50%, and 75%
plot(kdeHpi1, display = "filled.contour2", cont = c(25, 50, 75))
plot(kdeHpi1, display = "persp")

# Manual plotting using the kde object structure
image(kdeHpi1$eval.points[[1]], kdeHpi1$eval.points[[2]],
      kdeHpi1$estimate, col = viridis::viridis(20))
points(kdeHpi1$x)

# Diagonal vs. full
Hpi2 <- ks::Hpi.diag(x = faithful)
kdeHpi2 <- ks::kde(x = faithful, H = Hpi2)
plot(kdeHpi1, display = "filled.contour2", cont = c(25, 50, 75),
     main = "full")
plot(kdeHpi2, display = "filled.contour2", cont = c(25, 50, 75),
     main = "diagonal")

## ---- ks-2d-2, fig.margin = FALSE----------------------------------------
# Comparison of selectors along predefined contours
x <- faithful
Hlscv0 <- ks::Hlscv(x = x)
Hbcv0 <- ks::Hbcv(x = x)
Hpi0 <- ks::Hpi(x = x)
Hns0 <- ks::Hns(x = x)
par(mfrow = c(2, 2))
p <- lapply(list(Hlscv0, Hbcv0, Hpi0, Hns0), function(H) {
  # col.fun for custom colours
  plot(ks::kde(x = x, H = H), display = "filled.contour2",
       cont = seq(10, 90, by = 10), col.fun = viridis::viridis)
  points(x, cex = 0.5, pch = 16)
})

## ---- ks-3d, eval = knitr:::is_html_output()-----------------------------
## # Normal scale bandwidth
## Hns1 <- ks::Hns(iris[, 1:3])
## 
## # Show high nested contours of high density regions
## plot(ks::kde(x = iris[, 1:3], H = Hns1))
## points3d(x = iris[, 1:3])
## rgl::rglwidget()

## ---- nw-1, fig.cap = '(ref:nw-1title)', fig.margin = FALSE--------------
# A naive implementation of the Nadaraya-Watson estimator
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
# m <- function(x) x - x^2 # Other possible regression function, works
# equally well
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
legend("top", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- nw-2, eval = FALSE-------------------------------------------------
## # Simple plot of N-W for varying h's
## manipulate::manipulate({
## 
##   # Plot data
##   plot(X, Y)
##   rug(X, side = 1); rug(Y, side = 2)
##   lines(xGrid, m(xGrid), col = 1)
##   lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h), col = 2)
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
xGrid <- seq(-10, 10, l = 500)

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
lines(xGrid, m(xGrid), col = 1)
lines(lp0$x, lp0$y, col = 2)
lines(lp1$x, lp1$y, col = 3)
lines(xGrid, predict(lo0, newdata = data.frame(X = xGrid)), col = 2, lty = 2)
lines(xGrid, predict(lo1, newdata = data.frame(X = xGrid)), col = 3, lty = 2)
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
##   lines(xGrid, m(xGrid), col = 1)
##   lines(lpp$x, lpp$y, col = p + 2)
##   legend("bottom", legend = c("True regression", "Local polynomial fit"),
##          lwd = 2, col = c(1, p + 2))
## 
## }, p = manipulate::slider(min = 0, max = 4, initial = 0, step = 1),
## h = manipulate::slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))

## Under **A1**--**A5**, the conditional bias and variance of the local constant ($p=0$) and local linear ($p=1$) estimators are^[The notation $o_\mathbb{P}(a_n)$ stands for a random variable that converges in probability to zero at a rate faster than $a_n\to0$. It is mostly employed for denoting non-important terms in asymptotic expansions, like the ones in \@ref(eq:mbias)--\@ref(eq:mvar).]

## ---- bwd-1--------------------------------------------------------------
# Generate some data
set.seed(123456)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^3 * sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# DPI selector
hDPI <- KernSmooth::dpill(x = X, y = Y)

# Fits
lp1 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = 0.25, degree = 0,
                           range.x = c(-10, 10), gridsize = 500)
lp1DPI <- KernSmooth::locpoly(x = X, y = Y, bandwidth = hDPI, degree = 1,
                              range.x = c(-10, 10), gridsize = 500)

# Compare fits
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(lp1$x, lp1$y, col = 2)
lines(lp1DPI$x, lp1DPI$y, col = 3)
legend("bottom", legend = c("True regression", "Local linear",
                            "Local linear (DPI)"),
       lwd = 2, col = 1:3)

## ---- bwd-2, eval = FALSE------------------------------------------------
## # Grid for representing (3.22)
## hGrid <- seq(0.1, 1, l = 200)^2
## error <- sapply(hGrid, function(h) {
##   mean((Y - mNW(x = X, X = X, Y = Y, h = h))^2)
##   })
## 
## # Error curve
## plot(hGrid, error, type = "l")
## rug(hGrid)
## abline(v = hGrid[which.min(error)], col = 2)

## For any $p\geq0$, the weights of the leave-one-out estimator $\hat{m}_{-i}(x;p,h)=\sum_{\substack{j=1\\j\neq i}}^nW_{-i,j}^p(x)Y_j$ can be obtained from $\hat{m}(x;p,h)=\sum_{i=1}^nW_{i}^p(x)Y_i$:

## ---- bwd-3--------------------------------------------------------------
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
  # Beware: outer() is not very memory-friendly!

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
hCV <- bw.cv.grid(X = X, Y = Y, plot.cv = TRUE)
hCV

# Plot result
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = hCV), col = 2)
legend("top", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

## ---- bwd-4--------------------------------------------------------------
# np::npregbw computes by default the least squares CV bandwidth associated to
# a local constant fit
bw0 <- np::npregbw(formula = Y ~ X)

# Multiple initial points can be employed for minimizing the CV function (for
# one predictor, defaults to 1)
bw0 <- np::npregbw(formula = Y ~ X, nmulti = 2)

# The "rbandwidth" object contains many useful information, see ?np::npregbw for
# all the returned objects
bw0
# Recall that the fit is very similar to hCV

# Once the bandwith is estimated, np::npreg can be directly called with the
# "rbandwidth" object (it encodes the regression to be made, the data, the kind
# of estimator considered, etc). The hard work goes on np::npregbw, not on
# np::npreg
kre0 <- np::npreg(bw0)
kre0

# The evaluation points of the estimator are by default the predictor's sample
# (which is not sorted!)
# The evaluation of the estimator is given in "mean"
plot(kre0$eval$X, kre0$mean)

# The evaluation points can be changed using "exdat"
kre0 <- np::npreg(bw0, exdat = xGrid)

# Plot directly the fit via plot() - it employs different evaluation points
# than exdat
plot(kre0, col = 2, type = "o")
points(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(kre0$eval$xGrid, kre0$mean, col = 3, type = "o", pch = 16, cex = 0.5)
# Using the evaluation points

# Local linear fit - find first the CV bandwidth
bw1 <- np::npregbw(formula = Y ~ X, regtype = "ll")
# regtype = "ll" stands for "local linear", "lc" for "local constant"

# Local linear fit
kre1 <- np::npreg(bw1, exdat = xGrid)

# Comparison
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(kre0$eval$xGrid, kre0$mean, col = 2)
lines(kre1$eval$xGrid, kre1$mean, col = 3)
legend("top", legend = c("True regression", "Nadaraya-Watson", "Local linear"),
       lwd = 2, col = 1:3)

## ---- bwd-5, fig.margin = FALSE------------------------------------------
# Generate some data with bimodal density
set.seed(12345)
n <- 100
eps <- rnorm(2 * n, sd = 2)
m <- function(x) x^2 * sin(x)
X <- c(rnorm(n, mean = -2, sd = 0.5), rnorm(n, mean = 2, sd = 0.5))
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# Constant bandwidth
bwc <- np::npregbw(formula = Y ~ X, bwtype = "fixed", regtype = "ll")
krec <- np::npreg(bwc, exdat = xGrid)

# Variable bandwidths
bwg <- np::npregbw(formula = Y ~ X, bwtype = "generalized_nn", regtype = "ll")
kreg <- np::npreg(bwg, exdat = xGrid)
bwa <- np::npregbw(formula = Y ~ X, bwtype = "adaptive_nn", regtype = "ll")
krea <- np::npreg(bwa, exdat = xGrid)

# Comparison
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(krec$eval$xGrid, krec$mean, col = 2)
lines(kreg$eval$xGrid, kreg$mean, col = 3)
lines(krea$eval$xGrid, krea$mean, col = 4)
legend("top", legend = c("True regression", "Fixed", "Generalized NN",
                         "Adaptive NN"),
       lwd = 2, col = 1:4)
# Observe how the fixed bandwidth may yield a fit that produces serious
# artifacts in the low density region. At that region the NN-based bandwidths
# expand to borrow strenght from the points in the high density regions,
# whereas in the high density regions they shrink to adapt faster to the
# changes of the regression function

## ---- mult-2, fig.margin = FALSE-----------------------------------------
# Employing the wine dataset
# wine <- read.table(file = "wine.csv", header = TRUE, sep = ",")

# Bandwidth by CV for local linear estimator - a product kernel with 4 bandwidths
# Employs 4 random starts for minimizing the CV surface
out <- capture.output(
  bwWine <- np::npregbw(formula = Price ~ Age + WinterRain + AGST + HarvestRain,
                        data = wine, regtype = "ll")
  )
bwWine
# capture.output() to remove the multistart messages

# Regression
fitWine <- np::npreg(bwWine)
summary(fitWine)

# Plot marginal effects of each predictor on the response
plot(fitWine, )
# Therefore:
# - Age is positively related with Price (almost linearly)
# - WinterRain is positively related with Price (with a subtle nonlinearity)
# - AGST is positively related with Price, but now we see what it looks like a
#   quadratic pattern
# - HarvestRain is negatively related with Price (almost linearly)

## ---- mult-3, fig.margin = FALSE, fig.asp = 1/2--------------------------
# Bandwidth by CV for local linear estimator
# Recall that Species is a factor!
out <- capture.output(
  bwIris <- np::npregbw(formula = Petal.Length ~ Sepal.Width + Species,
                        data = iris, regtype = "ll")
)
bwIris
# Product kernel with 2 bandwidths

# Regression
fitIris <- np::npreg(bwIris)
summary(fitIris)

# Plot marginal effects of each predictor on the response
par(mfrow = c(1, 2))
plot(fitIris, plot.par.mfrow = FALSE)
# Options for the plot method for np::npreg available at ?np::npplot

## ---- mult-4, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3----
# Example from ?np::npreg: modeling of the GDP growth of a country from
# economic indicators of the country
# The predictors contain a mix of unordered, ordered, and continuous variables

# Load data
data(oecdpanel, package = "np")

# Bandwidth by CV for local constant - use only two starts to reduce the
# computation time
out <- capture.output(
  bwOECD <- np::npregbw(formula = growth ~ factor(oecd) + ordered(year) +
                          initgdp + popgro + inv + humancap, data = oecdpanel,
                        regtype = "lc", nmulti = 2)
)
bwOECD

# Regression
fitOECD <- np::npreg(bwOECD)
summary(fitOECD)

# Plot marginal effects of each predictor on the response
par(mfrow = c(2, 3))
plot(fitOECD, plot.par.mfrow = FALSE)

## ---- predci, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3----
# Asymptotic confidence bands for the marginal effects of each predictor on the
# response
par(mfrow = c(2, 3))
plot(fitOECD, plot.errors.method = "asymptotic", common.scale = FALSE,
     plot.par.mfrow = FALSE)

# Bootstrap confidence bands
# They take more time to compute because a resampling + refitting takes place
par(mfrow = c(2, 3))
plot(fitOECD, plot.errors.method = "bootstrap", plot.par.mfrow = FALSE)

# The asymptotic standard error associated to the regression evaluated at the
# evaluation points are in $merr
head(fitOECD$merr)

# Recall that in $mean we had the regression evaluated at the evaluation points,
# by default the sample of the predictors, so in this case the same as the
# fitted values
head(fitOECD$mean)

# Prediction for the first 3 points + standard errors
pred <- predict(fitOECD, newdata = oecdpanel[1:3, ], se.fit = TRUE)

# Approximate (based on assuming asymptotic normality) 100(1 - alpha)% CI for
# the conditional mean of the first 3 points
alpha <- 0.05
pred$fit + (qnorm(1 - alpha / 2) * pred$se.fit) %o% c(-1, 1)

## ---- ll-1, eval = TRUE--------------------------------------------------
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

# Approach 1: optimize the weighted log-likelihood through the workhorse
# function underneath glm, glm.fit
suppressWarnings(
  fitGlm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    glm.fit(x = cbind(1, X - x), y = Y, weights = K,
            family = binomial())$coefficients[1]
  })
)

# Approach 2: optimize directly the weighted log-likelihood
suppressWarnings(
  fitNlm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    nlm(f = function(beta) {
      -sum(K * (Y * (beta[1] + beta[2] * (X - x)) -
                  log(1 + exp(beta[1] + beta[2] * (X - x)))))
      }, p = c(0, 0))$estimate[1]
  })
)

# Approach 3: employ locfit::locfit
# Bandwidth can not be controlled explicitly - only through nn in ?lp
fitLocfit <- locfit::locfit(Y ~ locfit::lp(X, deg = 1, nn = h),
                            family = "binomial", kern = "gauss")

# Compare fits
plot(x, p(x), ylim = c(0, 1.5), type = "l", lwd = 2)
lines(x, logistic(fitGlm), col = 2)
lines(x, logistic(fitNlm), col = 2, lty = 2)
plot(fitLocfit, add = TRUE, col = 4)
legend("topright", legend = c("p(x)", "glm", "nlm", "locfit"), lwd = 2,
       col = c(1, 2, 2, 4), lty = c(1, 2, 1, 1))

## ---- ll-2, eval = TRUE--------------------------------------------------
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

