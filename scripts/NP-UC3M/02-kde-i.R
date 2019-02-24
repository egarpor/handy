
## ------------------------------------------------------------------------
## Name: 02-kde-i.R
## Description: Script for Chapter 2 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- hist-1-------------------------------------------------------------
# The faithful dataset is included in R
head(faithful)

# Duration of eruption
faith_eruptions <- faithful$eruptions

# Default histogram: automatically chosen bins and absolute frequencies!
histo <- hist(faith_eruptions)

# List that contains several objects
str(histo)

# With relative frequencies
hist(faith_eruptions, probability = TRUE)

# Choosing the breaks
# t0 = min(faithE), h = 0.25
Bk <- seq(min(faith_eruptions), max(faith_eruptions), by = 0.25)
hist(faith_eruptions, probability = TRUE, breaks = Bk)
rug(faith_eruptions) # Plotting the sample

## ---- hist-2, fig.cap = '(ref:hist-2-title)', fig.show = 'hold'----------
# Sample from a U(0, 1)
set.seed(1234567)
u <- runif(n = 100)

# Bins for t0 = 0, h = 0.2
Bk1 <- seq(0, 1, by = 0.2)

# Bins for t0 = -0.1, h = 0.2
Bk2 <- seq(-0.1, 1.1, by = 0.2)

# Comparison of histograms for different t0's
hist(u, probability = TRUE, breaks = Bk1, ylim = c(0, 1.5),
     main = "t0 = 0, h = 0.2")
rug(u)
abline(h = 1, col = 2)
hist(u, probability = TRUE, breaks = Bk2, ylim = c(0, 1.5),
     main = "t0 = -0.1, h = 0.2")
rug(u)
abline(h = 1, col = 2)

## ---- hist-3, fig.cap = '(ref:hist-3-title)', fig.show = 'hold'----------
# Sample 75 points from a N(0, 1) and 50 from a N(3, 0.25)
set.seed(1234567)
samp <- c(rnorm(n = 50, mean = 0, sd = 1),
          rnorm(n = 25, mean = 3.25, sd = sqrt(0.5)))

# min and max for choosing Bk1 and Bk2
range(samp)

# Comparison
Bk1 <- seq(-2.5, 5, by = 0.5)
Bk2 <- seq(-2.25, 5.25, by = 0.5)
hist(samp, probability = TRUE, breaks = Bk1, ylim = c(0, 0.5),
     main = "t0 = -2.5, h = 0.5")
curve(2/3 * dnorm(x, mean = 0, sd = 1) + 
        1/3 * dnorm(x, mean = 3.25, sd = sqrt(0.5)), col = 2, add = TRUE, 
      n = 200)
rug(samp)
hist(samp, probability = TRUE, breaks = Bk2, ylim = c(0, 0.5),
     main = "t0 = -2.25, h = 0.5")
curve(2/3 * dnorm(x, mean = 0, sd = 1) + 
        1/3 * dnorm(x, mean = 3.25, sd = sqrt(0.5)), col = 2, add = TRUE, 
      n = 200)
rug(samp)

## ---- kernels, echo = FALSE, fig.cap = '(ref:kernels-title)'-------------
# Extracting the implemented kernels
kernels <- eval(formals(density.default)$kernel)

# Kernels in the R parametrization
plot(density(0, bw = 1, n = 1e3), xlab = "", main = "", ylim = c(0, 0.5))
sapply(2:length(kernels), function(i) 
  lines(density(0, bw = 1, kernel =  kernels[i], n = 1e3), col = i)) -> s
legend("topright", legend = kernels, col = seq(kernels), lwd = 2)

## ---- kde-1--------------------------------------------------------------
# Sample 100 points from a N(0, 1)
set.seed(1234567)
samp <- rnorm(n = 100, mean = 0, sd = 1)

# Quickly compute a kde and plot the density object
# Automatically chooses bandwidth and uses normal kernel
plot(density(x = samp))

# Select a particular bandwidth (0.5) and kernel (Epanechnikov)
lines(density(x = samp, bw = 0.5, kernel = "epanechnikov"), col = 2)

# density automatically chooses the interval for plotting the kde
# (observe that the black line goes to roughly between -3 and 3)
# This can be tuned using "from" and "to"
plot(density(x = samp, from = -4, to = 4), xlim = c(-5, 5))

# The density object is a list
kde <- density(x = samp, from = -5, to = 5, n = 1024)
str(kde)
# Note that the evaluation grid "x"" is not directly controlled, only through
# "from, "to", and "n" (better use powers of 2)
plot(kde$x, kde$y, type = "l")
curve(dnorm(x), col = 2, add = TRUE) # True density
rug(samp)

## ---- R-kernels----------------------------------------------------------
# Implementation of the Epanechnikov based on the theory
K_Epa <- function(z, h = 1) 3 / (4 * h) * (1 - (z / h)^2) * (abs(z) < h)
mu2_K_Epa <- integrate(function(z) z^2 * K_Epa(z), lower = -1, upper = 1)$value

# Epanechnikov kernel by R
h <- 0.5
plot(density(0, kernel = "epanechnikov", bw = h))

# Build the equivalent bandwidth
h_tilde <- h / sqrt(mu2_K_Epa)
curve(K_Epa(x, h = h_tilde), add = TRUE, col = 2)

# The other way around
h_tilde <- 2
h <- h_tilde * sqrt(mu2_K_Epa)
curve(K_Epa(x, h = h_tilde), from = -3, to = 3, col = 2)
lines(density(0, kernel = "epanechnikov", bw = h))

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
                       h_grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot_cv = FALSE) {
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
  obj <- sapply(h_grid, function(h) fucv(h))
  h <- h_grid[which.min(obj)]
  if (plot_cv) {
    plot(h_grid, obj, type = "o")
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the LSCV curve
bw.ucv.mod(x = x, plot_cv = TRUE)

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
                       h_grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot_cv = FALSE) {
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
  obj <- sapply(h_grid, function(h) fbcv(h))
  h <- h_grid[which.min(obj)]
  if (plot_cv) {
    plot(h_grid, obj, type = "o")
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the BCV curve
bw.bcv.mod(x = x, plot_cv = TRUE)

# We can compare with the default bw.bcv output
abline(v = bw.bcv(x = x), col = 3)

## ---- nor1mix------------------------------------------------------------
# Available models
?nor1mix::MarronWand

# Simulating
samp <- nor1mix::rnorMix(n = 500, obj = nor1mix::MW.nm9) 
# MW object in the second argument
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
lines(nor1mix::MW.nm10) # Also possible

## Implement the $h_\mathrm{MISE}$ using \@ref(eq:misenorm) for model `nor1mix::MW.nm5`. Then, investigate by simulation the distributions of $\hat h_\mathrm{DPI}/h_\mathrm{MISE}-1$, $\hat h_\mathrm{LSCV}/h_\mathrm{MISE}-1$, and $\hat h_\mathrm{BCV}/h_\mathrm{MISE}-1$.

## ---- ci-1, fig.cap = '(ref:ci-1-title)', fig.margin = FALSE-------------
# R(K) for a normal
Rk <- 1 / (2 * sqrt(pi))

# Generate a sample from a N(mu, sigma^2)
n <- 100
mu <- 0
sigma <- 1
set.seed(123456)
x <- rnorm(n = n, mean = mu, sd = sigma)

# Compute the kde (NR bandwidth)
kde <- density(x = x, from = -4, to = 4, n = 1024, bw = "nrd")

# Selected bandwidth
h <- kde$bw

# Estimate the variance
var_kde_hat <- kde$y * Rk / (n * h)

# True expectation and variance (because the density is a normal)
E_kde <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
var_kde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
              (2 * sqrt(pi) * h) - E_kde^2) / n

# CI with estimated variance
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
ci_low_1 <- kde$y - z_alpha * sqrt(var_kde_hat)
ci_up_1 <- kde$y + z_alpha * sqrt(var_kde_hat)

# CI with known variance
ci_low_2 <- kde$y - z_alpha * sqrt(var_kde)
ci_up_2 <- kde$y + z_alpha * sqrt(var_kde)

# Plot estimate, CIs and expectation
plot(kde, main = "Density and CIs", ylim = c(0, 1))
lines(kde$x, ci_low_1, col = "gray")
lines(kde$x, ci_up_1, col = "gray")
lines(kde$x, ci_low_2, col = "gray", lty = 2)
lines(kde$x, ci_up_2, col = "gray", lty = 2)
lines(kde$x, E_kde, col = "red")
legend("topright", legend = c("Estimate", "CI estimated var",
                              "CI known var", "Smoothed density"),
       col = c("black", "gray", "gray", "red"), lwd = 2, lty = c(1, 1, 2, 1))

## ---- ci-2, fig.cap = '(ref:ci-2-title)', fig.margin = FALSE-------------
# Simulation setting
n <- 200; h <- 0.15
mu <- 0; sigma <- 1 # Normal parameters
M <- 5e2 # Number of replications in the simulation
n_grid <- 512 # Number of x's for computing the kde
alpha <- 0.05; z_alpha <- qnorm(1 - alpha/2) # alpha for CI

# Compute expectation and variance
kde <- density(x = 0, bw = h, from = -4, to = 4, n = n_grid) # Just for kde$x
E_kde <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
var_kde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
              (2 * sqrt(pi) * h) - E_kde^2) / n

# For storing if the mean is inside the CI
inside_ci_1 <- inside_ci_2 <- matrix(nrow = M, ncol = n_grid)

# Simulation
set.seed(12345)
for (i in 1:M) {

  # Sample & kde
  x <- rnorm(n = n, mean = mu, sd = sigma)
  kde <- density(x = x, bw = h, from = -4, to = 4, n = n_grid)
  sd_kde_hat <- sqrt(kde$y * Rk / (n * h))

  # CI with estimated variance
  ci_low_1 <- kde$y - z_alpha * sd_kde_hat
  ci_up_1 <- kde$y + z_alpha * sd_kde_hat

  # CI with known variance
  ci_low_2 <- kde$y - z_alpha * sqrt(var_kde)
  ci_up_2 <- kde$y + z_alpha * sqrt(var_kde)

  # Check if for each x the mean is inside the CI
  inside_ci_1[i, ] <- E_kde > ci_low_1 & E_kde < ci_up_1
  inside_ci_2[i, ] <- E_kde > ci_low_2 & E_kde < ci_up_2

}

# Plot results
plot(kde$x, colMeans(inside_ci_1), ylim = c(0.25, 1), type = "l",
     main = "Empirical coverage of CIs", xlab = "x", ylab = "Coverage")
lines(kde$x, colMeans(inside_ci_2), col = 4)
abline(h = 1 - alpha, col = 2)
abline(h = 1 - alpha + c(-1, 1) * qnorm(0.975) *
         sqrt(alpha * (1 - alpha) / M), col = 2, lty = 2)
legend(x = "bottom", legend = c("CI estimated var", "CI known var",
                                "Nominal level", 
                                "95% CI for the nominal level"),
       col = c(1, 4, 2, 2), lwd = 2, lty = c(1, 1, 1, 2))

## ---- manipulate-1, eval = FALSE-----------------------------------------
## # Sample
## x <- rnorm(100)
## 
## # Simple plot of kde for varying h
## manipulate::manipulate({
## 
##   kde <- density(x = x, from = -4, to = 4, bw = h)
##   plot(kde, ylim = c(0, 1), type = "l", main = "")
##   curve(dnorm(x), from = -4, to = 4, col = 2, add = TRUE)
##   rug(x)
## 
## }, h = manipulate::slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))

## ---- transf-1-----------------------------------------------------------
# Sample from a LN(0, 1)
set.seed(123456)
samp <- rlnorm(n = 500)

# kde and density
plot(density(samp), ylim = c(0, 0.8))
curve(dlnorm(x), from = -2, to = 10, n = 500, col = 2, add = TRUE)
rug(samp)

## ---- transf-2-----------------------------------------------------------
# kde with log-transformed data
kde <- density(log(samp))
plot(kde, main = "Kde of transformed data")
rug(log(samp))

# Careful: kde$x is in the reals!
range(kde$x)

# Untransform kde$x so the grid is in (0, infty)
kde_transf <- kde
kde_transf$x <- exp(kde_transf$x)

# Transform the density using the chain rule
kde_transf$y <- kde_transf$y * 1 / kde_transf$x

# Transformed kde
plot(kde_transf, main = "Transformed kde", xlim = c(0, 15))
curve(dlnorm(x), col = 2, add = TRUE, n = 500)
rug(samp)

## Consider the data given by `set.seed(12345); x <- rbeta(n = 500, shape1 = 2, shape2 = 2)`. Compute:

## ---- samp---------------------------------------------------------------
# Sample the Claw
n <- 100
set.seed(23456)
samp <- nor1mix::rnorMix(n = n, obj = nor1mix::MW.nm10)

# Kde
h <- 0.1
plot(density(samp, bw = h), main = "", col = 4)

# # Naive sampling algorithm
# N <- 1e6
# samp_kde <- numeric(N)
# for (k in 1:N) {
# 
#   i <- sample(x = 1:n, size = 1)
#   samp_kde[k] <- rnorm(n = 1, mean = samp[i], sd = h)
# 
# }

# Sample N points from the kde
N <- 1e6
i <- sample(x = n, size = N, replace = TRUE)
samp_kde <- rnorm(N, mean = samp[i], sd = h)

# Add kde of the sampled kde - almost equal
lines(density(samp_kde), col = 3)
legend("topright", legend = c("Kde", "Kde of sampled kde"), 
       lwd = 2, col = 4:3)

## Sample data points from the kde of `iris$Petal.Width` that is computed with the NS selector.

## The dataset `sunspot.year` contains the yearly numbers of sunspots from 1700 to 1988 (rounded to one digit). Employing a log-transformed kde with DPI bandwidth, sample new sunspots observations. *Beware*: recall the log-transformation before sampling.

## ---- kde-eval-1---------------------------------------------------------
# Sample
n <- 25
samp_t <- rt(n, df = 2)

# Comparison: same output and same parametrization for bandwidth
bw <- 0.75
plot(kde <- ks::kde(x = samp_t, h = bw), lwd = 3) # ?ks::plot.kde for options
lines(density(x = samp_t, bw = bw), col = 2)
# Beware: there is no lines() method for ks::kde objects

# The default h is the DPI obtained by ks::hpi
kde <- ks::kde(x = samp_t)

# Manual plot - recall $eval.points and $estimate
lines(kde$eval.points, kde$estimate, col = 4)

# Evaluating the kde at specific points, e.g., the first 5 sample points
ks::kde(x = samp_t, h = bw, eval.points = samp_t[1:5])

# By default ks::kde() computes the *binned* kde (much faster) and then employs 
# an interpolation to evaluate the kde at the given grid; if the exact kde is 
# desired, this can be specified with binned = FALSE
ks::kde(x = samp_t, h = bw, eval.points = samp_t[1:5], binned = FALSE)

# Changing the size of the evaluation grid
length(ks::kde(x = samp_t, h = bw, gridsize = 1e3)$estimate)

## ---- kde-eval-2---------------------------------------------------------
# Sample from a LN(0, 1)
set.seed(123456)
samp_ln <- rlnorm(n = 200)

# Log-kde without shifting
a <- seq(0.1, 2, by = 0.4) # Sequence of shiftings
col <- viridis::viridis(length(a) + 1)
plot(ks::kde(x = samp_ln, positive = TRUE), col = col[1],
     main = "Log-transformed kde and the effect of adj.positive", 
     xlim = c(0, 7.5), ylim = c(0, 0.75))
# If h is not provided, then ks::hpi() is called on the transformed data

# Shiftings: larger a increases the bias
for (i in seq_along(a)) {
  plot(ks::kde(x = samp_ln, positive = TRUE, adj.positive = a[i]), 
       add = TRUE, col = col[i])
}
curve(dlnorm(x), col = 2, add = TRUE, n = 500)
rug(samp_ln)
legend("topright", legend = c("True density", paste("adj.positive =", c(0, a))), 
       col = c(2, col), lwd = 2)

## ---- kde-eval-3---------------------------------------------------------
# Untransformed kde
plot(kde <- ks::kde(x = log(samp_ln)), col = 4)
samp_kde <- ks::rkde(n = 5e4, fhat = kde)
plot(ks::kde(x = samp_kde), add = TRUE, col = 3)
legend("topright", legend = c("Kde", "Kde of sampled kde"), 
       lwd = 2, col = 3:4)

# Transformed kde
plot(kde_transf <- ks::kde(x = samp_ln, positive = TRUE), col = 4)
samp_kde_transf <- ks::rkde(n = 5e4, fhat = kde_transf, positive = TRUE)
plot(ks::kde(x = samp_kde_transf), add = TRUE, col = 3)
legend("topright", legend = c("Kde", "Kde of sampled kde"), 
       lwd = 2, col = 3:4)
