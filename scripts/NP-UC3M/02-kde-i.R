
## ----------------------------------------------------------------------------
## Name: 02-kde-i.R
## Description: Script for Chapter 2 of "Notes for Nonparametric Statistics"
## Link: https://egarpor.github.io/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 6.13.0
## ----------------------------------------------------------------------------

## ----hist-1-----------------------------------------------------------------------------------------------------------
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

## ----hist-2, fig.cap = '(ref:hist-2-title)', fig.show = 'hold'--------------------------------------------------------
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

## ----hist-3, fig.cap = '(ref:hist-3-title)', fig.show = 'hold'--------------------------------------------------------
# Sample 50 points from a N(0, 1) and 25 from a N(3.25, 0.5)
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

## ----kde-1------------------------------------------------------------------------------------------------------------
# Sample 100 points from a N(0, 1)
set.seed(1234567)
samp <- rnorm(n = 100, mean = 0, sd = 1)

# Quickly compute a kde and plot the density object
# Automatically chooses bandwidth and uses normal kernel
plot(density(x = samp))

# Select a particular bandwidth (0.5) and kernel (Epanechnikov)
lines(density(x = samp, bw = 0.5, kernel = "epanechnikov"), col = 2)

# density() automatically chooses the interval for plotting the kde
# (observe that the black line goes to roughly between -3 and 3)
# This can be tuned using "from" and "to"
plot(density(x = samp, from = -4, to = 4), xlim = c(-5, 5))

# The density object is a list
kde <- density(x = samp, from = -5, to = 5, n = 1024)
str(kde)
# Note that the evaluation grid "x" is not directly controlled, only through
# "from, "to", and "n" (better use powers of 2)
plot(kde$x, kde$y, type = "l")
curve(dnorm(x), col = 2, add = TRUE) # True density
rug(samp)

## ----R-kernels--------------------------------------------------------------------------------------------------------
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

## ----bwnrd------------------------------------------------------------------------------------------------------------
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

## ----SJ---------------------------------------------------------------------------------------------------------------
# Data
set.seed(672641)
x <- rnorm(100)

# DPI selector
bw.SJ(x = x, method = "dpi")

# Similar to
ks::hpi(x) # Default is two-stage

## ----bw-ucv-mod, fig.cap = '(ref:ucv-title)'--------------------------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# UCV gives a warning
bw.ucv(x = x)

# Extend search interval
bw.ucv(x = x, lower = 0.01, upper = 1)

# bw.ucv.mod replaces the optimization routine of bw.ucv with an exhaustive
# search on "h_grid" (chosen adaptatively from the sample) and optionally
# plots the LSCV curve with "plot_cv"
bw.ucv.mod <- function(x, nb = 1000L,
                       h_grid = 10^seq(-3, log10(1.2 * sd(x) *
                                                   length(x)^(-1/5)), l = 200),
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
  if (h %in% range(h_grid))
    warning("minimum occurred at one end of h_grid")
  if (plot_cv) {
    plot(h_grid, obj, type = "o")
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the LSCV curve
bw.ucv.mod(x = x, plot_cv = TRUE, h_grid = 10^seq(-1.25, 0.5, l = 200))

# We can compare with the default bw.ucv output
abline(v = bw.ucv(x = x), col = 3)

## ----bw-bcv-mod, fig.cap = '(ref:bcv-title)'--------------------------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# BCV gives a warning
bw.bcv(x = x)

# Extend search interval
args(bw.bcv)
bw.bcv(x = x, lower = 0.01, upper = 1)

# bw.bcv.mod replaces the optimization routine of bw.bcv with an exhaustive
# search on "h_grid" (chosen adaptatively from the sample) and optionally
# plots the BCV curve with "plot_cv"
bw.bcv.mod <- function(x, nb = 1000L,
                       h_grid = 10^seq(-3, log10(1.2 * sd(x) *
                                                   length(x)^(-1/5)), l = 200),
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
  if (h %in% range(h_grid))
    warning("minimum occurred at one end of h_grid")
  if (plot_cv) {
    plot(h_grid, obj, type = "o")
    rug(h_grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

# Compute the bandwidth and plot the BCV curve
bw.bcv.mod(x = x, plot_cv = TRUE, h_grid = 10^seq(-1.25, 0.5, l = 200))

# We can compare with the default bw.bcv output
abline(v = bw.bcv(x = x), col = 3)

## ----nor1mix----------------------------------------------------------------------------------------------------------
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
# A normal with the same mean and variance is plotted in dashed lines --
# you can remove it with argument p.norm = FALSE
par(mfrow = c(2, 2))
plot(nor1mix::MW.nm5)
plot(nor1mix::MW.nm7)
plot(nor1mix::MW.nm10)
plot(nor1mix::MW.nm12)
lines(nor1mix::MW.nm7, col = 2) # Also possible

## ----transf-1---------------------------------------------------------------------------------------------------------
# Sample from a LN(0, 1)
set.seed(123456)
samp <- rlnorm(n = 500)

# kde and density
plot(density(samp), ylim = c(0, 0.8))
curve(dlnorm(x), from = -2, to = 10, n = 500, col = 2, add = TRUE)
rug(samp)

## ----transf-2---------------------------------------------------------------------------------------------------------
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

## ----samp-------------------------------------------------------------------------------------------------------------
# Sample the claw
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

# Add kde of the sampled kde -- almost equal
lines(density(samp_kde), col = 3)
legend("topright", legend = c("Kde", "Kde of sampled kde"),
       lwd = 2, col = 4:3)

## ----kde-eval-1-------------------------------------------------------------------------------------------------------
# Sample
n <- 5
set.seed(123456)
samp_t <- rt(n, df = 2)

# Comparison: same output and same parametrization for bandwidth
bw <- 0.75
plot(kde <- ks::kde(x = samp_t, h = bw), lwd = 3) # ?ks::plot.kde for options
lines(density(x = samp_t, bw = bw), col = 2)
# Beware: there is no lines() method for ks::kde objects

# The default h is the DPI obtained by ks::hpi
kde <- ks::kde(x = samp_t)

# Manual plot -- recall $eval.points and $estimate
lines(kde$eval.points, kde$estimate, col = 4)

# Evaluating the kde at specific points, e.g., the first 2 sample points
ks::kde(x = samp_t, h = bw, eval.points = samp_t[1:2])

# By default ks::kde() computes the *binned* kde (much faster) and then employs
# an interpolation to evaluate the kde at the given grid; if the exact kde is
# desired, this can be specified with binned = FALSE
ks::kde(x = samp_t, h = bw, eval.points = samp_t[1:2], binned = FALSE)

# Changing the size of the evaluation grid
length(ks::kde(x = samp_t, h = bw, gridsize = 1e3)$estimate)

## ----kde-eval-2-------------------------------------------------------------------------------------------------------
# Sample from a LN(0, 1)
set.seed(123456)
samp_ln <- rlnorm(n = 200)

# Log-kde without shifting
a <- seq(0.1, 2, by = 0.4) # Sequence of shifts
col <- viridis::viridis(length(a) + 1)
plot(ks::kde(x = samp_ln, positive = TRUE), col = col[1],
     main = "Log-transformed kde and the effect of adj.positive",
     xlim = c(0, 7.5), ylim = c(0, 0.75))
# If h is not provided, then ks::hpi() is called on the transformed data

# Shifting: larger a increases the bias
for (i in seq_along(a)) {
  plot(ks::kde(x = samp_ln, positive = TRUE, adj.positive = a[i]),
       add = TRUE, col = col[i + 1])
}
curve(dlnorm(x), col = 2, add = TRUE, n = 500)
rug(samp_ln)
legend("topright", legend = c("True density", paste("adj.positive =", c(0, a))),
       col = c(2, col), lwd = 2)

## ----kde-eval-3-------------------------------------------------------------------------------------------------------
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

