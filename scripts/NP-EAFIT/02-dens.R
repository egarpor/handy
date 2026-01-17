
## -----------------------------------------------------------------------------
## Name: 02-dens.R
## Description: Script for Chapter 2 of "A Short Course on Nonparametric Curve Estimation"
## Link: https://egarpor.github.io/NP-EAFIT
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## -----------------------------------------------------------------------------

## ----hist-1-------------------------------------------------------------------
# The faithful dataset is included in R
head(faithful)

# Duration of eruption
faithE <- faithful$eruptions

# Default histogram: automatically chosen bins and absolute frequencies!
histo <- hist(faithE)

# List that contains several objects
str(histo)

# With relative frequencies
hist(faithE, probability = TRUE)

# Choosing the breaks
# t0 = min(faithE), h = 0.25
Bk <- seq(min(faithE), max(faithE), by = 0.25)
hist(faithE, probability = TRUE, breaks = Bk)
rug(faithE) # Plotting the sample


## ----hist-2-------------------------------------------------------------------
# Uniform sample
set.seed(1234567)
u <- runif(n = 100)

# t0 = 0, h = 0.2
Bk1 <- seq(0, 1, by = 0.2)

# t0 = -0.1, h = 0.2
Bk2 <- seq(-0.1, 1.1, by = 0.2)

# Comparison
hist(u, probability = TRUE, breaks = Bk1, ylim = c(0, 1.5),
     main = "t0 = 0, h = 0.2")
rug(u)
abline(h = 1, col = 2)
hist(u, probability = TRUE, breaks = Bk2, ylim = c(0, 1.5),
     main = "t0 = -0.1, h = 0.2")
rug(u)
abline(h = 1, col = 2)


## ----hist-3-------------------------------------------------------------------
# Sample 100 points from a N(0, 1) and 50 from a N(3, 0.25)
set.seed(1234567)
samp <- c(rnorm(n = 100, mean = 0, sd = 1),
          rnorm(n = 50, mean = 3.25, sd = sqrt(0.5)))

# min and max for choosing Bk1 and Bk2
range(samp)

# Comparison
Bk1 <- seq(-2.5, 5, by = 0.5)
Bk2 <- seq(-2.25, 5.25, by = 0.5)
hist(samp, probability = TRUE, breaks = Bk1, ylim = c(0, 0.5),
     main = "t0 = -2.5, h = 0.5")
rug(samp)
hist(samp, probability = TRUE, breaks = Bk2, ylim = c(0, 0.5),
     main = "t0 = -2.25, h = 0.5")
rug(samp)






## ----kde-1--------------------------------------------------------------------
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




## ----rt-----------------------------------------------------------------------
# Data
x <- rnorm(100)

# Rule-of-thumb
bw.nrd(x = x)

# Same as
iqr <- diff(quantile(x, c(0.75, 0.25))) / diff(qnorm(c(0.75, 0.25)))
1.06 * length(x)^(-1/5) * min(sd(x), iqr)


## ----pi-----------------------------------------------------------------------
# Data
x <- rnorm(100)

# Rule-of-thumb
bw.SJ(x = x, method = "dpi")

# Similar to
library(ks)
hpi(x) # Default is two-stages


## ----hlscv--------------------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# UCV gives a warning
bw.ucv(x = x)

# Extend search interval
args(bw.ucv)
bw.ucv(x = x, lower = 0.01, upper = 1)

# bw.ucv.mod replaces the optimization routine of bw.ucv by an exhaustive search on
# "h.grid" (chosen adaptatively from the sample) and optionally plots the LSCV curve
# with "plot.cv"
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
  # h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
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


## ----bcv----------------------------------------------------------------------
# Data
set.seed(123456)
x <- rnorm(100)

# BCV gives a warning
bw.bcv(x = x)

# Extend search interval
args(bw.bcv)
bw.bcv(x = x, lower = 0.01, upper = 1)

# bw.bcv.mod replaces the optimization routine of bw.bcv by an exhaustive search on
# "h.grid" (chosen adaptatively from the sample) and optionally plots the BCV curve
# with "plot.cv"
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
  # h <- optimize(fbcv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
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


## ----nor1mix------------------------------------------------------------------
# Load package
library(nor1mix)

# Available models
# ?MarronWand

# Simulating
samp <- rnorMix(n = 500, obj = MW.nm9) # MW object in the second argument
hist(samp, freq = FALSE)

# Density evaluation
x <- seq(-4, 4, length.out = 400)
lines(x, dnorMix(x = x, obj = MW.nm9), col = 2)

# Plot a MW object directly
# A normal with the same mean and variance is plotted in dashed lines
par(mfrow = c(2, 2))
plot(MW.nm5)
plot(MW.nm7)
plot(MW.nm10)
plot(MW.nm12)
lines(MW.nm10) # Also possible




## ----ci-1---------------------------------------------------------------------
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
hatVarKde <- kde$y * Rk / (n * h)

# True expectation and variance (because the density is a normal)
EKh <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
varKde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
             (2 * sqrt(pi) * h) - EKh^2) / n

# CI with estimated variance
alpha <- 0.05
zalpha <- qnorm(1 - alpha/2)
ciLow1 <- kde$y - zalpha * sqrt(hatVarKde)
ciUp1 <- kde$y + zalpha * sqrt(hatVarKde)

# CI with known variance
ciLow2 <- kde$y - zalpha * sqrt(varKde)
ciUp2 <- kde$y + zalpha * sqrt(varKde)

# Plot estimate, CIs and expectation
plot(kde, main = "Density and CIs", ylim = c(0, 1))
lines(kde$x, ciLow1, col = "gray")
lines(kde$x, ciUp1, col = "gray")
lines(kde$x, ciUp2, col = "gray", lty = 2)
lines(kde$x, ciLow2, col = "gray", lty = 2)
lines(kde$x, EKh, col = "red")
legend("topright", legend = c("Estimate", "CI estimated var",
                              "CI known var", "Smoothed density"),
       col = c("black", "gray", "gray", "red"), lwd = 2, lty = c(1, 1, 2, 1))


## ----ci-2---------------------------------------------------------------------
# Simulation setting
n <- 100; h <- 0.2
mu <- 0; sigma <- 1 # Normal parameters
M <- 5e2 # Number of replications in the simulation
nGrid <- 512 # Number of x's for computing the kde
alpha <- 0.05; zalpha <- qnorm(1 - alpha/2) # alpha for CI

# Compute expectation and variance
kde <- density(x = 0, bw = h, from = -4, to = 4, n = nGrid) # Just to get kde$x
EKh <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
varKde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
           (2 * sqrt(pi) * h) - EKh^2) / n

# For storing if the mean is inside the CI
insideCi1 <- insideCi2 <- matrix(nrow = M, ncol = nGrid)

# Simulation
set.seed(12345)
for (i in 1:M) {

  # Sample & kde
  x <- rnorm(n = n, mean = mu, sd = sigma)
  kde <- density(x = x, bw = h, from = -4, to = 4, n = nGrid)
  hatSdKde <- sqrt(kde$y * Rk / (n * h))

  # CI with estimated variance
  ciLow1 <- kde$y - zalpha * hatSdKde
  ciUp1 <- kde$y + zalpha * hatSdKde

  # CI with known variance
  ciLow2 <- kde$y - zalpha * sqrt(varKde)
  ciUp2 <- kde$y + zalpha * sqrt(varKde)

  # Check if for each x the mean is inside the CI
  insideCi1[i, ] <- EKh > ciLow1 & EKh < ciUp1
  insideCi2[i, ] <- EKh > ciLow2 & EKh < ciUp2

}

# Plot results
plot(kde$x, colMeans(insideCi1), ylim = c(0.25, 1), type = "l",
     main = "Coverage CIs", xlab = "x", ylab = "Coverage")
lines(kde$x, colMeans(insideCi2), col = 4)
abline(h = 1 - alpha, col = 2)
abline(h = 1 - alpha + c(-1, 1) * qnorm(0.975) *
         sqrt(alpha * (1 - alpha) / M), col = 2, lty = 2)
legend(x = "bottom", legend = c("CI estimated var", "CI known var",
                                "Nominal level",
                                "95% CI for the nominal level"),
       col = c(1, 4, 2, 2), lwd = 2, lty = c(1, 1, 1, 2))


## ----manipulate-1, eval = FALSE-----------------------------------------------
# # Load manipulate
# # install.packages("manipulate")
# library(manipulate)
# 
# # Sample
# x <- rnorm(100)
# 
# # Simple plot of kde for varying h
# manipulate({
# 
#   kde <- density(x = x, from = -4, to = 4, bw = h)
#   plot(kde, ylim = c(0, 1), type = "l", main = "")
#   curve(dnorm(x), from = -4, to = 4, col = 2, add = TRUE)
#   rug(x)
# 
# }, h = slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))


## ----transf-1-----------------------------------------------------------------
# Sample from a LN(0, 1)
set.seed(123456)
samp <- rlnorm(n = 500)

# kde and density
plot(density(samp), ylim = c(0, 1))
curve(dlnorm(x), from = -2, to = 10, col = 2, add = TRUE)
rug(samp)




## ----transf-2-----------------------------------------------------------------
# kde with log-transformed data
kde <- density(log(samp))
plot(kde, main = "kde of transformed data")
rug(log(samp))

# Careful: kde$x is in the reals!
range(kde$x)

# Untransform kde$x so the grid is in (0, infty)
kdeTransf <- kde
kdeTransf$x <- exp(kdeTransf$x)

# Transform the density using the chain rule
kdeTransf$y <- kdeTransf$y * 1 / kdeTransf$x

# Transformed kde
plot(kdeTransf, main = "Transformed kde")
curve(dlnorm(x), col = 2, add = TRUE)
rug(samp)


## ----samp---------------------------------------------------------------------
# Sample the Claw
n <- 100
set.seed(23456)
samp <- rnorMix(n = n, obj = MW.nm10)

# Kde
h <- 0.1
plot(density(samp, bw = h))

# Naive sampling algorithm
sampKde <- numeric(1e6)
for (k in 1:1e6) {

  i <- sample(x = 1:100, size = 1)
  sampKde[k] <- rnorm(n = 1, mean = samp[i], sd = h)

}

# Add kde of the sampled kde - almost equal
lines(density(sampKde), col = 2)

# Sample 1e6 points from the kde
i <- sample(x = 100, size = 1e6, replace = TRUE)
sampKde <- rnorm(1e6, mean = samp[i], sd = h)

# Add kde of the sampled kde - almost equal
lines(density(sampKde), col = 3)


