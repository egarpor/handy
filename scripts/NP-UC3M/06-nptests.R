
## ----------------------------------------------------------------------------
## Name: 06-nptests.R
## Description: Script for Chapter 6 of "Notes for Nonparametric Statistics"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 6.9.0
## ----------------------------------------------------------------------------

## ---- ks-1, fig.cap = '(ref:ks-1-title)'------------------------------------------------------------
# Sample data
n <- 10
mu0 <- 2
sd0 <- 1
set.seed(54321)
samp <- rnorm(n = n, mean = mu0, sd = sd0)

# Fn vs. F0
plot(ecdf(samp), main = "", ylab = "Probability", xlim = c(-1, 6),
     ylim = c(0, 1.3))
curve(pnorm(x, mean = mu0, sd = sd0), add = TRUE, col = 4)

# Add Dn+ and Dn-
samp_sorted <- sort(samp)
Ui <- pnorm(samp_sorted, mean = mu0, sd = sd0)
Dn_plus <- (1:n) / n - Ui
Dn_minus <- Ui - (1:n - 1) / n
i_plus <- which.max(Dn_plus)
i_minus <- which.max(Dn_minus)
lines(rep(samp_sorted[i_plus], 2), 
      c(i_plus / n, pnorm(samp_sorted[i_plus], mean = mu0, sd = sd0)), 
      col = 3, lwd = 2, pch = 16, type = "o", cex = 0.75)
lines(rep(samp_sorted[i_minus], 2), 
      c((i_minus - 1) / n, pnorm(samp_sorted[i_minus], mean = mu0, sd = sd0)), 
      col = 2, lwd = 2, pch = 16, type = "o", cex = 0.75)
rug(samp)
legend("topleft", lwd = 2, col = c(1, 4, 3, 2), 
       legend = latex2exp::TeX(c("$F_n$", "$F_0$", "$D_n^+$", "$D_n^-$")))

## ---- ks-2------------------------------------------------------------------------------------------
# Sample data from a N(0, 1)
n <- 50
set.seed(3245678)
x <- rnorm(n = n)

# Kolmogorov-Smirnov test for H_0: F = N(0, 1). Does not reject
(ks <- ks.test(x = x, y = "pnorm")) # In "y" we specify F0 as a function

# Structure of "htest" class
str(ks)

# Kolmogorov-Smirnov test for H_0: F = N(0.5, 1). Rejects
ks.test(x = x, y = "pnorm", mean = 0.5)

# Kolmogorov-Smirnov test for H_0: F = Exp(2). Strongly rejects
ks.test(x = x, y = "pexp", rate = 1/2)

## ---- ks-3------------------------------------------------------------------------------------------
# Sample data from a Pois(5)
n <- 100
set.seed(3245678)
x <- rpois(n = n, lambda = 5)

# Kolmogorov-Smirnov test for H_0: F = Pois(5) without specifying that the 
# distribution is discrete. Rejects (!?) giving a warning message
ks.test(x = x, y = "ppois", lambda = 5)

# We rely on dgof::ks.test, which works as stats::ks.test if the "y" argument 
# is not marked as a "stepfun" object, the way the dgof package codifies
# discrete distribution functions

# Step function containing the cdf of the Pois(5). The "x" stands for the 
# location of the steps and "y" for the value of the steps. "y" needs to have
# an extra point for the initial value of the function before the first step
x_eval <- 0:20
ppois_stepfun <- stepfun(x = x_eval, y = c(0, ppois(q = x_eval, lambda = 5)))
plot(ppois_stepfun, main = "Cdf of a Pois(5)")

# Kolmogorov-Smirnov test for H_0: F = Pois(5) adapted for discrete data,
# with data coming from a Pois(5)
dgof::ks.test(x = x, y = ppois_stepfun)

# If data is normally distributed, the test rejects H_0
dgof::ks.test(x = rnorm(n = n, mean = 5), y = ppois_stepfun)

## ---- ks-4, fig.margin = FALSE, fig.fullwidth = TRUE, fig.asp = 1/2, fig.cap = '(ref:ks-4-title)', out.width = .tex_web('80%', '100%')----
# Simulation of p-values when H_0 is true
set.seed(131231)
n <- 100
M <- 1e4
pvalues_H0 <- sapply(1:M, function(i) {
  x <- rnorm(n) # N(0, 1)
  ks.test(x, "pnorm")$p.value
})

# Simulation of p-values when H_0 is false -- the data does not
# come from a N(0, 1) but from a N(0, 2)
pvalues_H1 <- sapply(1:M, function(i) {
  x <- rnorm(n, mean = 0, sd = sqrt(2)) # N(0, 2)
  ks.test(x, "pnorm")$p.value
})

# Comparison of p-values
par(mfrow = 1:2)
hist(pvalues_H0, breaks = seq(0, 1, l = 20), probability = TRUE,
     main = latex2exp::TeX("$H_0$"), ylim = c(0, 4))
abline(h = 1, col = 2)
hist(pvalues_H1, breaks = seq(0, 1, l = 20), probability = TRUE,
     main = latex2exp::TeX("$H_1$"), ylim = c(0, 4))
abline(h = 1, col = 2)

## ---- cvm-1-----------------------------------------------------------------------------------------
# Sample data from a N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)

# Cramér-von Mises test for H_0: F = N(0, 1). Does not reject
goftest::cvm.test(x = x, null = "pnorm")

# Comparison with Kolmogorov-Smirnov
ks.test(x = x, y = "pnorm") 

# Sample data from a Pois(5)
set.seed(3245678)
n <- 100
x <- rpois(n = n, lambda = 5)

# Cramér-von Mises test for H_0: F = Pois(5) without specifying that the 
# distribution is discrete. Rejects (!?) without giving a warning message
goftest::cvm.test(x = x, null = "ppois", lambda = 5)

# We rely on dgof::cvm.test and run a Cramér-von Mises test for H_0: F = Pois(5)
# adapted for discrete data, with data coming from a Pois(5)
x_eval <- 0:20
ppois_stepfun <- stepfun(x = x_eval, y = c(0, ppois(q = x_eval, lambda = 5)))
dgof::cvm.test(x = x, y = ppois_stepfun)

# Plot the asymptotic null distribution function
curve(goftest::pCvM(x), from = 0, to = 1, n = 300)

## ---- ad-1------------------------------------------------------------------------------------------
# Sample data from a N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)

# Anderson-Darling test for H_0: F = N(0, 1). Does not reject
goftest::ad.test(x = x, null = "pnorm")

# Sample data from a Pois(5)
set.seed(3245678)
n <- 100
x <- rpois(n = n, lambda = 5)

# Anderson-Darling test for H_0: F = Pois(5) without specifying that the 
# distribution is discrete. Rejects (!?) without giving a warning message
goftest::ad.test(x = x, null = "ppois", lambda = 5)

# We rely on dgof::cvm.test and run an Anderson-Darling test for H_0: F = Pois(5)
# adapted for discrete data, with data coming from a Pois(5)
x_eval <- 0:20
ppois_stepfun <- stepfun(x = x_eval, y = c(0, ppois(q = x_eval, lambda = 5)))
dgof::cvm.test(x = x, y = ppois_stepfun, type = "A2")

# Plot the asymptotic null distribution function
curve(goftest::pAD(x), from = 0, to = 5, n = 300)

## ---- norm-1----------------------------------------------------------------------------------------
# Sample data from a N(10, 1)
set.seed(123456)
n <- 100
x <- rnorm(n = n, mean = 10)

# Normality tests -- do not reject H0
nortest::lillie.test(x = x)
nortest::cvm.test(x = x)
nortest::ad.test(x = x)

# Sample data from a Student's t with 3 degrees of freedom
x <- rt(n = n, df = 3)

# Normality tests -- reject H0
nortest::lillie.test(x = x)
nortest::cvm.test(x = x)
nortest::ad.test(x = x)

# Flawed normality tests -- do not reject because the null distribution
# that is employed is wrong!
ks.test(x = x, y = "pnorm", mean = mean(x), sd = sd(x))
goftest::cvm.test(x = x, null = "pnorm", mean = mean(x), sd = sd(x))
goftest::ad.test(x = x, null = "pnorm", mean = mean(x), sd = sd(x))

## ---- qq, fig.cap = '(ref:qq-title)'----------------------------------------------------------------
n <- 100
mu <- 10
sigma <- 2
set.seed(12345678)
x <- rnorm(n, mean = mu, sd = sigma)
qqnorm(x)
abline(a = mu, b = sigma, col = 2)

## ---- conf-qq, fig.cap = '(ref:conf-qq-title)', fig.margin = FALSE----------------------------------
M <- 1e3
n <- 100
plot(0, 0, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), type = "n",
     xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
     main = "Confidence bands for QQ-plot")
x <- matrix(rnorm(M * n), nrow = n, ncol = M)
matpoints(qnorm(ppoints(n)), apply(x, 2, sort), pch = 19, cex = 0.5,
          col = gray(0, alpha = 0.01))
abline(a = 0, b = 1)
p <- seq(0, 1, l = 1e4)
xi <- qnorm(p)
lines(xi, xi - qnorm(0.975) / sqrt(n) * sqrt(p * (1 - p)) / dnorm(xi),
      col = 2, lwd = 2)
lines(xi, xi + qnorm(0.975) / sqrt(n) * sqrt(p * (1 - p)) / dnorm(xi),
      col = 2, lwd = 2)

## ---- norm-2----------------------------------------------------------------------------------------
# Does not reject H0
set.seed(123456)
n <- 100
x <- rnorm(n = n, mean = 10)
nortest::sf.test(x)

# Rejects H0
x <- rt(n = n, df = 3)
nortest::sf.test(x)

# Test statistic
cor(x = sort(x), y = qnorm(ppoints(n, a = 3/8)))^2

## ---- gof-1-----------------------------------------------------------------------------------------
# A goodness-of-fit test of the exponential distribution using the
# Cramér-von Mises statistic
cvm_exp_gof <- function(x, B = 5e3, plot_boot = TRUE) {

  # Test statistic function (depends on the data only)
  Tn <- function(data) {

    # Maximum likelihood estimator -- MODIFY DEPENDING ON THE PROBLEM
    theta_hat <- 1 / mean(data)

    # Test statistic -- MODIFY DEPENDING ON THE PROBLEM
    goftest::cvm.test(x = data, null = "pexp", rate = theta_hat)$statistic

  }

  # Function to simulate bootstrap samples X_1^*, ..., X_n^*. Requires TWO
  # arguments, one being the data X_1, ..., X_n and the other containing
  # the parameter theta
  r_mod <- function(data, theta) {

    # Simulate from an exponential. In this case, the function only uses
    # the sample size from the data argument to estimate theta -- MODIFY
    # DEPENDING ON THE PROBLEM
    rexp(n = length(data), rate = 1 / theta)

  }

  # Estimate of theta -- MODIFY DEPENDING ON THE PROBLEM
  theta_hat <- 1 / mean(x)

  # Perform bootstrap resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = x, statistic = Tn, sim = "parametric", 
                        ran.gen = r_mod, mle = theta_hat, R = B)

  # Test information -- MODIFY DEPENDING ON THE PROBLEM
  method <- "Bootstrap-based Cramér-von Mises test for exponentiality"
  alternative <- "any alternative to exponentiality"

  # p-value: modify if rejection does not happen for large values of the
  # test statistic
  pvalue <- mean(Tn_star$t > Tn_star$t0)

  # Construct an "htest" result
  result <- list(statistic = c("stat" = Tn_star$t0), p.value = pvalue,
                 theta_hat = theta_hat, statistic_boot = drop(Tn_star$t),
                 B = B, alternative = alternative, method = method,
                 data.name = deparse(substitute(x)))
  class(result) <- "htest"

  # Plot the position of the original statistic with respect to the
  # bootstrap replicates?
  if (plot_boot) {

    hist(result$statistic_boot, probability = TRUE, 
         main = paste("p-value:", result$p.value), 
         xlab = latex2exp::TeX("$T_n^*$"))
    rug(result$statistic_boot)
    abline(v = result$statistic, col = 2)

  }

  # Return "htest"
  return(result)

}

# Check the test for H0 true
set.seed(123456)
x <- rgamma(n = 100, shape = 1, scale = 1)
gof0 <- cvm_exp_gof(x = x, B = 1e3)
gof0

# Check the test for H0 false
x <- rgamma(n = 100, shape = 2, scale = 1)
gof1 <- cvm_exp_gof(x = x, B = 1e3)
gof1

## ---- gof-2-----------------------------------------------------------------------------------------
# A goodness-of-fit test of a mixture of m normals using the
# Cramér-von Mises statistic
cvm_nm_gof <- function(x, m, B = 1e3, plot_boot = TRUE) {

  # Test statistic function (depends on the data only)
  Tn <- function(data) {

    # EM algorithm for fitting normal mixtures. With trace = 0 we disable the
    # default convergence messages or otherwise they will saturate the screen
    # with the bootstrap loop. Be aware that this is a potentially dangerous
    # practice, as we may lose important information about the convergence of
    # the EM algorithm
    theta_hat <- nor1mix::norMixEM(x = data, m = m, trace = 0)

    # Test statistic
    goftest::cvm.test(x = data, null = nor1mix::pnorMix, 
                      obj = theta_hat)$statistic

  }

  # Function to simulate bootstrap samples X_1^*, ..., X_n^*. Requires TWO
  # arguments, one being the data X_1, ..., X_n and the other containing
  # the parameter theta
  r_mod <- function(data, theta) {

    nor1mix::rnorMix(n = length(data), obj = theta)

  }

  # Estimate of theta
  theta_hat <- nor1mix::norMixEM(x = x, m = m, trace = 0)

  # Perform bootstrap resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = x, statistic = Tn, sim = "parametric", 
                        ran.gen = r_mod, mle = theta_hat, R = B)

  # Test information
  method <- "Bootstrap-based Cramér-von Mises test for normal mixtures"
  alternative <- paste("any alternative to a", m, "normal mixture")

  # p-value: modify if rejection does not happen for large values of the
  # test statistic
  pvalue <- mean(Tn_star$t > Tn_star$t0)

  # Construct an "htest" result
  result <- list(statistic = c("stat" = Tn_star$t0), p.value = pvalue,
                 theta_hat = theta_hat, statistic_boot = drop(Tn_star$t),
                 B = B, alternative = alternative, method = method,
                 data.name = deparse(substitute(x)))
  class(result) <- "htest"

  # Plot the position of the original statistic with respect to the
  # bootstrap replicates?
  if (plot_boot) {

    hist(result$statistic_boot, probability = TRUE, 
         main = paste("p-value:", result$p.value), 
         xlab = latex2exp::TeX("$T_n^*$"))
    rug(result$statistic_boot)
    abline(v = result$statistic, col = 2)

  }

  # Return "htest"
  return(result)

}

# Check the test for H0 true
set.seed(123456)
x <- c(rnorm(n = 100, mean = 2, sd = 0.5), rnorm(n = 100, mean = -2))
gof0 <- cvm_nm_gof(x = x, m = 2, B = 1e3)
gof0

# Graphical assessment that H0 (parametric fit) and data (kde) match
plot(gof0$theta_hat, p.norm = FALSE, ylim = c(0, 0.5))
plot(ks::kde(x), col = 2, add = TRUE)

# Check the test for H0 false
x <- rgamma(n = 100, shape = 2, scale = 1)
gof1 <- cvm_nm_gof(x = x, m = 1, B = 1e3)
gof1

# Graphical assessment that H0 (parametric fit) and data (kde) do not match
plot(gof1$theta_hat, p.norm = FALSE, ylim = c(0, 0.5))
plot(ks::kde(x), col = 2, add = TRUE)

## ---- comp-ks-2, fig.cap = '(ref:comp-ks-2-title)'--------------------------------------------------
# Sample data
n <- 10; m <- 10
mu1 <- 0; sd1 <- 1
mu2 <- 0; sd2 <- 2
set.seed(1998)
samp1 <- rnorm(n = n, mean = mu1, sd = sd1)
samp2 <- rnorm(n = m, mean = mu2, sd = sd2)

# Fn vs. Gm
plot(ecdf(samp1), main = "", ylab = "Probability", xlim = c(-3.5, 3.5),
     ylim = c(0, 1.3))
lines(ecdf(samp2), main = "", col = 4)

# Add Dnm1
samp1_sorted <- sort(samp1)
samp2_sorted <- sort(samp2)
Dnm_1 <- abs((1:n) / n - ecdf(samp2)(samp1_sorted))
i1 <- which.max(Dnm_1)
lines(rep(samp2_sorted[i1], 2), 
      c(i1 / m, ecdf(samp1_sorted)(samp2_sorted[i1])), 
      col = 3, lwd = 2, type = "o", pch = 16, cex = 0.75)
rug(samp1, col = 1)

# Add Dnm2
Dnm_2 <- abs(ecdf(samp1)(samp2_sorted) - (1:m) / m)
i2 <- which.max(Dnm_2)
lines(rep(samp1_sorted[i2], 2), 
      c(i2 / n, ecdf(samp2_sorted)(samp1_sorted[i2])), 
      col = 2, lwd = 2, type = "o", pch = 16, cex = 0.75)
rug(samp2, col = 4)
legend("topleft", lwd = 2, col = c(1, 4, 3, 2), legend =
         latex2exp::TeX(c("$F_n$", "$G_m$", "$D_{n,m,1}$", "$D_{n,m,2}$")))

## ---- comp-ks-3-------------------------------------------------------------------------------------
# Check the test for H0 true
set.seed(123456)
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)
ks.test(x = x0, y = y0)

# Check the test for H0 false
x1 <- rgamma(n = 50, shape = 2, scale = 1)
y1 <- rgamma(n = 75, shape = 1, scale = 1)
ks.test(x = x1, y = y1)

## ---- comp-ks-4-------------------------------------------------------------------------------------
# Check the test for H0 true
set.seed(123456)
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)
ks.test(x = x0, y = y0, alternative = "less") # H1: F < G
ks.test(x = x0, y = y0, alternative = "greater") # H1: F > G

# Check the test for H0 false
x1 <- rnorm(n = 50, mean = 1, sd = 1)
y1 <- rnorm(n = 75, mean = 0, sd = 1)
ks.test(x = x1, y = y1, alternative = "less") # H1: F < G
ks.test(x = x1, y = y1, alternative = "greater") # H1: F > G
# Interpretations:
# 1. Strong rejection of H0: F = G in favor of H1: F < G when
# alternative = "less", as in reality x1 is stochastically greater than y1.
# This outcome allows stating that "there is strong evidence supporting that
# x1 is locally stochastically greater than y1"
# 2. No rejection ("strong acceptance") of H0: F = G versus H1: F > G when
# alternative = "greater". Even if in reality x1 is stochastically greater than
# y1 (so H0 is false), the alternative to which H0 is confronted is even less
# plausible. A p-value ~ 1 indicates one is probably conducting the test in
# the uninteresting direction alternative!

## ---- comp-cvm-1------------------------------------------------------------------------------------
# Two-sample Cramér-von Mises statistic
cvm2_stat <- function(x, y) {

  # Sample sizes
  n <- length(x)
  m <- length(y)

  # Pooled sample
  z <- c(x, y)

  # Statistic computation via ecdf()
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2)

}

# Check the test for H0 true
set.seed(123456)
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)
cvm0 <- cvm2_stat(x = x0, y = y0)
pval0 <- 1 - goftest::pCvM(q = cvm0)
c("statistic" = cvm0, "p-value"= pval0)

# Check the test for H0 false
x1 <- rgamma(n = 50, shape = 2, scale = 1)
y1 <- rgamma(n = 75, shape = 1, scale = 1)
cvm1 <- cvm2_stat(x = x1, y = y1)
pval1 <- 1 - goftest::pCvM(q = cvm1)
c("statistic" = cvm1, "p-value"= pval1)

## ---- comp-ad-1-------------------------------------------------------------------------------------
# Two-sample Anderson-Darling statistic
ad2_stat <- function(x, y) {

  # Sample sizes
  n <- length(x)
  m <- length(y)

  # Pooled sample and pooled ecdf
  z <- c(x, y)
  z <- z[-which.max(z)] # Exclude the largest point
  H <- rank(z) / (n + m)

  # Statistic computation via ecdf()
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2 / ((1 - H) * H))

}

# Check the test for H0 true
set.seed(123456)
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)
ad0 <- ad2_stat(x = x0, y = y0)
pval0 <- 1 - goftest::pAD(q = ad0)
c("statistic" = ad0, "p-value"= pval0)

# Check the test for H0 false
x1 <- rgamma(n = 50, shape = 2, scale = 1)
y1 <- rgamma(n = 75, shape = 1, scale = 1)
ad1 <- ad2_stat(x = x1, y = y1)
pval1 <- 1 - goftest::pAD(q = ad1)
c("statistic" = ad1, "p-value"= pval1)

## ---- comp-wilcox-4---------------------------------------------------------------------------------
# Check the test for H0 true
set.seed(123456)
n <- 50
m <- 100
x0 <- rgamma(n = n, shape = 1, scale = 1)
y0 <- rgamma(n = m, shape = 1, scale = 1)
wilcox.test(x = x0, y = y0) # H1: P[X >= Y] != 0.5
wilcox.test(x = x0, y = y0, alternative = "greater") # H1: P[X >= Y] > 0.5
wilcox.test(x = x0, y = y0, alternative = "less") # H1: P[X <= Y] > 0.5

# Check the test for H0 false
x1 <- rnorm(n = n, mean = 0, sd = 1)
y1 <- rnorm(n = m, mean = 1, sd = 2)
wilcox.test(x = x1, y = y1) # H1: P[X >= Y] != 0.5
wilcox.test(x = x1, y = y1, alternative = "greater") # H1: P[X >= Y] > 0.5
wilcox.test(x = x1, y = y1, alternative = "less") # H1: P[X <= Y] > 0.5

# Exact pmf versus asymptotic pdf
# Beware! dwilcox considers (m, n) as the sample sizes of (X, Y)
x <- seq(1500, 3500, by = 100)
plot(x, dwilcox(x = x, m = n, n = m), type = "h", ylab = "Density")
curve(dnorm(x, mean = n * m / 2, sd = sqrt((n * m * (n + m + 1)) / 12)),
      add = TRUE, col = 2)

## ---- comp-signed-1---------------------------------------------------------------------------------
# Check the test for H0 true
set.seed(123456)
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- x0 + rnorm(n = 50)
wilcox.test(x = x0, y = y0, paired = TRUE) # H1: P[X >= Y] != 0.5
wilcox.test(x = x0, y = y0, paired = TRUE, alternative = "greater")
# H1: P[X >= Y] > 0.5
wilcox.test(x = x0, y = y0, paired = TRUE, alternative = "less")
# H1: P[X <= Y] > 0.5

# Check the test for H0 false
x1 <- rnorm(n = 50, mean = 0, sd = 1)
y1 <- x1 + rnorm(n = 50, mean = 1, sd = 2)
wilcox.test(x = x1, y = y1, paired = TRUE) # H1: P[X >= Y] != 0.5
wilcox.test(x = x1, y = y1, paired = TRUE, alternative = "greater")
# H1: P[X >= Y] > 0.5
wilcox.test(x = x1, y = y1, paired = TRUE, alternative = "less")
# H1: P[X <= Y] > 0.5

## ---- comp-perm-1-----------------------------------------------------------------------------------
# A homogeneity test using the Anderson-Darling statistic
perm_comp_test <- function(x, y, B = 1e3, plot_boot = TRUE) {

  # Sizes of x and y
  n <- length(x)
  m <- length(y)

  # Test statistic function. Requires TWO arguments, one being the original
  # data (X_1, ..., X_n, Y_1, ..., Y_m) and the other containing the random
  # index for permuting the sample
  Tn <- function(data, perm_index) {

    # Permute sample by perm_index
    data <- data[perm_index]

    # Split into two samples
    x <- data[1:n]
    y <- data[(n + 1):(n + m)]

    # Test statistic -- MODIFY DEPENDING ON THE PROBLEM
    ad2_stat(x = x, y = y)

  }

  # Perform permutation resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = c(x, y), statistic = Tn,
                        sim = "permutation", R = B)

  # Test information -- MODIFY DEPENDING ON THE PROBLEM
  method <- "Permutation-based Anderson-Darling test of homogeneity"
  alternative <- "any alternative to homogeneity"

  # p-value: modify if rejection does not happen for large values of the
  # test statistic
  pvalue <- mean(Tn_star$t > Tn_star$t0)

  # Construct an "htest" result
  result <- list(statistic = c("stat" = Tn_star$t0), p.value = pvalue,
                 statistic_perm = drop(Tn_star$t),
                 B = B, alternative = alternative, method = method,
                 data.name = deparse(substitute(x)))
  class(result) <- "htest"

  # Plot the position of the original statistic with respect to the
  # permutation replicates?
  if (plot_boot) {

    hist(result$statistic_perm, probability = TRUE, 
         main = paste("p-value:", result$p.value), 
         xlab = latex2exp::TeX("$T_{n,m}^*$"))
    rug(result$statistic_perm)
    abline(v = result$statistic, col = 2)

  }

  # Return "htest"
  return(result)

}

# Check the test for H0 true
set.seed(123456)
x0 <- rpois(n = 75, lambda = 5)
y0 <- rpois(n = 75, lambda = 5)
comp0 <- perm_comp_test(x = x0, y = y0, B = 1e3)
comp0

# Check the test for H0 false
x1 <- rpois(n = 50, lambda = 3)
y1 <- rpois(n = 75, lambda = 5)
comp1 <- perm_comp_test(x = x1, y = y1, B = 1e3)
comp1

## ---- indep-assoc-1---------------------------------------------------------------------------------
# Outliers fool correlation, but do not fool concordance
set.seed(123456)
n <- 200
x <- rnorm(n)
y <- rnorm(n)
y[n] <- x[n] <- 1e4
cor.test(x, y, method = "pearson") # Close to 1 due to outlier
cor.test(x, y, method = "kendall") # Close to 0, as it should
cor.test(x, y, method = "spearman") # Close to 0, as it should
cor(rank(x), rank(y)) # Spearman's rho

# Outliers fool the sign of correlation, but do not fool concordance
x <- rnorm(n)
y <- x
x[n] <- 1e3
y[n] <- -1e4
cor.test(x, y, method = "pearson", alternative = "greater") # Change of sign!
cor.test(x, y, method = "kendall", alternative = "greater") # Fine
cor.test(x, y, method = "spearman", alternative = "greater") # Fine

## ---- indep-assoc-2---------------------------------------------------------------------------------
# Non-monotone dependence fools concordance
set.seed(123456)
n <- 200
x <- rnorm(n)
y <- abs(x) + rnorm(n)
cor.test(x, y, method = "kendall")
cor.test(x, y, method = "spearman")

# Dependence in terms of conditional variance fools concordance
x <- rnorm(n)
y <- rnorm(n, sd = abs(x))
cor.test(x, y, method = "kendall")
cor.test(x, y, method = "spearman")

## ---- indep-dcor-2----------------------------------------------------------------------------------
# Distance correlation detects non-monotone dependence
set.seed(123456)
n <- 200
x <- rnorm(n)
y <- abs(x) + rnorm(n)

# Distance covariance and correlation tests. R is the number of permutations
# and needs to be specified (the default is R = 0 -- no test is produced)
energy::dcov.test(x, y, R = 1e3)
energy::dcor.test(x, y, R = 1e3)

# Distance correlation detects conditional variance as dependence
x <- rnorm(n)
y <- rnorm(n, sd = abs(x))

# Distance covariance and correlation tests
energy::dcov.test(x, y, R = 1e3)
energy::dcor.test(x, y, R = 1e3)

## ---- indep-dcor-3----------------------------------------------------------------------------------
# A multivariate case with independence
set.seed(123456)
n <- 200
p <- 5
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- matrix(rnorm(n * p), nrow = n, ncol = p)
energy::dcov.test(x, y, R = 1e3)
energy::dcor.test(x, y, R = 1e3)

# A multivariate case with dependence
y <- matrix(0.2 * rnorm(n = n * p, mean = c(x)) +
              rnorm(n * p, sd = 1.25), nrow = n, ncol = p)
energy::dcov.test(x, y, R = 1e3)
energy::dcor.test(x, y, R = 1e3)

## ---- ind-perm-1------------------------------------------------------------------------------------
# A no-association test using the absolute value of Spearman's rho statistic
perm_ind_test <- function(x, B = 1e3, plot_boot = TRUE) {

  # Test statistic function. Requires TWO arguments, one being the original
  # data (X_1, Y_1), ..., (X_n, Y_n) and the other containing the random
  # index for permuting the second component of sample
  Tn <- function(data, perm_index) {

    # Permute sample by perm_index -- only permute the second component
    data[, 2] <- data[, 2][perm_index]

    # Test statistic -- MODIFY DEPENDING ON THE PROBLEM
    abs(cor(x = data[, 1], y = data[, 2], method = "spearman"))

  }

  # Perform permutation resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = x, statistic = Tn, sim = "permutation", R = B)

  # Test information -- MODIFY DEPENDING ON THE PROBLEM
  method <- "Permutation-based Spearman's rho test of no concordance"
  alternative <- "Spearman's rho is not zero"

  # p-value: modify if rejection does not happen for large values of the
  # test statistic
  pvalue <- mean(Tn_star$t > Tn_star$t0)

  # Construct an "htest" result
  result <- list(statistic = c("stat" = Tn_star$t0), p.value = pvalue,
                 statistic_perm = drop(Tn_star$t),
                 B = B, alternative = alternative, method = method,
                 data.name = deparse(substitute(x)))
  class(result) <- "htest"

  # Plot the position of the original statistic with respect to the
  # permutation replicates?
  if (plot_boot) {

    hist(result$statistic_perm, probability = TRUE,
         main = paste("p-value:", result$p.value),
         xlab = latex2exp::TeX("$T_n^*$"))
    rug(result$statistic_perm)
    abline(v = result$statistic, col = 2)

  }

  # Return "htest"
  return(result)

}

# Check the test for H0 true
set.seed(123456)
x0 <- mvtnorm::rmvnorm(n = 100, mean = c(0, 0), sigma = diag(c(1:2)))
ind0 <- perm_ind_test(x = x0, B = 1e3)
ind0

# Check the test for H0 false
x1 <- mvtnorm::rmvnorm(n = 100, mean = c(0, 0), sigma = toeplitz(c(1, 0.5)))
ind1 <- perm_ind_test(x = x1, B = 1e3)
ind1

