
## ------------------------------------------------------------------------
## Name: 06-nptests.R
## Description: Script for Chapter 6 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- ks-1, fig.cap = '(ref:ks-1-title)', fig.margin = FALSE-------------
# Sample data with H_0 true
n <- 20
mu0 <- 2; sd0 <- 1
set.seed(12341)
samp <- rnorm(n = n, mean = mu0, sd = sd0)

# Fn vs F0
plot(ecdf(samp), main = "", ylab = "Probability")
curve(pnorm(x, mean = mu0, sd = sd0), add = TRUE, col = 2)

# Maximum distance
samp_sorted <- sort(samp)
Ui <- pnorm(samp_sorted, mean = mu0, sd = sd0)
Dn_plus <- (1:n) / n - Ui
Dn_minus <- Ui - (1:n - 1) / n
i <- which.max(pmax(Dn_plus, Dn_minus))
lines(rep(samp_sorted[i], 2), 
      c(i / n, pnorm(samp_sorted[i], mean = mu0, sd = sd0)), 
      col = 4, lwd = 2)
rug(samp)
legend("topleft", lwd = 2, col = c(1:2, 4), 
       legend = latex2exp::TeX(c("$F_n$", "$F_0$", "sup_x|F_n(x)-F_0(x)|")))

## ---- ks-2---------------------------------------------------------------
# Sample data from a N(0, 1)
n <- 50
set.seed(3245678)
x <- rnorm(n = n)

# Kolmogorov-Smirnov test for H_0: F = N(0, 1). Does not reject.
(ks <- ks.test(x = x, y = "pnorm")) # In "y" we specify the cdf F0 as a function

# Structure of "htest" class
str(ks)

# Kolmogorov-Smirnov test for H_0: F = N(0.5, 1). Rejects.
ks.test(x = x, y = "pnorm", mean = 0.5)

# Kolmogorov-Smirnov test for H_0: F = Exp(2). Strongly rejects.
ks.test(x = x, y = "pexp", rate = 1/2)

## ---- ks-3---------------------------------------------------------------
# Sample data from a Pois(5)
n <- 100
set.seed(3245678)
x <- rpois(n = n, lambda = 5)

# Kolmogorov-Smirnov test for H_0: F = Pois(5) without specifiying that the 
# distribution is discrete. Rejects (!?) giving a warning message
ks.test(x = x, y = "ppois", lambda = 5)

# We rely on dgof::ks.test, which works as stats::ks.test if the "y" argument 
# is not marked as a "stepfun" object, the way the package codifies discrete 
# distribution functions

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

## ---- ks-4, fig.margin = FALSE, fig.fullwidth = TRUE, fig.asp = 1/2, fig.cap = '(ref:ks-4-title)', out.width = '100%'----
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
     main = latex2exp::TeX("$H_0$"), ylim = c(0, 3.5))
abline(h = 1, col = 2)
hist(pvalues_H1, breaks = seq(0, 1, l = 20), probability = TRUE,
     main = latex2exp::TeX("$H_1$"), ylim = c(0, 3.5))
abline(h = 1, col = 2)

## ---- cvm-1--------------------------------------------------------------
# Sample data from a N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)

# Cramér-von Mises test for H_0: F = N(0, 1). Does not reject.
goftest::cvm.test(x = x, null = "pnorm")

# Comparison with Kolmogorov-Smirnov
ks.test(x = x, y = "pnorm") 

# Sample data from a Pois(5)
set.seed(3245678)
n <- 100
x <- rpois(n = n, lambda = 5)

# Cramér-von Mises test for H_0: F = Pois(5) without specifiying that the 
# distribution is discrete. Rejects (!?) without giving a warning message
goftest::cvm.test(x = x, null = "ppois", lambda = 5)

# We rely on dgof::cvm.test and run a Cramér-von Mises test for H_0: F = Pois(5)
# adapted for discrete data, with data coming from a Pois(5)
x_eval <- 0:20
ppois_stepfun <- stepfun(x = x_eval, y = c(0, ppois(q = x_eval, lambda = 5)))
dgof::cvm.test(x = x, y = ppois_stepfun)

# Plot the asymptotic null distribution function
curve(goftest::pCvM(x), from = 0, to = 1, n = 300)

## ---- ad-1---------------------------------------------------------------
# Sample data from a N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n = n)

# Anderson-Darling test for H_0: F = N(0, 1). Does not reject.
goftest::ad.test(x = x, null = "pnorm")

# Sample data from a Pois(5)
set.seed(3245678)
n <- 100
x <- rpois(n = n, lambda = 5)

# Anderson-Darling test for H_0: F = Pois(5) without specifiying that the 
# distribution is discrete. Rejects (!?) without giving a warning message
goftest::ad.test(x = x, null = "ppois", lambda = 5)

# We rely on dgof::cvm.test and run an Anderson-Darling test for H_0: F = Pois(5)
# adapted for discrete data, with data coming from a Pois(5)
x_eval <- 0:20
ppois_stepfun <- stepfun(x = x_eval, y = c(0, ppois(q = x_eval, lambda = 5)))
dgof::cvm.test(x = x, y = ppois_stepfun, type = "A2")

# Plot the asymptotic null distribution function
curve(goftest::pAD(x), from = 0, to = 5, n = 300)

## ---- norm-1-------------------------------------------------------------
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

## ---- qq, fig.cap = '(ref:qq-title)'-------------------------------------
n <- 100
mu <- 10; sigma <- 2
set.seed(12345678)
x <- rnorm(n, mean = mu, sd = sigma)
qqnorm(x)
abline(a = mu, b = sigma, col = 2)

## ---- conf-qq, echo = FALSE, fig.cap = '(ref:conf-qq-title)'-------------
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

## ---- norm-2-------------------------------------------------------------
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

## ---- gof-1--------------------------------------------------------------
# A goodness-of-fit test of the exponential distribution using the Cramér-von 
# Mises statistic
cvm_exp_test <- function(x, B = 5e3, plot_boot = TRUE) {

  # Test statistic function (only depends on the data)
  Tn <- function(data) {
    
    # Maximum likelihood estimator
    theta_hat <- 1 / mean(data)
    
    # Test statistic
    goftest::cvm.test(x = data, null = "pexp", rate = theta_hat)$statistic
    
  }
  
  # Function to simulate bootstrap samples X_1^*, ..., X_n^* from an 
  # Exp(theta). Requires TWO arguments, one being the data X_1, ..., X_n 
  # (in this case, the function only uses the sample size from the data 
  # argument) and other with the parameter theta
  r_mod <- function(data, theta) {
    
    rexp(n = length(data), rate = 1 / theta)
    
  }
  
  # Estimate of theta
  theta_hat <- 1 / mean(x)
  
  # Perform bootstrap resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = x, statistic = Tn, sim = "parametric", 
                        ran.gen = r_mod, mle = theta_hat, R = B)
  
  # Return an "htest" result
  method <- "Bootstrap-based Cramér-von Mises test for exponentiality"
  result <- list(statistic = Tn_star$t0, p.value = mean(Tn_star$t > Tn_star$t0), 
                 theta_hat = theta_hat, statistic_boot = drop(Tn_star$t), B = B,
                 alternative = "any alternative to exponentiality", 
                 method = method, data.name = deparse(substitute(x)))
  class(result) <- "htest"
    
  # Plot the position of the original statistic with respect to the bootstrap 
  # replicates?
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
cvm_exp_test(x = x, B = 1e3)

# Check the test for H0 false
x <- rgamma(n = 100, shape = 2, scale = 1)
cvm_exp_test(x = x, B = 1e3)

## ---- gof-2--------------------------------------------------------------
# A goodness-of-fit test of a mixture of m normals using the Cramér-von Mises 
# statistic
cvm_nm_test <- function(x, m, B = 1e3, plot_boot = TRUE) {

  # Test statistic function (only depends on the data)
  Tn <- function(data) {
    
    # EM algorithm for fitting normal mixtures. With trace = 0 we disable the
    # default convergence messages or otherwise they will saturate the screen
    # the bootstrap loop. Be aware that this is a potentially dangerous 
    # practice, as we may lose important information about the convergence of
    # the EM algorithm
    theta_hat <- nor1mix::norMixEM(x = data, m = m, trace = 0)
    
    # Test statistic
    goftest::cvm.test(x = data, null = nor1mix::pnorMix, 
                      obj = theta_hat)$statistic
    
  }
  
  # Function to simulate bootstrap samples X_1^*, ..., X_n^*
  r_mod <- function(data, theta) {
    
    nor1mix::rnorMix(n = length(data), obj = theta)
    
  }
  
  # Estimate of theta
  theta_hat <- nor1mix::norMixEM(x = x, m = m, trace = 0)
  
  # Perform bootstrap resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = x, statistic = Tn, sim = "parametric", 
                        ran.gen = r_mod, mle = theta_hat, R = B)
  
  # Return an "htest" result
  method <- "Bootstrap-based Cramér-von Mises test for normal mixtures"
  result <- list(statistic = Tn_star$t0, p.value = mean(Tn_star$t > Tn_star$t0), 
                 theta_hat = theta_hat, statistic_boot = drop(Tn_star$t), B = B,
                 alternative = paste("any alternative to a", m, 
                                     "normal mixture"),
                 method = method, data.name = deparse(substitute(x)))
  class(result) <- "htest"
    
  # Plot the position of the original statistic with respect to the bootstrap 
  # replicates?
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
(gof <- cvm_nm_test(x = x, m = 2, B = 1e3))

# Graphical assessment that H0 (parametric model) and data (kde) match
plot(gof$theta_hat)
plot(ks::kde(x), col = 2, add = TRUE)

# Check the test for H0 false
x <- rgamma(n = 100, shape = 2, scale = 1)
(gof <- cvm_nm_test(x = x, m = 1, B = 1e3))

# Graphical assessment that H0 (parametric model) and data (kde) do not match
plot(gof$theta_hat)
plot(ks::kde(x), col = 2, add = TRUE)

