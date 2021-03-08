
## ------------------------------------------------------------------------
## Name: 05-kre-ii.R
## Description: Script for Chapter 5 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- mult-1, eval = knitr:::is_html_output(), cache = TRUE-----------------------
## # Sample data from a bivariate regression
## n <- 300
## set.seed(123456)
## X <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
##                       sigma = matrix(c(2, 0.5, 0.5, 1.5), nrow = 2, ncol = 2))
## m <- function(x) 0.5 * (x[, 1]^2 + x[, 2]^2)
## epsilon <- rnorm(n)
## Y <- m(x = X) + epsilon
## 
## # Plot sample and regression function
## rgl::plot3d(x = X[, 1], y = X[, 2], z = Y, xlim = c(-3, 3), ylim = c(-3, 3),
##             zlim = c(-4, 10), xlab = "X1", ylab = "X2", zlab = "Y")
## lx <- ly <- 50
## x_grid <- seq(-3, 3, l = lx)
## y_grid <- seq(-3, 3, l = ly)
## xy_grid <- as.matrix(expand.grid(x_grid, y_grid))
## rgl::surface3d(x = x_grid, y = y_grid,
##                z = matrix(m(xy_grid), nrow = lx, ncol = ly),
##                col = "lightblue", alpha = 1, lit = FALSE)
## 
## # Local constant fit
## 
## # An alternative for calling np::npregbw without formula
## bw0 <- np::npregbw(xdat = X, ydat = Y, regtype = "lc")
## kre0 <- np::npreg(bws = bw0, exdat = xy_grid) # Evaluation grid is now a matrix
## rgl::surface3d(x = x_grid, y = y_grid,
##                z = matrix(kre0$mean, nrow = lx, ncol = ly),
##                col = "red", alpha = 0.25, lit = FALSE)
## 
## # Local linear fit
## bw1 <- np::npregbw(xdat = X, ydat = Y, regtype = "ll")
## kre1 <- np::npreg(bws = bw1, exdat = xy_grid)
## rgl::surface3d(x = x_grid, y = y_grid,
##                z = matrix(kre1$mean, nrow = lx, ncol = ly),
##                col = "green", alpha = 0.25, lit = FALSE)
## rgl::rglwidget()


## ---- mult-2----------------------------------------------------------------------
# Load the wine dataset
wine <- read.table(file = "datasets/wine.csv", header = TRUE, sep = ",")

# Bandwidth by CV for local linear estimator -- a product kernel with
# 4 bandwidths
# Employs 4 random starts for minimizing the CV surface
bw_wine <- np::npregbw(formula = Price ~ Age + WinterRain + AGST +
                         HarvestRain, data = wine, regtype = "ll")
bw_wine

# Regression
fit_wine <- np::npreg(bw_wine)
summary(fit_wine)

# Plot the "marginal effects of each predictor" on the response
plot(fit_wine)

# These marginal effects are the p profiles of the estimated regression surface
# \hat{m}(x_1, ..., x_p) that are obtained by fixing the predictors to each of
# their median values. For example, the profile for Age is the curve
# \hat{m}(x, median_WinterRain, median_AGST, median_HarvestRain). The medians
# are:
apply(wine[c("Age", "WinterRain", "AGST", "HarvestRain")], 2, median)

# Therefore, conditionally on the median values of the predictors:
# - Age is positively related to Price (almost linearly)
# - WinterRain is positively related to Price (with a subtle nonlinearity)
# - AGST is positively related to Price, but now we see what it looks like a
#   quadratic pattern
# - HarvestRain is negatively related to Price (almost linearly)


## ---- mult-3----------------------------------------------------------------------
# The argument "xq" controls the conditioning quantile of the predictors, by
# default the median (xq = 0.5). But xq can be a vector of p quantiles, for
# example (0.25, 0.5, 0.25, 0.75) for (Age, WinterRain, AGST, HarvestRain)
plot(fit_wine, xq = c(0.25, 0.5, 0.25, 0.75))

# With "plot.behavior = data" the plot() function returns a list with the data
# for performing the plots
res <- plot(fit_wine, xq = 0.5, plot.behavior = "data")
str(res, 1)

# Plot the marginal effect of AGST ($r3) alone
head(res$r3$eval) # All the predictors are constant (medians, except age)
plot(res$r3$eval$V3, res$r3$mean, type = "l", xlab = "AGST",
     ylab = "Marginal effect")

# Plot the marginal effects of AGST for varying quantiles in the rest of
# predictors (all with the same quantile)
tau <- seq(0.1, 0.9, by = 0.1)
res <- plot(fit_wine, xq = tau[1], plot.behavior = "data")
col <- viridis::viridis(length(tau))
plot(res$r3$eval$V3, res$r3$mean, type = "l", xlab = "AGST",
     ylab = "Marginal effect", col = col[1], ylim = c(6, 9),
     main = "Marginal effects of AGST for varying quantiles in the predictors")
for (i in 2:length(tau)) {
  res <- plot(fit_wine, xq = tau[i], plot.behavior = "data")
  lines(res$r3$eval$V3, res$r3$mean, col = col[i])
}
legend("topleft", legend = latex2exp::TeX(paste0("$\\tau =", tau, "$")),
       col = col, lwd = 2)

# These quantiles are
apply(wine[c("WinterRain", "AGST", "HarvestRain")], 2, quantile, prob = tau)




## ---- mix-1, fig.margin = FALSE, fig.asp = 1/2------------------------------------
# Bandwidth by CV for local linear estimator
# Recall that Species is a factor!
bw_iris <- np::npregbw(formula = Petal.Length ~ Sepal.Width + Species,
                       data = iris)
bw_iris
# Product kernel with 2 bandwidths

# Regression
fit_iris <- np::npreg(bw_iris)
summary(fit_iris)

# Plot marginal effects (for quantile 0.5) of each predictor on the response
par(mfrow = c(1, 2))
plot(fit_iris, plot.par.mfrow = FALSE)
# Options for the plot method for np::npreg available at ?np::npplot

# Plot marginal effects (for quantile 0.9) of each predictor on the response
par(mfrow = c(1, 2))
plot(fit_iris, xq = 0.9, plot.par.mfrow = FALSE)


## ---- mix-2, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3--------------
# Load data
data(oecdpanel, package = "np")

# Bandwidth by CV for local constant -- use only two starts to reduce the
# computation time
bw_OECD <- np::npregbw(formula = growth ~ oecd + ordered(year) +
                         initgdp + popgro + inv + humancap, data = oecdpanel,
                       regtype = "lc", nmulti = 2)
bw_OECD

# Recall that ordered(year) id doing an in-formula transformation of year,
# which is *not* codified as an ordered factor in the oecdpanel dataset
# Therefore, if ordered() was not present, year would have been treated
# as continuous, as illustrated below
np::npregbw(formula = growth ~ oecd + year + initgdp + popgro +
              inv + humancap, data = oecdpanel, regtype = "lc", nmulti = 2)

# A cleaner approach to avoid doing the in-formula transformation, which
# may be problematic when using predict() or np_pred_CI(), is to directly
# change in the dataset the nature of the factor/ordered variables that are not
# codified as such. For example:
oecdpanel$year <- ordered(oecdpanel$year)
bw_OECD <- np::npregbw(formula = growth ~ oecd + year + initgdp + popgro +
                         inv + humancap, data = oecdpanel,
                       regtype = "lc", nmulti = 2)
bw_OECD

# Regression
fit_OECD <- np::npreg(bw_OECD)
summary(fit_OECD)

# Plot marginal effects of each predictor on the response
par(mfrow = c(2, 3))
plot(fit_OECD, plot.par.mfrow = FALSE)










## ---- predci-1, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3-----------
# Asymptotic confidence bands for the marginal effects of each predictor on
# the response
par(mfrow = c(2, 3))
plot(fit_OECD, plot.errors.method = "asymptotic", common.scale = FALSE,
     plot.par.mfrow = FALSE)

# The asymptotic standard errors associated with the regression evaluated at
# the evaluation points are in $merr
head(fit_OECD$merr)

# Recall that in $mean we had the regression evaluated at the evaluation points,
# by default the sample of the predictors, so in this case it is the same as
# the fitted values
head(fit_OECD$mean)

# Fitted values
head(fitted(fit_OECD))

# Prediction for the first 3 points + asymptotic standard errors
pred <- predict(fit_OECD, newdata = oecdpanel[1:3, ], se.fit = TRUE)

# Predictions
pred$fit

# Manual computation of the asymptotic 100 * (1 - alpha)% confidence intervals
# for the conditional mean of the first 3 points
alpha <- 0.05
z_alpha2 <- qnorm(1 - alpha / 2)
cbind(pred$fit - z_alpha2 * pred$se.fit, pred$fit + z_alpha2 * pred$se.fit)

# Recall that z_alpha2 is almost 2
z_alpha2


## ---- predci-2, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3-----------
# Bootstrap confidence bands (using naive bootstrap, the default)
# They take more time to compute because a resampling + refitting takes place
B <- 200
par(mfrow = c(2, 3))
plot(fit_OECD, plot.errors.method = "bootstrap", common.scale = FALSE,
     plot.par.mfrow = FALSE, plot.errors.boot.num = B, random.seed = 42)
# plot.errors.boot.num is B and defaults to 399
# random.seed fixes the seed to always get the same bootstrap errors. It
# defaults to 42 if not specified


## ---- predci-3--------------------------------------------------------------------
# Univariate local constant regression with CV bandwidth
bw1 <- np::npregbw(formula = growth ~ initgdp, data = oecdpanel, regtype = "lc")
fit1 <- np::npreg(bw1)
summary(fit1)

# Asymptotic (not bootstrap) standard errors
head(fit1$merr)

# Normal approximation confidence intervals + extraction of errors
npplot_std <- plot(fit1, plot.errors.method = "bootstrap",
                   plot.errors.type = "standard", plot.errors.boot.num = B, 
                   plot.errors.style = "bar", plot.behavior = "plot-data",
                   lwd = 2)
lines(npplot_std$r1$eval[, 1], npplot_std$r1$mean + npplot_std$r1$merr[, 1],
      col = 2, lty = 2)
lines(npplot_std$r1$eval[, 1], npplot_std$r1$mean + npplot_std$r1$merr[, 2],
      col = 2, lty = 2)

# These bootstrap standard errors are different from the asymptotic ones
head(npplot_std$r1$merr)

# Quantile confidence intervals + extraction of errors
npplot_qua <- plot(fit1, plot.errors.method = "bootstrap",
                   plot.errors.type = "quantiles", plot.errors.boot.num = B,
                   plot.errors.style = "bar", plot.behavior = "plot-data")
lines(npplot_qua$r1$eval[, 1], npplot_qua$r1$mean + npplot_qua$r1$merr[, 1],
      col = 2, lty = 2)
lines(npplot_qua$r1$eval[, 1], npplot_qua$r1$mean + npplot_qua$r1$merr[, 2],
      col = 2, lty = 2)

# These bootstrap standard errors are different from the asymptotic ones,
# and also from the previous bootstrap errors (different confidence
# interval method)
head(npplot_qua$r1$merr)

# There is no predict() method featuring bootstrap confidence intervals,
# it has to be coded manually!

# Function to predict and compute confidence intervals for m(x). Takes as main
# arguments a np::npreg object (npfit) and the values of the predictors where
# to carry out prediction (exdat). Requires that exdat is a data.frame of the
# same type than the one used for the predictors (e.g., there will be an error
# if one variable appears as a factor for computing npfit and then is passed
# as a numeric in exdat)
np_pred_CI <- function(npfit, exdat, B = 200, conf = 0.95,
                       type_CI = c("standard", "quantiles")[1]) {

  # Extract predictors
  xdat <- npfit$eval

  # Extract response, using a trick from np::npplot.rbandwidth
  tt <- terms(npfit$bws)
  tmf <- npfit$bws$call[c(1, match(c("formula", "data"),
                                   names(npfit$bws$call)))]
  tmf[[1]] <- as.name("model.frame")
  tmf[["formula"]] <- tt
  tmf <- eval(tmf, envir = environment(tt))
  ydat <- model.response(tmf)

  # Predictions
  m_hat <- np::npreg(txdat = xdat, tydat = ydat, exdat = exdat,
                     bws = npfit$bws)$mean

  # Function for performing Step 3
  boot_function <- function(data, indices) {

    np::npreg(txdat = xdat[indices,], tydat = ydat[indices],
              exdat = exdat, bws = npfit$bws)$mean

  }

  # Carry out Step 3
  m_hat_star <- boot::boot(data = data.frame(xdat), statistic = boot_function,
                           R = B)$t

  # Confidence intervals
  alpha <- 1 - conf
  if (type_CI == "standard") {

    z <- qnorm(p = 1 - alpha / 2)
    se <- apply(m_hat_star, 2, sd)
    lwr <- m_hat - z * se
    upr <- m_hat + z * se

  } else if (type_CI == "quantiles") {

    q <- apply(m_hat_star, 2, quantile, probs = c(alpha / 2, 1 - alpha / 2))
    lwr <- q[1, ]
    upr <- q[2, ]

  } else {

    stop("Incorrect type_CI")

  }

  # Return evaluation points, estimates, and confidence intervals
  return(data.frame("exdat" = exdat, "m_hat" = m_hat, "lwr" = lwr, "upr" = upr))

}

# Obtain predictions and confidence intervals along a fine grid, using the
# same seed employed by np::npplot for proper comparison
set.seed(42)
ci1 <- np_pred_CI(npfit = fit1, B = B, exdat = seq(5, 10, by = 0.01),
                  type_CI = "quantiles")

# Reconstruction of np::npplot's figure -- the curves coincide perfectly
plot(fit1, plot.errors.method = "bootstrap", plot.errors.type = "quantiles",
     plot.errors.boot.num = B, plot.errors.style = "bar", lwd = 3)
lines(npplot_qua$r1$eval[, 1], npplot_qua$r1$mean + npplot_qua$r1$merr[, 1],
      col = 2, lwd = 3)
lines(npplot_qua$r1$eval[, 1], npplot_qua$r1$mean + npplot_qua$r1$merr[, 2],
      col = 2, lwd = 3)
lines(ci1$exdat, ci1$m_hat, col = 3)
lines(ci1$exdat, ci1$lwr, col = 4)
lines(ci1$exdat, ci1$upr, col = 4)

# But np_pred_CI is also valid for regression models with several predictors!
# An example with bivariate local linear regression with CV bandwidth
bw2 <- np::npregbw(formula = growth ~ initgdp + popgro, data = oecdpanel,
                   regtype = "ll")
fit2 <- np::npreg(bw2)

# Predictions and confidence intervals along a bivariate grid
L <- 50
x_initgdp <- seq(5.5, 10, l = L)
x_popgro <- seq(-3.2, -2.3, l = L)
exdat <- expand.grid(x_initgdp, x_popgro)
ci2 <- np_pred_CI(npfit = fit2, exdat = exdat)

# Regression surface. Observe the extrapolatory artifacts for
# low-density regions
m_hat <- matrix(ci2$m_hat, nrow = L, ncol = L)
filled.contour(x_initgdp, x_popgro, m_hat, nlevels = 20,
               color.palette = viridis::viridis,
               plot.axes = {
                 axis(1); axis(2);
                 points(popgro ~ initgdp, data = oecdpanel, pch = 16)
                 })

# Length of the 95%-confidence intervals for the regression. Observe how
# they grow for low-density regions
ci_dif <- matrix(ci2$upr - ci2$lwr, nrow = L, ncol = L)
filled.contour(x_initgdp, x_popgro, ci_dif, nlevels = 20,
               color.palette = function(n) viridis::viridis(n, direction = -1),
               plot.axes = {
                 axis(1); axis(2);
                 points(popgro ~ initgdp, data = oecdpanel, pch = 16)
                 })












## ---- ll-1, eval = TRUE-----------------------------------------------------------
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
  fit_glm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    glm.fit(x = cbind(1, X - x), y = Y, weights = K,
            family = binomial())$coefficients[1]
  })
)

# Approach 2: optimize directly the weighted log-likelihood
suppressWarnings(
  fit_nlm <- sapply(x, function(x) {
    K <- dnorm(x = x, mean = X, sd = h)
    nlm(f = function(beta) {
      -sum(K * (Y * (beta[1] + beta[2] * (X - x)) -
                  log(1 + exp(beta[1] + beta[2] * (X - x)))))
      }, p = c(0, 0))$estimate[1]
  })
)

# Approach 3: employ locfit::locfit
# Bandwidth can not be controlled explicitly -- only through nn in ?lp
fit_locfit <- locfit::locfit(Y ~ locfit::lp(X, deg = 1, nn = h),
                             family = "binomial", kern = "gauss")

# Compare fits
plot(x, p(x), ylim = c(0, 1.5), type = "l", lwd = 2)
lines(x, logistic(fit_glm), col = 2)
lines(x, logistic(fit_nlm), col = 3, lty = 2)
plot(fit_locfit, add = TRUE, col = 4)
legend("topright", legend = c("p(x)", "glm", "nlm", "locfit"), lwd = 2,
       col = c(1, 2, 3, 4), lty = c(1, 1, 2, 1))


## ---- ll-2, eval = TRUE-----------------------------------------------------------
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

