
## ------------------------------------------------------------------------
## Name: 05-kre-ii.R
## Description: Script for Chapter 5 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- mult-1, eval = knitr:::is_html_output()------------------------------------------------------
## # Sample data from a bivariate regression
## n <- 300
## set.seed(123456)
## X <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
##                       sigma = matrix(c(2, 0.5, 0.5, 1.5), nrow = 2, ncol = 2))
## 
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


## ---- mult-2---------------------------------------------------------------------------------------
# Employing the wine dataset
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
apply(wine, 2, median)

# Therefore, conditionally on the median values of the predictors:
# - Age is positively related with Price (almost linearly)
# - WinterRain is positively related with Price (with a subtle nonlinearity)
# - AGST is positively related with Price, but now we see what it looks like a
#   quadratic pattern
# - HarvestRain is negatively related with Price (almost linearly)


## ---- mult-3---------------------------------------------------------------------------------------
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
apply(wine[c("Price", "WinterRain", "HarvestRain")], 2, quantile, prob = tau)




## ---- mix-1, fig.margin = FALSE, fig.asp = 1/2-----------------------------------------------------
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


## ---- mix-2, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3-------------------------------
# Load data
data(oecdpanel, package = "np")

# Bandwidth by CV for local constant -- use only two starts to reduce the
# computation time
bw_OECD <- np::npregbw(formula = growth ~ factor(oecd) + ordered(year) +
                         initgdp + popgro + inv + humancap, data = oecdpanel,
                       regtype = "lc", nmulti = 2)
bw_OECD

# Regression
fit_OECD <- np::npreg(bw_OECD)
summary(fit_OECD)

# Plot marginal effects of each predictor on the response
par(mfrow = c(2, 3))
plot(fit_OECD, plot.par.mfrow = FALSE)






## ---- predci, fig.fullwidth = TRUE, fig.margin = FALSE, fig.asp = 2/3------------------------------
# Asymptotic confidence bands for the marginal effects of each predictor on the
# response
par(mfrow = c(2, 3))
plot(fit_OECD, plot.errors.method = "asymptotic", common.scale = FALSE,
     plot.par.mfrow = FALSE)

# Bootstrap confidence bands
# They take more time to compute because a resampling + refitting takes place
par(mfrow = c(2, 3))
plot(fit_OECD, plot.errors.method = "bootstrap", plot.par.mfrow = FALSE)

# The asymptotic standard error associated to the regression evaluated at the
# evaluation points are in $merr
head(fit_OECD$merr)

# Recall that in $mean we had the regression evaluated at the evaluation points,
# by default the sample of the predictors, so in this case the same as the
# fitted values
head(fit_OECD$mean)

# Prediction for the first 3 points + standard errors
pred <- predict(fit_OECD, newdata = oecdpanel[1:3, ], se.fit = TRUE)

# Approximate (based on assuming asymptotic normality) 100(1 - alpha)% CI for
# the conditional mean of the first 3 points
alpha <- 0.05
pred$fit + (qnorm(1 - alpha / 2) * pred$se.fit) %o% c(-1, 1)




## ---- exr-pred-sol, echo = FALSE, eval = FALSE-----------------------------------------------------
## # Data
## data(Auto, package = "ISLR")
## set.seed(12345)
## ind_train <- sample(392, size = 380)
## train <- Auto[ind_train, ]
## validation <- Auto[-ind_train, ]
## 
## # Local constant fit
## bw0 <- np::npregbw(formula = mpg ~ ordered(cylinders) + horsepower + weight +
##                      factor(origin), data = train, regtype = "lc")
## kre0 <- np::npreg(bws = bw0)
## 
## # Local linear fit
## bw1 <- np::npregbw(formula = mpg ~ ordered(cylinders) + horsepower + weight +
##                      factor(origin), data = train, regtype = "ll")
## kre1 <- np::npreg(bws = bw1)
## 
## # Linear fit
## lm_mod <- lm(formula = mpg ~ ordered(cylinders) + horsepower + weight +
##                factor(origin), data = train)
## 
## # MSEP
## mean((validation$mpg - predict(kre0, newdata = validation))^2)
## mean((validation$mpg - predict(kre1, newdata = validation))^2)
## mean((validation$mpg - predict(lm_mod, newdata = validation))^2)




## ---- ll-1, eval = TRUE----------------------------------------------------------------------------
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
# Bandwidth can not be controlled explicitly - only through nn in ?lp
fit_locfit <- locfit::locfit(Y ~ locfit::lp(X, deg = 1, nn = h),
                             family = "binomial", kern = "gauss")

# Compare fits
plot(x, p(x), ylim = c(0, 1.5), type = "l", lwd = 2)
lines(x, logistic(fit_glm), col = 2)
lines(x, logistic(fit_nlm), col = 3, lty = 2)
plot(fit_locfit, add = TRUE, col = 4)
legend("topright", legend = c("p(x)", "glm", "nlm", "locfit"), lwd = 2,
       col = c(1, 2, 3, 4), lty = c(1, 1, 2, 1))


## ---- ll-2, eval = TRUE----------------------------------------------------------------------------
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

