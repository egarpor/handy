
## ----------------------------------------------------------------------------
## Name: 04-lm-iii.R
## Description: Script for Chapter 4 of "Notes for Predictive Modeling"
## Link: https://egarpor.github.io/PM-UC3M//
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 5.12.4
## ----------------------------------------------------------------------------

## ----norms-3, echo = FALSE, eval = knitr:::is_html_output()-------------------
# Graphing function
M <- plot3D::mesh(x = seq(0, 1, l = 20), y = seq(-pi, pi, l = 4 * 4 + 1))
x <- M$x
y <- M$y
graph <- function(r, f = 1, col, ...) {

  plot3Drgl::surf3Drgl(f * sign(x) * (x^2)^(1/r),
                       f * (1 - x^2)^(1/r) * sign(cos(y)) * (cos(y)^2)^(1/r),
                       f * (1 - x^2)^(1/r) * sign(sin(y)) * (sin(y)^2)^(1/r),
                       smooth = FALSE, lighting = FALSE, col = col,
                       alpha = 0.35, border = "black", ...)

}

# Plot
graph(r = 3, f = 1, col = "green", xlim = c(-1.25, 1.25),
      ylim = c(-1.25, 1.25), zlim = c(-1.25, 1.25))
graph(r = 2, f = 1, col = "blue", add = TRUE)
graph(r = 1, f = 1, col = "red", add = TRUE)
rgl::par3d(userMatrix = matrix(c(0.30811620, -0.95120299, -0.01665032, 0,
                                 0.09989159, 0.01494217, 0.99488610, 0,
                                 -0.94608986, -0.30820376, 0.09962107, 0,
                                 0, 0, 0, 1),
                               nrow = 4, ncol = 4, byrow = TRUE))
rgl::rglwidget()

## ----shrinkage----------------------------------------------------------------
# Load data -- baseball players statistics
data(Hitters, package = "ISLR")

# Discard NA's
Hitters <- na.omit(Hitters)

# The glmnet function works with the design matrix of predictors (without
# the ones). This can be obtained easily through model.matrix()
x <- model.matrix(Salary ~ 0 + ., data = Hitters)
# 0 + to exclude a column of 1's for the intercept, since the intercept will be
# added by default in glmnet::glmnet and if we do not exclude it here we will
# end with two intercepts, one of them resulting in NA. In the newer versions of
# glmnet this step is luckily not necessary

# Interestingly, note that in Hitters there are two-level factors and these
# are automatically transformed into dummy variables in x -- the main advantage
# of model.matrix
head(Hitters[, 14:20])
head(x[, 14:19])

# We also need the vector of responses
y <- Hitters$Salary

## ----modelmatrix--------------------------------------------------------------
# Data with NA in the first observation
data_na <- data.frame("x1" = rnorm(3), "x2" = rnorm(3), "y" = rnorm(3))
data_na$x1[1] <- NA

# The first observation disappears!
model.matrix(y ~ 0 + ., data = data_na)

# Still ignores NA's
model.matrix(y ~ 0 + ., data = data_na, na.action = "na.pass")

# Does not ignore NA's
model.matrix.lm(y ~ 0 + ., data = data_na, na.action = "na.pass")

## ----ridge-1------------------------------------------------------------------
# Call to the main function -- use alpha = 0 for ridge regression
library(glmnet)
ridgeMod <- glmnet(x = x, y = y, alpha = 0)
# By default, it computes the ridge solution over a set of lambdas
# automatically chosen. It also standardizes the variables by default to make
# the model fitting since the penalization is scale-sensitive. Importantly,
# the coefficients are returned on the original scale of the predictors

# Plot of the solution path -- gives the value of the coefficients for different
# measures in xvar (penalization imposed to the model or fitness)
plot(ridgeMod, xvar = "norm", label = TRUE)
# xvar = "norm" is the default: L1 norm of the coefficients sum_j abs(beta_j)

# Versus lambda
plot(ridgeMod, label = TRUE, xvar = "lambda")

# Versus the percentage of deviance explained -- this is a generalization of the
# R^2 for generalized linear models. Since we have a linear model, this is the
# same as the R^2
plot(ridgeMod, label = TRUE, xvar = "dev")
# The maximum R^2 is slightly above 0.5

# Indeed, we can see that R^2 = 0.5461
summary(lm(Salary ~., data = Hitters))$r.squared

# Some persistently important predictors are 16, 14, and 15
colnames(x)[c(16, 14, 15)]

# What is inside glmnet's output?
names(ridgeMod)

# lambda versus R^2 -- fitness decreases when sparsity is introduced, in
# in exchange of better variable interpretation and avoidance of overfitting
plot(log(ridgeMod$lambda), ridgeMod$dev.ratio, type = "l",
     xlab = "log(lambda)", ylab = "R2")
ridgeMod$dev.ratio[length(ridgeMod$dev.ratio)]
# Slightly different to lm's because it compromises accuracy for speed

# The coefficients for different values of lambda are given in $a0 (intercepts)
# and $beta (slopes) or, alternatively, both in coef(ridgeMod)
length(ridgeMod$a0)
dim(ridgeMod$beta)
length(ridgeMod$lambda) # 100 lambda's were automatically chosen

# Inspecting the coefficients associated with the 50th lambda
coef(ridgeMod)[, 50]
ridgeMod$lambda[50]

# Zoom in path solution
plot(ridgeMod, label = TRUE, xvar = "lambda",
     xlim = log(ridgeMod$lambda[50]) + c(-2, 2), ylim = c(-30, 10))
abline(v = log(ridgeMod$lambda[50]))
points(rep(log(ridgeMod$lambda[50]), nrow(ridgeMod$beta)), ridgeMod$beta[, 50],
       pch = 16, col = 1:6)

# The squared l2-norm of the coefficients decreases as lambda increases
plot(log(ridgeMod$lambda), sqrt(colSums(ridgeMod$beta^2)), type = "l",
     xlab = "log(lambda)", ylab = "l2 norm")

## ----ridge-2------------------------------------------------------------------
# If we want, we can choose manually the grid of penalty parameters to explore
# The grid should be descending
ridgeMod2 <- glmnet(x = x, y = y, alpha = 0, lambda = 100:1)
plot(ridgeMod2, label = TRUE, xvar = "lambda") # Not a good choice!

# Lambda is a tuning parameter that can be chosen by cross-validation, using as
# error the MSE (other possible error can be considered for generalized models
# using the argument type.measure)

# 10-fold cross-validation. Change the seed for a different result
set.seed(12345)
kcvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10)

# The lambda that minimizes the CV error is
kcvRidge$lambda.min

# Equivalent to
indMin <- which.min(kcvRidge$cvm)
kcvRidge$lambda[indMin]

# The minimum CV error
kcvRidge$cvm[indMin]
min(kcvRidge$cvm)

# Potential problem! Minimum occurs at one extreme of the lambda grid in which
# CV is done. The grid was automatically selected, but can be manually inputted
range(kcvRidge$lambda)
lambdaGrid <- 10^seq(log10(kcvRidge$lambda[1]), log10(0.1),
                     length.out = 150) # log-spaced grid
kcvRidge2 <- cv.glmnet(x = x, y = y, nfolds = 10, alpha = 0,
                       lambda = lambdaGrid)

# Much better
plot(kcvRidge2)
kcvRidge2$lambda.min

# But the CV curve is random, since it depends on the sample. Its variability
# can be estimated by considering the CV curves of each fold. An alternative
# approach to select lambda is to choose the largest within one standard
# deviation of the minimum error, in order to favor simplicity of the model
# around the optimal lambda value. This is known as the "one standard error rule"
kcvRidge2$lambda.1se

# Location of both optimal lambdas in the CV loss function in dashed vertical
# lines, and lowest CV error and lowest CV error + one standard error
plot(kcvRidge2)
indMin2 <- which.min(kcvRidge2$cvm)
abline(h = kcvRidge2$cvm[indMin2] + c(0, kcvRidge2$cvsd[indMin2]))
# The consideration of the one standard error rule for selecting lambda makes
# special sense when the CV function is quite flat around the minimum (hence an
# overpenalization that gives more sparsity does not affect so much the CV loss)

# Leave-one-out cross-validation. More computationally intense but completely
# objective in the choice of the fold-assignment
ncvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = nrow(Hitters),
                      lambda = lambdaGrid)

# Location of both optimal lambdas in the CV loss function
plot(ncvRidge)

## ----ridge-3------------------------------------------------------------------
# Inspect the best models (the glmnet fit is inside the output of cv.glmnet)
plot(kcvRidge2$glmnet.fit, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvRidge2$lambda.min, kcvRidge2$lambda.1se)))

# The model associated with `lambda.1se` (or any other lambda not included in the
# original path solution -- obtained by an interpolation) can be retrieved with
predict(kcvRidge2, type = "coefficients", s = kcvRidge2$lambda.1se)

# Alternatively, we can use coef()
coef(kcvRidge2, s = kcvRidge2$lambda.1se)

# Predictions for the first two observations
predict(kcvRidge2, type = "response", s = kcvRidge2$lambda.1se,
        newx = x[1:2, ])

# Predictions for the first observation, for all the lambdas. We can see how
# the prediction for one observation changes according to lambda
plot(log(kcvRidge2$lambda),
     predict(kcvRidge2, type = "response", newx = x[1, , drop = FALSE],
             s = kcvRidge2$lambda),
     type = "l", xlab = "log(lambda)", ylab = " Prediction")

## ----ridge-4------------------------------------------------------------------
# Random data
p <- 5
n <- 200
beta <- seq(-1, 1, l = p)
set.seed(123124)
x <- matrix(rnorm(n * p), n, p)
y <- 1 + x %*% beta + rnorm(n)

# Mimic internal standardization of y done in glmnet, which affects the scale
# of lambda in the regularization
y <- scale(y, center = TRUE, scale = TRUE) * sqrt(n / (n - 1))

# Unrestricted fit
fit <- glmnet(x, y, alpha = 0, lambda = 0, intercept = TRUE,
              standardize = FALSE)
beta0Hat <- rbind(fit$a0, fit$beta)
beta0Hat

# Unrestricted fit matches least squares -- but recall glmnet uses an
# iterative method so it is inexact (convergence threshold thresh = 1e-7 by
# default)
X <- model.matrix(y ~ x) # A way of constructing a design matrix that is a
# data.frame and has a column of ones
solve(crossprod(X)) %*% t(X) %*% y

# Restricted fit
# glmnet considers as the regularization parameter "lambda" the value
# lambda / n (lambda being here the penalty parameter employed in the theory)
lambda <- 2
fit <- glmnet(x, y, alpha = 0, lambda = lambda / n, intercept = TRUE,
              standardize = FALSE, thresh = 1e-10)
betaLambdaHat <- rbind(fit$a0, fit$beta)
betaLambdaHat

# Analytical form with intercept
solve(crossprod(X) + diag(c(0, rep(lambda, p)))) %*% t(X) %*% y

## ----lasso-1------------------------------------------------------------------
# Get the Hitters data back
x <- model.matrix(Salary ~ 0 + ., data = Hitters)
y <- Hitters$Salary

# Call to the main function -- use alpha = 1 for lasso regression (the default)
lassoMod <- glmnet(x = x, y = y, alpha = 1)
# Same defaults as before, same object structure

# Plot of the solution path -- now the paths are not smooth when decreasing to
# zero (they are zero exactly). This is a consequence of the l1 norm
plot(lassoMod, xvar = "lambda", label = TRUE)
# Some persistently important predictors are 16 and 14

# Versus the R^2 -- same maximum R^2 as before
plot(lassoMod, label = TRUE, xvar = "dev")

# Now the l1-norm of the coefficients decreases as lambda increases
plot(log(lassoMod$lambda), colSums(abs(lassoMod$beta)), type = "l",
     xlab = "log(lambda)", ylab = "l1 norm")

# 10-fold cross-validation. Change the seed for a different result
set.seed(12345)
kcvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)

# The lambda that minimizes the CV error
kcvLasso$lambda.min

# The "one standard error rule" for lambda
kcvLasso$lambda.1se

# Location of both optimal lambdas in the CV loss function
indMin <- which.min(kcvLasso$cvm)
plot(kcvLasso)
abline(h = kcvLasso$cvm[indMin] + c(0, kcvLasso$cvsd[indMin]))
# No problems now: the minimum does not occur at one extreme
# Interesting: note that the numbers on top of the figure give the number of
# coefficients *exactly* different from zero -- the number of predictors
# effectively considered in the model!
# In this case, the one standard error rule makes also sense

# Leave-one-out cross-validation
lambdaGrid <- 10^seq(log10(kcvLasso$lambda[1]), log10(0.1),
                     length.out = 150) # log-spaced grid
ncvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = nrow(Hitters),
                      lambda = lambdaGrid)

# Location of both optimal lambdas in the CV loss function
plot(ncvLasso)

## ----lasso-2------------------------------------------------------------------
# Inspect the best models
plot(kcvLasso$glmnet.fit, label = TRUE, xvar = "lambda")
abline(v = log(c(kcvLasso$lambda.min, kcvLasso$lambda.1se)))

# The model associated with `lambda.min` (or any other lambda not included in the
# original path solution -- obtained by an interpolation) can be retrieved with
predict(kcvLasso, type = "coefficients",
        s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se))

# Predictions for the first two observations
predict(kcvLasso, type = "response",
        s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se),
        newx = x[1:2, ])

## ----lasso-3------------------------------------------------------------------
# We can use lasso for model selection!
selPreds <- predict(kcvLasso, type = "coefficients",
                    s = c(kcvLasso$lambda.min, kcvLasso$lambda.1se))[-1, ] != 0
x1 <- x[, selPreds[, 1]]
x2 <- x[, selPreds[, 2]]

# Least squares fit with variables selected by lasso
modLassoSel1 <- lm(y ~ x1)
modLassoSel2 <- lm(y ~ x2)
summary(modLassoSel1)
summary(modLassoSel2)

# Comparison with stepwise selection
modBIC <- MASS::stepAIC(lm(Salary ~ ., data = Hitters), k = log(nrow(Hitters)),
                        trace = 0)
summary(modBIC)
# The lasso variable selection is similar, although the model is slightly worse
# in terms of adjusted R^2 and significance of the predictors. However, keep in
# mind that lasso is solving a constrained least squares problem, so it is
# expected to achieve better R^2 and adjusted R^2 via a selection procedure
# that employs solutions of unconstrained least squares. What is remarkable
# is the speed of lasso on selecting variables, and the fact that gives quite
# good starting points for performing further model selection

# Another interesting possibility is to run a stepwise selection starting from
# the set of predictors selected by lasso. In this search, it is important to
# use direction = "both" (default) and define the scope argument adequately
f <- formula(paste("Salary ~", paste(names(which(selPreds[, 2])),
                                     collapse = " + ")))
start <- lm(f, data = Hitters) # Model with predictors selected by lasso
scope <- list(lower = lm(Salary ~ 1, data = Hitters), # No predictors
              upper = lm(Salary ~ ., data = Hitters)) # All the predictors
modBICFromLasso <- MASS::stepAIC(object = start, k = log(nrow(Hitters)),
                                 scope = scope, trace = 0)
summary(modBICFromLasso)

# Comparison in terms of BIC, slight improvement with modBICFromLasso
BIC(modLassoSel1, modLassoSel2, modBICFromLasso, modBIC)

## ----lasso-4, fig.cap = '(ref:lasso-4-title)'---------------------------------
# Random data with predictors unrelated to the response
p <- 100
n <- 300
set.seed(123124)
x <- matrix(rnorm(n * p), n, p)
y <- 1 + rnorm(n)

# CV
lambdaGrid <- exp(seq(-10, 3, l = 200))
plot(cv.glmnet(x = x, y = y, alpha = 1, nfolds = n, lambda = lambdaGrid))

## ----constr-1-----------------------------------------------------------------
# Simulate data
set.seed(123456)
n <- 50
p <- 3
x1 <- rnorm(n, mean = 1)
x2 <- rnorm(n, mean = 2)
x3 <- rnorm(n, mean = 3)
eps <- rnorm(n, sd = 0.5)
y <- 1 + 2 * x1 - 3 * x2 + x3 + eps

# Center the data and compute design matrix
x1Cen <- x1 - mean(x1)
x2Cen <- x2 - mean(x2)
x3Cen <- x3 - mean(x3)
yCen <- y - mean(y)
X <- cbind(x1Cen, x2Cen, x3Cen)

# Linear restriction: use that
# beta_1 + beta_2 + beta_3 = 0
# beta_2 = -3
# In this case q = 2. The restriction is codified as
A <- rbind(c(1, 1, 1),
           c(0, 1, 0))
c <- c(0, -3)

# Fit model without intercept
S <- solve(crossprod(X))
beta_hat <- S %*% t(X) %*% yCen
beta_hat

# Restricted fit enforcing A * beta = c
beta_hat_A <- beta_hat +
  S %*% t(A) %*% solve(A %*% S %*% t(A)) %*% (c - A %*% beta_hat)
beta_hat_A

# Intercept of the constrained fit
beta_hat_A_0 <- mean(y) - c(mean(x1), mean(x2), mean(x3)) %*% beta_hat_A
beta_hat_A_0

## ----multr-1------------------------------------------------------------------
# Dimensions and sample size
p <- 3
q <- 2
n <- 100

# A quick way of creating a non-diagonal (valid) covariance matrix for the
# errors
Sigma <- 3 * toeplitz(seq(1, 0.1, l = q))
set.seed(12345)
X <- mvtnorm::rmvnorm(n = n, mean = 1:p, sigma = diag(0.5, nrow = p, ncol = p))
E <- mvtnorm::rmvnorm(n = n, mean = rep(0, q), sigma = Sigma)

# Linear model
B <- matrix((-1)^(1:p) * (1:p), nrow = p, ncol = q, byrow = TRUE)
Y <- X %*% B + E

# Fitting the model (note: Y and X are matrices!)
mod <- lm(Y ~ X)
mod
# Note that the intercept is markedly different from zero -- that is because
# X is not centered

# Compare with B
B

# Summary of the model: gives q separate summaries, one for each fitted
# univariate model
summary(mod)

# Exactly equivalent to
summary(lm(Y[, 1] ~ X))
summary(lm(Y[, 2] ~ X))

## ----multr-2------------------------------------------------------------------
# When we want to add several variables of a dataset as responses through a
# formula interface, we have to use cbind() in the response. Doing
# "Petal.Width + Petal.Length ~ ..." is INCORRECT, as lm will understand
# "I(Petal.Width + Petal.Length) ~ ..." and do one single regression

# Predict Petal's measurements from Sepal's
modIris <- lm(cbind(Petal.Width, Petal.Length) ~
                Sepal.Length + Sepal.Width + Species, data = iris)
summary(modIris)

# The fitted values and residuals are now matrices
head(modIris$fitted.values)
head(modIris$residuals)

# The individual models
modIris1 <- lm(Petal.Width ~Sepal.Length + Sepal.Width + Species, data = iris)
modIris2 <- lm(Petal.Length ~Sepal.Length + Sepal.Width + Species, data = iris)
summary(modIris1)
summary(modIris2)

## ----multr-3------------------------------------------------------------------
# Confidence intervals for the parameters
confint(modIris)
# Warning! Do not confuse Petal.Width:Sepal.Length with an interaction term!
# It is meant to represent the Response:Predictor coefficient

# Prediction -- now more limited without confidence intervals implemented
predict(modIris, newdata = iris[1:3, ])

# MANOVA table
manova(modIris)

# "Same" as the "Sum Sq" and "Df" entries of
anova(modIris1)
anova(modIris2)

# anova() serves for assessing the significance of including a new predictor
# for explaining all the responses. This is based on an extension of the
# *sequential* ANOVA table briefly covered in Section 2.6. The hypothesis test
# is by default conducted with the Pillai statistic (an extension of the F-test)
anova(modIris)

## ----multr-4------------------------------------------------------------------
# Simulate data
n <- 500
p <- 50
q <- 10
set.seed(123456)
X <- mvtnorm::rmvnorm(n = n, mean = p:1, sigma = 5 * 0.5^toeplitz(1:p))
E <- mvtnorm::rmvnorm(n = n, mean = rep(0, q), sigma = toeplitz(q:1))
B <- 5 * (2 / (0.5 * (1:p - 15)^2 + 2) + 1 / (0.1 * (1:p - 40)^2 + 1)) %*%
  t(1 / sqrt(1:q))
Y <- X %*% B + E

# Visualize B -- dark violet is close to 0
image(1:q, 1:p, t(B), col = viridisLite::viridis(20), xlab = "q", ylab = "p")

# Lasso path fit
mfit <- glmnet(x = X, y = Y, family = "mgaussian", alpha = 1)

# A list of models for each response
str(mfit$beta, 1)

# Tuning parameter selection by 10-fold cross-validation
set.seed(12345)
kcvLassoM <- cv.glmnet(x = X, y = Y, family = "mgaussian", alpha = 1)
kcvLassoM$lambda.min
kcvLassoM$lambda.1se

# Location of both optimal lambdas in the CV loss function
indMin <- which.min(kcvLassoM$cvm)
plot(kcvLassoM)
abline(h = kcvLassoM$cvm[indMin] + c(0, kcvLassoM$cvsd[indMin]))

# Extract the coefficients associated with some fits
coefs <- predict(kcvLassoM, type = "coefficients",
                 s = c(kcvLassoM$lambda.min, kcvLassoM$lambda.1se))
str(coefs, 1)

# Predictions for the first two observations
preds <- predict(kcvLassoM, type = "response",
                 s = c(kcvLassoM$lambda.min, kcvLassoM$lambda.1se),
                 newx = X[1:2, ])
preds

## ----multr-5, eval = FALSE----------------------------------------------------
# manipulate::manipulate({
# 
#   # Color
#   col <- viridisLite::viridis(20)
# 
#   # Common zlim
#   zlim <- range(B) + c(-0.25, 0.25)
# 
#   # Plot true B
#   par(mfrow = c(1, 2))
#   image(1:q, 1:p, t(B), col = col, xlab = "q", ylab = "p", zlim = zlim,
#         main = "B")
# 
#   # Extract B_hat from the lasso fit, a p x q matrix
#   B_hat <- sapply(seq_along(mfit$beta), function(i) mfit$beta[i][[1]][, j])
# 
#   # Put as black rows the predictors included
#   not_zero <- abs(B_hat) > 0
#   image(1:q, 1:p, t(not_zero), breaks = c(0.5, 1),
#         col = rgb(1, 1, 1, alpha = 0.1), add = TRUE)
# 
#   # For B_hat
#   image(1:q, 1:p, t(B_hat), col = col, xlab = "q", ylab = "p", zlim = zlim,
#         main = "Bhat")
#   image(1:q, 1:p, t(not_zero), breaks = c(0.5, 1),
#         col = rgb(1, 1, 1, alpha = 0.1), add = TRUE)
# 
# }, j = manipulate::slider(min = 1, max = ncol(mfit$beta$y1), step = 1,
#                           label = "j in lambda(j)"))

## ----biglm-1------------------------------------------------------------------
# Not really "big data", but for the sake of illustration
set.seed(12345)
n <- 1e6
p <- 10
beta <- seq(-1, 1, length.out = p)^5
x1 <- matrix(rnorm(n * p), nrow = n, ncol = p)
x1[, p] <- 2 * x1[, 1] + rnorm(n, sd = 0.1) # Add some dependence to predictors
x1[, p - 1] <- 2 - x1[, 2] + rnorm(n, sd = 0.5)
y1 <- 1 + x1 %*% beta + rnorm(n)
x2 <- matrix(rnorm(100 * p), nrow = 100, ncol = p)
y2 <- 1 + x2 %*% beta + rnorm(100)
bigData1 <- data.frame("resp" = y1, "pred" = x1)
bigData2 <- data.frame("resp" = y2, "pred" = x2)

# biglm has a very similar syntax to lm -- but the formula interface does not
# work always as expected
# biglm::biglm(formula = resp ~ ., data = bigData1) # Does not work
# biglm::biglm(formula = y ~ x) # Does not work
# biglm::biglm(formula = resp ~ pred.1 + pred.2, data = bigData1) # Does work,
# but not very convenient for a large number of predictors
# Hack for automatic inclusion of all the predictors
f <- formula(paste("resp ~", paste(names(bigData1)[-1], collapse = " + ")))
biglmMod <- biglm::biglm(formula = f, data = bigData1)

# lm's call
lmMod <- lm(formula = resp ~ ., data = bigData1)

# The reduction in size of the resulting object is more than notable
print(object.size(biglmMod), units = "KB")
print(object.size(lmMod), units = "MB")

# Summaries
s1 <- summary(biglmMod)
s2 <- summary(lmMod)
s1
s2

# Further information
s1$mat # Coefficients and their inferences
s1$rsq # R^2
s1$nullrss # SST (as in Section 2.6)

# Extract coefficients
coef(biglmMod)

# Prediction works as usual
predict(biglmMod, newdata = bigData2[1:5, ])

# Must contain a column for the response
# predict(biglmMod, newdata = bigData2[1:5, -1]) # Error

# Update the model with training data
update(biglmMod, moredata = bigData2)

# AIC and BIC
AIC(biglmMod, k = 2)
AIC(biglmMod, k = log(n))

# Features not immediately available for biglm objects: stepwise selection by
# stepAIC, residuals, variance of the error, model diagnostics, and vifs

# Workaround for obtaining hat(sigma)^2 = SSE / (n - p - 1), SSE = SST * (1 - R^2)
(s1$nullrss * (1 - s1$rsq)) / s1$obj$df.resid
s2$sigma^2

## ----biglm-2, fig.cap = '(ref:biglm-2-title)', fig.margin = FALSE-------------
# Model selection adapted to big data models
reg <- leaps::regsubsets(biglmMod, nvmax = p, method = "exhaustive")
plot(reg) # Plot best model (top row) to worst model (bottom row)

# Summarize (otherwise regsubsets's output is hard to decipher)
subs <- summary(reg)
subs

# Lots of useful information
str(subs, 1)

# Get the model with lowest BIC
subs$which
subs$bic
subs$which[which.min(subs$bic), ]

# Show the display in Figure 4.6
subs$which[order(subs$bic), ]

# It also works with ordinary linear models and it is much faster and
# informative than stepAIC
reg <- leaps::regsubsets(resp ~ ., data = bigData1, nvmax = p,
                         method = "backward")
subs <- summary(reg)
subs$bic
subs$which[which.min(subs$bic), ]

# Compare it with stepAIC
MASS::stepAIC(lm(resp ~ ., data = bigData1), trace = 0,
              direction = "backward", k = log(n))

## ----biglm-3------------------------------------------------------------------
# Size of the response
print(object.size(rnorm(1e6)) * 1e2, units = "GB")

# Size of the predictors
print(object.size(rnorm(1e6)) * 1e2 * 10, units = "GB")

## ----biglm-4------------------------------------------------------------------
# Linear regression with n = 10^8 and p = 10
n <- 10^8
p <- 10
beta <- seq(-1, 1, length.out = p)^5

# Number of chunks for splitting the dataset
nChunks <- 1e3
nSmall <- n / nChunks

# Simulates reading the first chunk of data
set.seed(12345)
x <- matrix(rnorm(nSmall * p), nrow = nSmall, ncol = p)
x[, p] <- 2 * x[, 1] + rnorm(nSmall, sd = 0.1)
x[, p - 1] <- 2 - x[, 2] + rnorm(nSmall, sd = 0.5)
y <- 1 + x %*% beta + rnorm(nSmall)

# First fit
bigMod <- biglm::biglm(y ~ x, data = data.frame(y, x))

# Update fit
# pb <- txtProgressBar(style = 3)
for (i in 2:nChunks) {

  # Simulates reading the i-th chunk of data
  set.seed(12345 + i)
  x <- matrix(rnorm(nSmall * p), nrow = nSmall, ncol = p)
  x[, p] <- 2 * x[, 1] + rnorm(nSmall, sd = 0.1)
  x[, p - 1] <- 2 - x[, 2] + rnorm(nSmall, sd = 0.5)
  y <- 1 + x %*% beta + rnorm(nSmall)

  # Update the fit
  bigMod <- update(bigMod, moredata = data.frame(y, x))

  # Progress
  # setTxtProgressBar(pb = pb, value = i / nChunks)

}

# Final model
summary(bigMod)
print(object.size(bigMod), units = "KB")

