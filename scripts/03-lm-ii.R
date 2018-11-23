
## ------------------------------------------------------------------------
## Name: 03-lm-ii.R
## Description: Script for Chapter 3 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- case2-2, eval = FALSE----------------------------------------------
## # Read data
## Boston <- readxl::read_excel(path = "Boston.xlsx", sheet = 1, col_names = TRUE)
## 
## # # Alternatively
## # data(MASS::Boston)

## ---- case2-3------------------------------------------------------------
summary(Boston)

## ---- scat2, fig.cap = '(ref:scat2-title)', fig.margin = FALSE-----------
car::scatterplotMatrix(~ crim + dis + medv + nox + rm, regLine = list(col = 2),
                       col = 1, smooth = list(col.smooth = 4, col.spread = 4), 
                       data = Boston)

## ---- case2-4------------------------------------------------------------
# Two models with different predictors
mod1 <- lm(medv ~ age + crim, data = Boston)
mod2 <- lm(medv ~ age + crim + lstat, data = Boston)

# BICs
BIC(mod1)
BIC(mod2) # Smaller -> better

# AICs
AIC(mod1)
AIC(mod2) # Smaller -> better

# Check the summaries
summary(mod1)
summary(mod2)

## ---- case2-6, eval = FALSE----------------------------------------------
## # Load data - notice that "Year" is also included
## wine <- read.csv(file = "wine.csv", header = TRUE)

## ---- bic-1--------------------------------------------------------------
# Full model
mod <- lm(Price ~ ., data = wine)

# With BIC
modBIC <- MASS::stepAIC(mod, k = log(nrow(wine)))
summary(modBIC)

# With AIC
modAIC <- MASS::stepAIC(mod, k = 2)
summary(modAIC)

## ---- bic-2--------------------------------------------------------------
# Different search directions and omitting the trace,
# gives only the final model
modAICFor <- MASS::stepAIC(mod, trace = 0, direction = "forward")
modAICBack <- MASS::stepAIC(mod, trace = 0, direction = "backward")
modAICFor
modAICBack

## ---- case2-7------------------------------------------------------------
modHouse <- lm(medv ~ ., data = Boston)
summary(modHouse)

## ---- case2-8------------------------------------------------------------
# Best models
modBIC <- MASS::stepAIC(modHouse, k = log(nrow(Boston)))
modAIC <- MASS::stepAIC(modHouse, trace = 0, k = 2)

# Comparison
car::compareCoefs(modBIC, modAIC)
summary(modBIC)

# Confidence intervals
confint(modBIC)

## ---- qua----------------------------------------------------------------
# iris dataset - factors in the last column
summary(iris)

# Summary of a linear model
mod1 <- lm(Sepal.Length ~ ., data = iris)
summary(mod1)
# Speciesversicolor (D1) coefficient: -0.72356. The average increment of
# Sepal.Length when the species is versicolor instead of setosa (reference)
# Speciesvirginica (D2) coefficient: -1.02350. The average increment of
# Sepal.Length when the species is virginica instead of setosa (reference)
# Both dummy variables are significant

# How to set a different level as reference (versicolor)
iris$Species <- relevel(iris$Species, ref = "versicolor")

# Same estimates except for the dummy coefficients
mod2 <- lm(Sepal.Length ~ ., data = iris)
summary(mod2)
# Speciessetosa (D1) coefficient: 0.72356. The average increment of
# Sepal.Length when the species is setosa instead of versicolor (reference)
# Speciesvirginica (D2) coefficient: -0.29994. The average increment of
# Sepal.Length when the species is virginica instead of versicolor (reference)
# Both dummy variables are significant

# Coefficients of the model
confint(mod2)
# The coefficients of Speciesversicolor and Speciesvirginica are 
# significantly negative

# Show the dummy variables employed for encoding a factor
contrasts(iris$Species)
iris$Species <- relevel(iris$Species, ref = "setosa")
contrasts(iris$Species)

## ---- case2-9------------------------------------------------------------
# Load the Boston dataset
data(MASS::Boston)

# Structure of the data
str(Boston)
# chas is a dummy variable measuring if the suburb is close to the river (1)
# or not (0). In this case it is not codified as a factor but as a 0 or 1
# (so it is already dummyfied)

# Summary of a linear model
mod <- lm(medv ~ chas + crim, data = Boston)
summary(mod)
# The coefficient associated to chas is 5.57772. That means that if the suburb
# is close to the river, the mean of medv increases in 5.57772 units for
# the same house and neighborhood conditions
# chas is significant (the presence of the river adds a valuable information
# for explaining medv)

# Summary of the best model in terms of BIC
summary(modBIC)
# The coefficient associated to chas is 2.71871. If the suburb is close to
# the river, the mean of medv increases in 2.71871 units
# chas is significant as well in the presence of more predictors

## ---- nonlineartransf, echo = FALSE, fig.cap = '(ref:nonlineartransf-title)', fig.margin = FALSE, fig.show = 'hold', fig.purl = FALSE----
x <- seq(-2, 5, l = 200)
plot(x, x, xlab = "x", ylab = "y", type = "l", col = 1, lwd = 2)
lines(x, x^2, col = 2, lwd = 2)
lines(x, x^3, col = 3, lwd = 2)
lines(x, sqrt(x), col = 4, lwd = 2)
lines(x, exp(x), col = 5, lwd = 2)
lines(x, exp(-x), col = 6, lwd = 2)
lines(x, log(x), col = 7, lwd = 2)
legend("bottomright", legend = expression(y == x, y == x^2, y == x^3, y == sqrt(x),
                                          y == exp(x), y == exp(-x), y == log(x)),
       lwd = 2, col = 1:7)
plot(x, -x, xlab = "x", ylab = "y", type = "l", col = 1, lwd = 2)
lines(x, -x^2, col = 2, lwd = 2)
lines(x, -x^3, col = 3, lwd = 2)
lines(x, -sqrt(x), col = 4, lwd = 2)
lines(x, -exp(x), col = 5, lwd = 2)
lines(x, -exp(-x), col = 6, lwd = 2)
lines(x, -log(x), col = 7, lwd = 2)
legend("topright", legend = expression(y == -x, y == -x^2, y == -x^3, y == -sqrt(x),
                                       y == -exp(-x), y == -exp(x), y == -log(x)),
       lwd = 2, col = 1:7)

## ---- datatra------------------------------------------------------------
# Data
x <- c(-2, -1.9, -1.7, -1.6, -1.4, -1.3, -1.1, -1, -0.9, -0.7, -0.6,
       -0.4, -0.3, -0.1, 0, 0.1, 0.3, 0.4, 0.6, 0.7, 0.9, 1, 1.1, 1.3,
       1.4, 1.6, 1.7, 1.9, 2, 2.1, 2.3, 2.4, 2.6, 2.7, 2.9, 3, 3.1,
       3.3, 3.4, 3.6, 3.7, 3.9, 4, 4.1, 4.3, 4.4, 4.6, 4.7, 4.9, 5)
y <- c(1.4, 0.4, 2.4, 1.7, 2.4, 0, 0.3, -1, 1.3, 0.2, -0.7, 1.2, -0.1,
       -1.2, -0.1, 1, -1.1, -0.9, 0.1, 0.8, 0, 1.7, 0.3, 0.8, 1.2, 1.1,
       2.5, 1.5, 2, 3.8, 2.4, 2.9, 2.7, 4.2, 5.8, 4.7, 5.3, 4.9, 5.1,
       6.3, 8.6, 8.1, 7.1, 7.9, 8.4, 9.2, 12, 10.5, 8.7, 13.5)

# Data frame (a matrix with column names)
nonLinear <- data.frame(x = x, y = y)

# We create a new column inside nonLinear, called x2, that contains the
# newvariable x^2
nonLinear$x2 <- nonLinear$x^2
# If you wish to remove it
# nonLinear$x2 <- NULL

# Regressions
mod1 <- lm(y ~ x, data = nonLinear)
mod2 <- lm(y ~ x2, data = nonLinear)
summary(mod1)
summary(mod2)
# mod2 has a larger R^2. Also notice the intercept is not significative

## ---- pol-1-1, fig.cap = '(ref:pol-1-1-title)'---------------------------
x1 <- seq(-1, 1, l = 5)
poly(x = x1, degree = 2, raw = TRUE) # (X, X^2)
poly(x = x1, degree = 2) # By default, it employs orthogonal polynomials

# Depiction of raw polynomials
x <- seq(-1, 1, l = 200)
degree <- 5
matplot(x, poly(x, degree = degree, raw = TRUE), type = "l", lty = 1,
        ylab = expression(x^k))
legend("bottomright", legend = paste("k =", 1:degree), col = 1:degree, lwd = 2)

## ---- pol-1-2, fig.cap = '(ref:pol-1-2-title)'---------------------------
# Depiction of orthogonal polynomials
matplot(x, poly(x, degree = degree), type = "l", lty = 1,
        ylab = expression(p[k](x)))
legend("bottomright", legend = paste("k =", 1:degree), col = 1:degree, lwd = 2)

## ---- pol-2-1, fig.cap = '(ref:pol-2-1-title)'---------------------------
# Data containing speed (mph) and stopping distance (ft) of cars from 1920
data(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")

# Fit a linear model of dist ~ speed
mod1 <- lm(dist ~ speed, data = cars)
abline(coef = mod1$coefficients, col = 2)

# Quadratic
mod2 <- lm(dist ~ poly(speed, degree = 2), data = cars)
# The fit is not a line, we must look for an alternative approach
d <- seq(0, 25, length.out = 200)
lines(d, predict(mod2, new = data.frame(speed = d)), col = 3)

# Cubic
mod3 <- lm(dist ~ poly(speed, degree = 3), data = cars)
lines(d, predict(mod3, new = data.frame(speed = d)), col = 4)

# 10th order - overfitting
mod10 <- lm(dist ~ poly(speed, degree = 10), data = cars)
lines(d, predict(mod10, new = data.frame(speed = d)), col = 5)

# BICs - the linear model is better!
BIC(mod1, mod2, mod3, mod10)

# poly computes by default orthogonal polynomials. These are not
# X^1, X^2, ..., X^p but combinations of them such that the polynomials are
# orthogonal. 'Raw' polynomials are possible with raw = TRUE. They give the
# same fit, but the coefficient estimates are different.
mod2Raw <- lm(dist ~ poly(speed, degree = 2, raw = TRUE), data = cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")
lines(d, predict(mod2, new = data.frame(speed = d)), col = 1)
lines(d, predict(mod2Raw, new = data.frame(speed = d)), col = 2)

## ---- pol-2-2, fig.cap = '(ref:pol-2-2-title)', fig.show = 'hold'--------
# However: different coefficient estimates, but same R^2. How is this possible?
summary(mod2)
summary(mod2Raw)

# Because the predictors in mod2Raw are highly related between them, and
# the ones in mod2 are uncorrelated between them!
car::scatterplotMatrix(mod2$model[, -1], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
car::scatterplotMatrix(mod2Raw$model[, -1],col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
cor(mod2$model[, -1])
cor(mod2Raw$model[, -1])

## ---- int-1--------------------------------------------------------------
# Interaction between lstat and age
summary(lm(medv ~ lstat + lstat:age, data = Boston))
# For a unit increment in age, the effect of lstat in the response
# increases positively by 0.004103 units, shifting from -1.388161 to -1.384058
# Thus, the fact that age increases makes lstat is affecting less
# negatively medv. Note that the same intepretation does NOT hold if we
# switching the roles of age and lstat because age is not present as a sole
# predictor!

# First order interaction
summary(lm(medv ~ lstat * age, data = Boston))

# Second order interaction
summary(lm(medv ~ lstat * age * indus, data = Boston))

## ---- int-2--------------------------------------------------------------
# Include first-order interactions in the search for the best model in
# terms of BIC, not just single predictors
modIntBIC <- MASS::stepAIC(object = lm(medv ~ ., data = Boston), 
                           scope = medv ~ .^2, k = log(nobs(modBIC)), trace = 0)
summary(modIntBIC)

# There is no improvement by removing terms in modIntBIC
MASS::dropterm(modIntBIC, k = log(nobs(modIntBIC)), sorted = TRUE)

# Neither by including other terms interactions
MASS::addterm(modIntBIC, scope = lm(medv ~ .^2, data = Boston),
              k = log(nobs(modIntBIC)), sorted = TRUE)

## ---- int-3--------------------------------------------------------------
# Group settings
col <- Boston$chas + 3
cex <- 0.5 + 0.25 * Boston$chas

# 1. No dummy variable
(mod1 <- lm(medv ~ lstat, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "1")
abline(coef = mod1$coefficients, lwd = 2)

# 2. Dummy variable
(mod2 <- lm(medv ~ lstat + chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "2")
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = 3, lwd = 2)
abline(a = mod2$coefficients[1] + mod2$coefficients[3],
       b = mod2$coefficients[2], col = 4, lwd = 2)

# 3. Dummy variable, with interaction
(mod3 <- lm(medv ~ lstat * chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "3")
abline(a = mod3$coefficients[1], b = mod3$coefficients[2], col = 3, lwd = 2)
abline(a = mod3$coefficients[1] + mod3$coefficients[3],
       b = mod3$coefficients[2] + mod3$coefficients[4], col = 4, lwd = 2)

# 4. Dummy variable only present in interaction
(mod4 <- lm(medv ~ lstat + lstat:chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "4")
abline(a = mod4$coefficients[1], b = mod4$coefficients[2], col = 3, lwd = 2)
abline(a = mod4$coefficients[1],
       b = mod4$coefficients[2] + mod4$coefficients[3], col = 4, lwd = 2)

# 5. Dummy variable and no predictor
(mod5 <- lm(medv ~ chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "5")
abline(a = mod5$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod5$coefficients[1] + mod5$coefficients[2], b = 0, col = 4, lwd = 2)

# 6. Dummy variable. Interaction in the intercept and slope
(mod6 <- lm(medv ~ chas + lstat:chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "6")
abline(a = mod6$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod6$coefficients[1] + mod6$coefficients[2],
       b = mod6$coefficients[3], col = 4, lwd = 2)

# 7. Dummy variable. Interaction in the intercept and slope
(mod7 <- lm(medv ~ lstat:chas, data = Boston))
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "7")
abline(a = mod7$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod7$coefficients[1], b = mod7$coefficients[2], col = 4, lwd = 2)

## ---- int-4--------------------------------------------------------------
# Model using a dummy variable in the full dataset
lm(medv ~ lstat + chas + lstat:chas, data = Boston)

# Individual model for the group with chas == 0
lm(medv ~ lstat, data = Boston, subset = chas == 0)
# Notice that the intecept and lstat coeffient are the same as before

# Individual model for the group with chas == 1
lm(medv ~ lstat, data = Boston, subset = chas == 1)
# Notice that the intecept and lstat coeffient equal the ones from the
# joint model, plus the specific terms associated to chas

## ---- int-5, fig.cap = '(ref:int-5-title)'-------------------------------
# Does not take into account the groups in the data
modIris <- lm(Sepal.Width ~ Petal.Width, data = iris)
modIris$coefficients

# Adding interactions with the groups
modIrisSpecies <- lm(Sepal.Width ~ Petal.Width * Species, data = iris)
modIrisSpecies$coefficients

# Joint regression line shows negative correlation, but each group
# regression line shows a positive correlation
plot(Sepal.Width ~ Petal.Width, data = iris, col = as.integer(Species) + 1,
     pch = 16)
abline(a = modIris$coefficients[1], b = modIris$coefficients[2], lwd = 2)
abline(a = modIrisSpecies$coefficients[1], b = modIrisSpecies$coefficients[2],
       col = 2, lwd = 2)
abline(a = modIrisSpecies$coefficients[1] + modIrisSpecies$coefficients[3],
       b = modIrisSpecies$coefficients[2] + modIrisSpecies$coefficients[5],
       col = 3, lwd = 2)
abline(a = modIrisSpecies$coefficients[1] + modIrisSpecies$coefficients[4],
       b = modIrisSpecies$coefficients[2] + modIrisSpecies$coefficients[6],
       col = 4, lwd = 2)

## ---- case2-11, eval = FALSE---------------------------------------------
## load("wine.RData")
## mod <- lm(Price ~ Age + AGST + HarvestRain + WinterRain, data = wine)
## summary(mod)

## ---- diag-1, fig.cap = '(ref:diag-1-title)'-----------------------------
plot(mod, 1)

## ---- diag-2, fig.cap = '(ref:diag-2-title)'-----------------------------
par(mfrow = c(2, 2)) # We have 4 predictors
termplot(mod, partial.resid = TRUE)

## ---- diag-3, fig.cap = '(ref:diag-3-title)', fig.asp = 1/2--------------
par(mfrow = c(1, 2))
plot(lm(y ~ x, data = nonLinear), 1) # Nonlinear
plot(lm(y ~ I(x^2), data = nonLinear), 1) # Linear

## ---- diag-4, fig.cap = '(ref:diag-4-title)'-----------------------------
plot(mod, 2)

## ---- diag-5-------------------------------------------------------------
# Shapiro-Wilk test of normality
shapiro.test(mod$residuals) # Allows up to 5000 observations - if dealing with 
# more data points, randomization of the input is a possibility
# We do not reject normality

# Lilliefors test - the Kolmogorov-Smirnov adaptation for testing normality
nortest::lillie.test(mod$residuals)
# We do not reject normality

## ---- box-yeo, fig.asp = 1/2, fig.margin = FALSE-------------------------
# Test data

# Predictors
n <- 200
set.seed(121938)
X1 <- rexp(n, rate = 1 / 5) # Non-negative
X2 <- rchisq(n, df = 5) - 3 # Real

# Response of a linear model
epsilon <- rchisq(n, df = 10) - 10 # Centered error, but not normal
Y <- 10 - 0.5 * X1 + X2 + epsilon

# Transformation of non-normal data to achieve normal-like data (no model)

# Optimal lambda for Box-Cox
BC <- car::powerTransform(lm(X1 ~ 1), family = "bcPower") # Maximum-likelihood fit
# Note we use a regression model with no predictors
(lambdaBC <- BC$lambda) # The optimal lambda
# lambda < 1, so positive skewness is corrected

# Box-Cox transformation
X1Transf <- car::bcPower(U = X1, lambda = lambdaBC)

# Comparison
par(mfrow = c(1, 2))
hist(X1, freq = FALSE, breaks = 10, ylim = c(0, 0.3))
hist(X1Transf, freq = FALSE, breaks = 10, ylim = c(0, 0.3))

# Optimal lambda for Yeo-Johnson
YJ <- car::powerTransform(lm(X2 ~ 1), family = "yjPower")
(lambdaYJ <- YJ$lambda)

# Yeo-Johnson transformation
X2Transf <- car::yjPower(U = X2, lambda = lambdaYJ)

# Comparison
par(mfrow = c(1, 2))
hist(X2, freq = FALSE, breaks = 10, ylim = c(0, 0.3))
hist(X2Transf, freq = FALSE, breaks = 10, ylim = c(0, 0.3))

# Transformation of non-normal response in a linear model

# Optimal lambda for Yeo-Johnson
YJ <- car::powerTransform(lm(Y ~ X1 + X2), family = "yjPower")
(lambdaYJ <- YJ$lambda)

# Yeo-Johnson transformation
YTransf <- car::yjPower(U = Y, lambda = lambdaYJ)

# Comparison for the residuals
par(mfrow = c(1, 2))
plot(lm(Y ~ X1 + X2), 2)
plot(lm(YTransf ~ X1 + X2), 2) # Slightly better

## ---- diag-6, fig.cap = '(ref:diag-6-title)'-----------------------------
plot(mod, 3)

## ---- diag-7-------------------------------------------------------------
# Breusch-Pagan test
car::ncvTest(mod)
# We do not reject homoscedasticity

## ---- breusch, fig.cap = '(ref:breusch-title)', fig.show = 'hold'--------
# Heteroskedastic models
set.seed(123456)
x <- rnorm(100)
y1 <- 1 + 2 * x + rnorm(100, sd = x^2)
y2 <- 1 + 2 * x + rnorm(100, sd = 1 + x * (x > 0))
modHet1 <- lm(y1 ~ x)
modHet2 <- lm(y2 ~ x)

# Heteroskedasticity not detected
car::ncvTest(modHet1)
plot(modHet1, 3)

# Heteroskedasticity correctly detected
car::ncvTest(modHet2)
plot(modHet2, 3)

## ---- diag-8, fig.cap = '(ref:diag-8-title)', fig.show = 'hold'----------
# Artificial data with heteroskedasticity
set.seed(12345)
X <- rchisq(500, df = 3)
e <- rnorm(500, sd = sqrt(0.1 + 2 * X))
Y <- 1 + X + e

# Original
plot(lm(Y ~ X), 3) # Very heteroskedastic

# Transformed
plot(lm(I(log(abs(Y))) ~ X), 3) # Much less hereroskedastic, but at the price
# of losing the signs in Y...

# Shifted and transformed
delta <- 1 # This is tuneable
m <- -min(Y) + delta
plot(lm(I(log(Y + m)) ~ X), 3) # No signs loss

# Transformed by Yeo-Johnson

# Optimal lambda for Yeo-Johnson
YJ <- car::powerTransform(lm(Y ~ X), family = "yjPower")
(lambdaYJ <- YJ$lambda)

# Yeo-Johnson transformation
YTransf <- car::yjPower(U = Y, lambda = lambdaYJ)
plot(lm(YTransf ~ X), 3) # Slightly less hereroskedastic

## ---- diag-9, fig.cap = '(ref:diag-9-title)'-----------------------------
plot(mod$residuals, type = "o")

## ---- diag-10------------------------------------------------------------
lag.plot(mod$residuals, lags = 1, do.lines = FALSE)
# No serious serial trend, but some negative autocorrelation is appreaciated
cor(mod$residuals[-1], mod$residuals[-length(mod$residuals)])

## ---- diag-11------------------------------------------------------------
# Durbin-Watson test
car::durbinWatsonTest(mod)
# Does not reject at alpha = 0.05

## ---- multico-1, message = FALSE, fig.cap = '(ref:multico-1-title)'------
# Numerically
cor(wine)

# Graphically
corrplot::corrplot(cor(wine), addCoef.col = "grey")

## ---- multico-2, fig.cap = '(ref:multico-2-title)'-----------------------
# Create predictors with multicollinearity: x4 depends on the rest
set.seed(45678)
x1 <- rnorm(100)
x2 <- 0.5 * x1 + rnorm(100)
x3 <- 0.5 * x2 + rnorm(100)
x4 <- -x1 + x2 + rnorm(100, sd = 0.25)

# Response
y <- 1 + 0.5 * x1 + 2 * x2 - 3 * x3 - x4 + rnorm(100)
data <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, y = y)

# Correlations - none seems suspicious
corrplot::corrplot(cor(data), addCoef.col = "grey")

## ---- multico-3----------------------------------------------------------
# Abnormal variance inflation factors: largest for x4, we remove it
modMultiCo <- lm(y ~ x1 + x2 + x3 + x4)
car::vif(modMultiCo)

# Without x4
modClean <- lm(y ~ x1 + x2 + x3)

# Comparison
car::compareCoefs(modMultiCo, modClean)
confint(modMultiCo)
confint(modClean)

# Sumamries
summary(modMultiCo)
summary(modClean)

# Variance inflation factors are normal
car::vif(modClean)

## ---- outl-1, fig.cap = '(ref:outl-1-title)'-----------------------------
plot(mod, 5)

## ---- outl-2-------------------------------------------------------------
# Create data
set.seed(12345)
x <- rnorm(100)
e <- rnorm(100, sd = 0.5)
y <- 1 + 2 * x + e

# Leverage expected value
2 / 101 # (p + 1) / n

# Base model
m0 <- lm(y ~ x)
plot(x, y)
abline(coef = m0$coefficients, col = 2)
plot(m0, 5)
summary(m0)

# Make an outlier
x[101] <- 0; y[101] <- 30
m1 <- lm(y ~ x)
plot(x, y)
abline(coef = m1$coefficients, col = 2)
plot(m1, 5)
summary(m1)

# Make a high-leverage point
x[101] <- 10; y[101] <- 5
m2 <- lm(y ~ x)
plot(x, y)
abline(coef = m2$coefficients, col = 2)
plot(m2, 5)
summary(m2)

## ---- outl-3-------------------------------------------------------------
# Access leverage statistics
head(influence(model = m2, do.coef = FALSE)$hat)

# Another option
h <- hat(x = x)

# 1% most influential points
n <- length(x)
p <- 1
hist(h, breaks = 20)
abline(v = (qchisq(0.99, df = p) + 1) / n, col = 2)

# Standardized residuals
rs <- rstandard(m2)
plot(m2, 2) # QQ-plot
points(qnorm(ppoints(n = n)), sort(rs), col = 2, pch = '+') # Manually computed

