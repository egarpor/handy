
## ------------------------------------------------------------------------
## Name: 05-glm.R
## Description: Script for Chapter 5 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- challenger-load, eval = FALSE--------------------------------------
## challenger <- read.table(file = "challenger.txt", header = TRUE, sep = "\t")

## ---- challengerfigs-----------------------------------------------------
car::scatterplot(nfails.field ~ temp, smooth = FALSE, boxplots = FALSE, 
                 data = challenger, subset = nfails.field > 0)
car::scatterplot(nfails.field ~ temp, smooth = FALSE, boxplots = FALSE, 
                 data = challenger)

## ---- nasadiag-----------------------------------------------------------
mod <- lm(nfails.field ~ temp, data = challenger)
plot(mod, 1)
plot(mod, 2)

## ---- logcurve-----------------------------------------------------------
# Logistic regression
nasa <- glm(fail.field ~ temp, family = "binomial", data = challenger)

# Plot data
plot(challenger$temp, challenger$fail.field, xlim = c(-1, 30),
     xlab = "Temperature", ylab = "Incident probability")

# Draw the fitted logistic curve
x <- seq(-1, 30, l = 200)
y <- exp(-(nasa$coefficients[1] + nasa$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)

# The Challenger
points(-0.6, 1, pch = 16)
text(-0.6, 1, labels = "Challenger", pos = 4)

## ---- logsummary---------------------------------------------------------
# Exponentiated coefficients ("odds ratios")
exp(coef(nasa))

## ---- modform-1----------------------------------------------------------
# Data
set.seed(34567)
x <- rnorm(50, sd = 1.5)
y1 <- -0.5 + 3 * x
y2 <- 0.5 - 2 * x
y3 <- -2 + 5 * x
y1 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y1)))
y2 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y2)))
y3 <- rbinom(50, size = 1, prob = 1 / (1 + exp(-y3)))

# Data
dataMle <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3)

## ---- modform-2, fig.cap = '(ref:modform-2-title)'-----------------------
# Call glm
mod <- glm(y1 ~ x, family = "binomial", data = dataMle)
mod$coefficients

# -loglik(beta)
minusLogLik <- function(beta) {
  p <- 1 / (1 + exp(-(beta[1] + beta[2] * x)))
  -sum(y1 * log(p) + (1 - y1) * log(1 - p))
}

# Optimization using as starting values beta = c(0, 0)
opt <- optim(par = c(0, 0), fn = minusLogLik)
opt

# Visualization of the minusLogLik surface
beta0 <- seq(-3, 3, l = 50)
beta1 <- seq(-2, 8, l = 50)
L <- matrix(nrow = length(beta0), ncol = length(beta1))
for (i in seq_along(beta0)) {
  for (j in seq_along(beta1)) {
    L[i, j] <- minusLogLik(c(beta0[i], beta1[j]))
  }
}
filled.contour(beta0, beta1, -L, color.palette = viridis::viridis, 
               xlab = expression(beta[0]), ylab = expression(beta[1]), 
               plot.axes = {
                 axis(1:2)
                 points(mod$coefficients[1], mod$coefficients[2], 
                        col = 2, pch = 16)
                 points(opt$par[1], opt$par[2], col = 4)
               }) 
# The plot.axes argument is a hack to add graphical information within the 
# coordinates of the main panel (behind filled.contour there is a layout()...)

## ---- pois-load, eval = FALSE--------------------------------------------
## species <- read.table("species.txt", header = TRUE)

## ---- poiscurve----------------------------------------------------------
# Data
plot(Species ~ Biomass, data = species, col = pH)
legend("topright", legend = c("Low pH", "Medium pH", "High pH"),
       col = 1:3, lwd = 2)

# Fit Poisson regression
species1 <- glm(Species ~ ., data = species, family = poisson)
summary(species1)
# Took 4 iterations of the IRLS

# Interpretation of the coefficients:
exp(species1$coefficients)
# - 46.9433 is the average number of species when Biomass = 0 and the pH is high
# - For each increment in one unit in Biomass, the number of species decreases
#   by a factor of 0.88 (12% reduction)
# - If pH decreases to med (low), then the number of species decreases by a factor
#   of 0.6407 (0.3209)

# With interactions
species2 <- glm(Species ~ Biomass * pH, data = species, family = poisson)
summary(species2)
exp(species2$coefficients)
# - If pH decreases to med (low), then the effect of the biomass in the number
#   of species decreases by a factor of 0.8564 (0.9686). The higher the pH, the
#   stronger the effect of the Biomass in Species

# Draw fits
plot(Species ~ Biomass, data = species, col = pH)
legend("topright", legend = c("High pH", "Medium pH", "Low pH"),
       col = 1:3, lwd = 2)

# Without interactions
bio <- seq(0, 10, l = 100)
z <- species1$coefficients[1] + species1$coefficients[4] * bio
lines(bio, exp(z), col = 1)
lines(bio, exp(species1$coefficients[2] + z), col = 2)
lines(bio, exp(species1$coefficients[3] + z), col = 3)

# With interactions seems to provide a significant improvement
bio <- seq(0, 10, l = 100)
z <- species2$coefficients[1] + species2$coefficients[2] * bio
lines(bio, exp(z), col = 1, lty = 2)
lines(bio, exp(species2$coefficients[3] + species2$coefficients[5] * bio + z),
      col = 2, lty = 2)
lines(bio, exp(species2$coefficients[4] + species2$coefficients[6] * bio  + z),
      col = 3, lty = 2)

## ---- nasa-case-1, message = FALSE---------------------------------------
# Summary of the model
summary(nasa)

# Confidence intervals at 95%
confint.default(nasa)

# Confidence intervals at other levels
confint.default(nasa, level = 0.90)

# Confidence intervals for the factors affecting the odds
exp(confint.default(nasa))

## ---- testcoef-1---------------------------------------------------------
# Significances with asymptotic approximation for the standard errors
summary(nasa)

# CIs with asymptotic approximation - coherent with summary
confint.default(nasa, level = 0.95)
confint.default(nasa, level = 0.99)

# CIs with profile likelihood - incoherent with summary
confint(nasa, level = 0.95) # intercept still significant
confint(nasa, level = 0.99) # temp still significant

## ---- pred-case-1--------------------------------------------------------
predict(nasa, newdata = data.frame(temp = -0.6), type = "response")

## ---- pred-case-2--------------------------------------------------------
# Function for computing the predictions and CIs for the conditional probability
predictCIsLogistic <- function(object, newdata, level = 0.95) {

  # Compute predictions in the log-odds
  pred <- predict(object = object, newdata = newdata, se.fit = TRUE)

  # CI in the log-odds
  za <- qnorm(p = (1 - level) / 2)
  lwr <- pred$fit + za * pred$se.fit
  upr <- pred$fit - za * pred$se.fit

  # Transform to probabilities
  fit <- 1 / (1 + exp(-pred$fit))
  lwr <- 1 / (1 + exp(-lwr))
  upr <- 1 / (1 + exp(-upr))

  # Return a matrix with column names "fit", "lwr" and "upr"
  result <- cbind(fit, lwr, upr)
  colnames(result) <- c("fit", "lwr", "upr")
  return(result)

}

## ---- pred-case-3--------------------------------------------------------
# Data for which we want a prediction
newdata <- data.frame(temp = -0.6)

# Prediction of the conditional log-odds, the default
predict(nasa, newdata = newdata, type = "link")

# Prediction of the conditional probability
predict(nasa, newdata = newdata, type = "response")

# Simple call
predictCIsLogistic(nasa, newdata = newdata)
# The CI is large because there is no data around temp = -0.6 and
# that makes the prediction more variable (and also because we only
# have 23 observations)

## ---- deviance-1---------------------------------------------------------
# Summary of model
nasa <- glm(fail.field ~ temp, family = "binomial", data = challenger)
summaryLog <- summary(nasa)
summaryLog
# 'Residual deviance' is the deviance; 'Null deviance' is the null deviance

# Null model (only intercept)
null <- glm(fail.field ~ 1, family = "binomial", data = challenger)
summaryNull <- summary(null)
summaryNull

# Computation of the R^2 with a function - useful for repetitive computations
r2glm <- function(model) {

  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance

}

# R^2
r2glm(nasa)
r2glm(null)

## ---- deviance-2---------------------------------------------------------
# Polynomial predictors
nasa0 <- glm(fail.field ~ 1, family = "binomial", data = challenger)
nasa1 <- glm(fail.field ~ temp, family = "binomial", data = challenger)
nasa2 <- glm(fail.field ~ poly(temp, degree = 2), family = "binomial",
             data = challenger)
nasa3 <- glm(fail.field ~ poly(temp, degree = 3), family = "binomial",
             data = challenger)

# Plot fits
temp <- seq(-1, 35, l = 200)
tt <- data.frame(temp = temp)
plot(fail.field ~ temp, data = challenger, pch = 16, xlim = c(-1, 30),
     xlab = "Temperature", ylab = "Incident probability")
lines(temp, predict(nasa0, newdata = tt, type = "response"), col = 1)
lines(temp, predict(nasa1, newdata = tt, type = "response"), col = 2)
lines(temp, predict(nasa2, newdata = tt, type = "response"), col = 3)
lines(temp, predict(nasa3, newdata = tt, type = "response"), col = 4)
legend("bottomleft", legend = c("Null model", "Linear", "Quadratic", "Cubic"),
       lwd = 2, col = 1:4)

# R^2's
r2glm(nasa0)
r2glm(nasa1)
r2glm(nasa2)
r2glm(nasa3)

# Chisq and F tests - same results since phi is known
anova(nasa1, test = "Chisq")
anova(nasa1, test = "F")

# Incremental comparisons of nested models
anova(nasa1, nasa2, nasa3, test = "Chisq")
# Quadratic effects are not significative

# Cubic vs linear
anova(nasa1, nasa3, test = "Chisq")

# Example in Poisson regression
species1 <- glm(Species ~ ., data = species, family = poisson)
species2 <- glm(Species ~ Biomass * pH, data = species, family = poisson)

# Comparison
anova(species1, species2, test = "Chisq")
r2glm(species1)
r2glm(species2)

## ---- glmmodsel-1--------------------------------------------------------
# Models
nasa1 <- glm(fail.field ~ temp, family = "binomial", data = challenger)
nasa2 <- glm(fail.field ~ temp + pres.field, family = "binomial",
             data = challenger)

# Summaries
summary(nasa1)
summary(nasa2)

# AICs
AIC(nasa1) # Better
AIC(nasa2)

## ---- glmmodsel-2--------------------------------------------------------
# Boston dataset
data(Boston, package = "MASS")

# Model whether a suburb has a median house value larger than $25000
mod <- glm(I(medv > 25) ~ ., data = Boston, family = "binomial")
summary(mod)
r2glm(mod)

# With BIC - ends up with only the significant variables and a similar R^2
modBIC <- MASS::stepAIC(mod, trace = 0, k = log(nrow(Boston)))
summary(modBIC)
r2glm(modBIC)

## ---- glmmodsel-3--------------------------------------------------------
# Fitted probabilities for Y = 1
nasa$fitted.values

# Classified Y's
yHat <- nasa$fitted.values > 0.5

# Hit matrix:
# - 16 correctly classified as 0
# - 4 correctly classified as 1
# - 3 incorrectly classified as 0
tab <- table(challenger$fail.field, yHat)
tab

# Hit ratio (ratio of correct classification)
sum(diag(tab)) / sum(tab)

