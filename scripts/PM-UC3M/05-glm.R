
## ------------------------------------------------------------------------
## Name: 05-glm.R
## Description: Script for Chapter 5 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 5.9.7
## ------------------------------------------------------------------------

## ---- challenger-load, eval = FALSE----------------------------------------------------
## # Read data
## challenger <- read.table(file = "challenger.txt", header = TRUE, sep = "\t")

## ---- challengerfigs-------------------------------------------------------------------
# Figures 5.3a and 5.3b
car::scatterplot(nfails.field ~ temp, smooth = FALSE, boxplots = FALSE,
                 data = challenger, subset = nfails.field > 0)
car::scatterplot(nfails.field ~ temp, smooth = FALSE, boxplots = FALSE,
                 data = challenger)

## ---- nasadiag-------------------------------------------------------------------------
# Fit linear model, and run linearity and normality diagnostics
mod <- lm(nfails.field ~ temp, data = challenger)
plot(mod, 1)
plot(mod, 2)

## ---- logcurve-------------------------------------------------------------------------
# Logistic regression: computed with glm and family = "binomial"
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

## ---- logsummary-----------------------------------------------------------------------
# Exponentiated coefficients ("odds ratios")
exp(coef(nasa))

## ---- modform-1------------------------------------------------------------------------
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

## ---- modform-2, fig.cap = '(ref:modform-2-title)'-------------------------------------
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

# Visualization of the log-likelihood surface
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
                 axis(1); axis(2)
                 points(mod$coefficients[1], mod$coefficients[2],
                        col = 2, pch = 16)
                 points(opt$par[1], opt$par[2], col = 4)
               })
# The plot.axes argument is a hack to add graphical information within the
# coordinates of the main panel (behind filled.contour there is a layout()...)

## ---- pois-load, eval = FALSE----------------------------------------------------------
## # Read data
## species <- read.table("species.txt", header = TRUE)
## species$pH <- as.factor(species$pH)

## ---- poiscurve------------------------------------------------------------------------
# Plot data
plot(Species ~ Biomass, data = species, col = as.numeric(pH))
legend("topright", legend = c("High pH", "Medium pH", "Low pH"),
       col = c(1, 3, 2), lwd = 2) # colors according to as.numeric(pH)

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
#   of species decreases by a factor of 0.9686 (0.8564). The higher the pH, the
#   stronger the effect of the Biomass in Species

# Draw fits
plot(Species ~ Biomass, data = species, col = as.numeric(pH))
legend("topright", legend = c("High pH", "Medium pH", "Low pH"),
       col = c(1, 3, 2), lwd = 2) # colors according to as.numeric(pH)

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
lines(bio, exp(species2$coefficients[4] + species2$coefficients[6] * bio + z),
      col = 3, lty = 2)

## ---- bino-load, eval = FALSE----------------------------------------------------------
## # Read data
## heart <- read.table("heart.txt", header = TRUE)
## 
## # Sizes for each observation (Ni's)
## heart$Ni <- heart$ok + heart$ha
## 
## # Proportions of patients with heart attacks
## heart$prop <- heart$ha / (heart$ha + heart$ok)

## ---- binocurve------------------------------------------------------------------------
# Plot of proportions versus ck: twelve observations, each requiring
# Ni patients to determine the proportion
plot(heart$ck, heart$prop, xlab = "Creatinine kinase level",
     ylab = "Proportion of heart attacks")

# Fit binomial regression: recall the cbind() to pass the number of successes
# and failures
heart1 <- glm(cbind(ha, ok) ~ ck, family = binomial, data = heart)
summary(heart1)

# Alternatively: put proportions as responses, but then it is required to
# inform about the binomial size of each observation
heart1 <- glm(prop ~ ck, family = binomial, data = heart, weights = Ni)
summary(heart1)

# Add fitted line
ck <- 0:500
newdata <- data.frame(ck = ck)
logistic <- function(eta) 1 / (1 + exp(-eta))
lines(ck, logistic(cbind(1, ck) %*% heart1$coefficients))

# It seems that a polynomial fit could better capture the "wiggly" pattern
# of the data
heart2 <- glm(prop ~ poly(ck, 2, raw = TRUE), family = binomial, data = heart,
              weights = Ni)
heart3 <- glm(prop ~ poly(ck, 3, raw = TRUE), family = binomial, data = heart,
              weights = Ni)
heart4 <- glm(prop ~ poly(ck, 4, raw = TRUE), family = binomial, data = heart,
              weights = Ni)

# Best fit given by heart3
BIC(heart1, heart2, heart3, heart4)
summary(heart3)

# All fits together
lines(ck, logistic(cbind(1, poly(ck, 2, raw = TRUE)) %*% heart2$coefficients),
      col = 2)
lines(ck, logistic(cbind(1, poly(ck, 3, raw = TRUE)) %*% heart3$coefficients),
      col = 3)
lines(ck, logistic(cbind(1, poly(ck, 4, raw = TRUE)) %*% heart4$coefficients),
      col = 4)
legend("bottomright", legend = c("Linear", "Quadratic", "Cubic", "Quartic"),
       col = 1:4, lwd = 2)

## ---- nasa-case-1, message = FALSE-----------------------------------------------------
# Summary of the model
summary(nasa)

# Confidence intervals at 95%
confint.default(nasa)

# Confidence intervals at other levels
confint.default(nasa, level = 0.90)

# Confidence intervals for the factors affecting the odds
exp(confint.default(nasa))

## ---- testcoef-1-----------------------------------------------------------------------
# Significances with asymptotic approximation for the standard errors
summary(nasa)

# CIs with asymptotic approximation -- coherent with summary
confint.default(nasa, level = 0.95)
confint.default(nasa, level = 0.99)

# CIs with profile likelihood -- incoherent with summary
confint(nasa, level = 0.95) # intercept still significant
confint(nasa, level = 0.99) # temp still significant

## ---- pred-case-1----------------------------------------------------------------------
predict(nasa, newdata = data.frame(temp = -0.6), type = "response")

## ---- pred-case-2----------------------------------------------------------------------
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

## ---- pred-case-3----------------------------------------------------------------------
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

## ---- pred-case-4----------------------------------------------------------------------
# Estimated probability for launching at 53 degrees Fahrenheit
predictCIsLogistic(nasa, newdata = data.frame(temp = 11.67))

## ---- deviance-1-----------------------------------------------------------------------
# Summary of model
nasa <- glm(fail.field ~ temp, family = "binomial", data = challenger)
summaryLog <- summary(nasa)
summaryLog
# 'Residual deviance' is the deviance; 'Null deviance' is the null deviance

# Null model (only intercept)
null <- glm(fail.field ~ 1, family = "binomial", data = challenger)
summaryNull <- summary(null)
summaryNull

# Compute the R^2 with a function -- useful for repetitive computations
r2glm <- function(model) {

  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance

}

# R^2
r2glm(nasa)
r2glm(null)

## ---- deviance-2-----------------------------------------------------------------------
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

# Chisq and F tests -- same results since phi is known
anova(nasa1, test = "Chisq")
anova(nasa1, test = "F")

# Incremental comparisons of nested models
anova(nasa1, nasa2, nasa3, test = "Chisq")
# Quadratic effects are not significative

# Cubic vs. linear
anova(nasa1, nasa3, test = "Chisq")

# Example in Poisson regression
species1 <- glm(Species ~ ., data = species, family = poisson)
species2 <- glm(Species ~ Biomass * pH, data = species, family = poisson)

# Comparison
anova(species1, species2, test = "Chisq")
r2glm(species1)
r2glm(species2)

## ---- glmmodsel-1----------------------------------------------------------------------
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

## ---- glmmodsel-2----------------------------------------------------------------------
# Boston dataset
data(Boston, package = "MASS")

# Model whether a suburb has a median house value larger than $25000
mod <- glm(I(medv > 25) ~ ., data = Boston, family = "binomial")
summary(mod)
r2glm(mod)

# With BIC -- ends up with only the significant variables and a similar R^2
modBIC <- MASS::stepAIC(mod, trace = 0, k = log(nrow(Boston)))
summary(modBIC)
r2glm(modBIC)

## ---- glmmodsel-3----------------------------------------------------------------------
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

## ---- moddiag-1------------------------------------------------------------------------
# Create predictors with multicollinearity: x4 depends on the rest
set.seed(45678)
x1 <- rnorm(100)
x2 <- 0.5 * x1 + rnorm(100)
x3 <- 0.5 * x2 + rnorm(100)
x4 <- -x1 + x2 + rnorm(100, sd = 0.25)

# Response
z <- 1 + 0.5 * x1 + 2 * x2 - 3 * x3 - x4
y <- rbinom(n = 100, size = 1, prob = 1/(1 + exp(-z)))
data <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, y = y)

# Correlations -- none seems suspicious
cor(data)

# Abnormal generalized variance inflation factors: largest for x4, we remove it
modMultiCo <- glm(y ~ x1 + x2 + x3 + x4, family = "binomial")
car::vif(modMultiCo)

# Without x4
modClean <- glm(y ~ x1 + x2 + x3, family = "binomial")

# Comparison
summary(modMultiCo)
summary(modClean)

# Generalized variance inflation factors normal
car::vif(modClean)

## ---- glmshrinkage---------------------------------------------------------------------
# Load data
data(Hitters, package = "ISLR")

# Include only predictors related with 1986 season and discard NA's
Hitters <- subset(Hitters, select = c(League, AtBat, Hits, HmRun, Runs, RBI,
                                      Walks, Division, PutOuts, Assists,
                                      Errors))
Hitters <- na.omit(Hitters)

# Response and predictors
y <- Hitters$League
x <- model.matrix(League ~ ., data = Hitters)[, -1]

## ---- glmshr-1-------------------------------------------------------------------------
# Ridge and lasso regressions
library(glmnet)
ridgeMod <- glmnet(x = x, y = y, alpha = 0, family = "binomial")
lassoMod <- glmnet(x = x, y = y, alpha = 1, family = "binomial")

# Solution paths versus lambda
plot(ridgeMod, label = TRUE, xvar = "lambda")
plot(lassoMod, label = TRUE, xvar = "lambda")

# Versus the percentage of deviance explained
plot(ridgeMod, label = TRUE, xvar = "dev")
plot(lassoMod, label = TRUE, xvar = "dev")
# The percentage of deviance explained only goes up to 0.05. There are no
# clear patterns indicating player differences between both leagues

# Let's select the predictors to be included with a 10-fold cross-validation
set.seed(12345)
kcvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10, family = "binomial")
plot(kcvLasso)

# The lambda that minimizes the CV error and "one standard error rule"'s lambda
kcvLasso$lambda.min
kcvLasso$lambda.1se

# Leave-one-out cross-validation -- similar result
ncvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = nrow(Hitters),
                      family = "binomial")
plot(ncvLasso)
ncvLasso$lambda.min
ncvLasso$lambda.1se

# Model selected
predict(ncvLasso, type = "coefficients", s = ncvLasso$lambda.1se)

## ---- glmshr-2, fig.asp = 1/2----------------------------------------------------------
# Analyze the selected model)
fit <- glm(League ~ HmRun, data = Hitters, family = "binomial")
summary(fit)
# HmRun is significant -- but it may be spurious due to the model selection
# procedure (see Appendix A.5)

# Let's split the dataset in two, do model-selection in one part and then
# inference on the selected model in the other, to have an idea of the real
# significance of HmRun
set.seed(12345678)
train <- sample(c(FALSE, TRUE), size = nrow(Hitters), replace = TRUE)

# Model selection in training part
ncvLasso <- cv.glmnet(x = x[train, ], y = y[train], alpha = 1,
                      nfolds = sum(train), family = "binomial")
predict(ncvLasso, type = "coefficients", s = ncvLasso$lambda.1se)

# Inference in testing part
summary(glm(League ~ HmRun, data = Hitters[!train, ], family = "binomial"))
# HmRun is now not significant...
# We can repeat the analysis for different partitions of the data and we will
# obtain weak significances. Therefore, we can conclude that this is an spurious
# finding and that HmRun is not significant as a single predictor

# Prediction (obviously not trustable, but for illustration)
pred <- predict(ncvLasso, newx = x[!train, ], type = "response",
                s = ncvLasso$lambda.1se)

# Hit matrix and hit ratio
H <- table(pred > 0.5, y[!train] == "A") # ("A" was the reference level)
H
sum(diag(H)) / sum(H) # Almost like tossing a coin!

## ---- bigglm-0, eval = FALSE-----------------------------------------------------------
## # To install specific versions of packages
## install.packages("versions")
## library(versions)
## 
## # Install specific package versions. It may take a while to do so, be patient
## install.versions(pkgs = c("bit", "ff", "ffbase"),
##                  versions = c("1.1-15.2", "2.2-14.2", "0.12.8"))
## # After bit's version 1.1-15.2, something is off in the integration with
## # ffbase; see issue in https://github.com/edwindj/ffbase/issues/61
## 
## # Alternatively, if the binaries for your OS are not available (e.g., for
## # Apple M1's processors), then you will need to compile the packages from
## # source... and cross your fingers!
## urls <- c(
##   "https://cran.r-project.org/src/contrib/Archive/bit/bit_1.1-15.2.tar.gz",
##   "https://cran.r-project.org/src/contrib/Archive/ff/ff_2.2-14.2.tar.gz",
##   "https://cran.r-project.org/src/contrib/Archive/ffbase/ffbase_0.12.8.tar.gz"
##   )
## install.packages(pkgs = urls, repos = NULL, type = "source")

## ---- bigglm-1, eval = FALSE-----------------------------------------------------------
## # Not really "big data", but for the sake of illustration
## set.seed(12345)
## n <- 1e6
## p <- 10
## beta <- seq(-1, 1, length.out = p)^5
## x1 <- matrix(rnorm(n * p), nrow = n, ncol = p)
## x1[, p] <- 2 * x1[, 1] + rnorm(n, sd = 0.1) # Add some dependence to predictors
## x1[, p - 1] <- 2 - x1[, 2] + rnorm(n, sd = 0.5)
## y1 <- rbinom(n, size = 1, prob = 1 / (1 + exp(-(1 + x1 %*% beta))))
## x2 <- matrix(rnorm(100 * p), nrow = 100, ncol = p)
## y2 <- rbinom(100, size = 1, prob = 1 / (1 + exp(-(1 + x2 %*% beta))))
## bigData1 <- data.frame("resp" = y1, "pred" = x1)
## bigData2 <- data.frame("resp" = y2, "pred" = x2)
## 
## # Save files to disk to emulate the situation with big data
## write.csv(x = bigData1, file = "bigData1.csv", row.names = FALSE)
## write.csv(x = bigData2, file = "bigData2.csv", row.names = FALSE)
## 
## # Read files using ff
## library(ffbase) # Imports ff
## bigData1ff <- read.table.ffdf(file = "bigData1.csv", header = TRUE, sep = ",")
## bigData2ff <- read.table.ffdf(file = "bigData2.csv", header = TRUE, sep = ",")
## 
## # Recall: bigData1.csv is not copied into RAM
## print(object.size(bigData1), units = "MB")
## print(object.size(bigData1ff), units = "KB")
## 
## # Logistic regression
## # Same comments for the formula framework -- this is the hack for automatic
## # inclusion of all the predictors
## library(biglm)
## f <- formula(paste("resp ~", paste(names(bigData1)[-1], collapse = " + ")))
## bigglmMod <- bigglm.ffdf(formula = f, data = bigData1ff, family = binomial())
## 
## # glm's call
## glmMod <- glm(formula = resp ~ ., data = bigData1, family = binomial())
## 
## # Compare sizes
## print(object.size(bigglmMod), units = "KB")
## print(object.size(glmMod), units = "MB")
## 
## # Summaries
## s1 <- summary(bigglmMod)
## s2 <- summary(glmMod)
## s1
## s2
## 
## # Further information
## s1$mat # Coefficients and their inferences
## s1$rsq # R^2
## s1$nullrss # Null deviance
## 
## # Extract coefficients
## coef(bigglmMod)
## 
## # Prediction works as usual
## predict(bigglmMod, newdata = bigData2[1:5, ], type = "response")
## # predict(bigglmMod, newdata = bigData2[1:5, -1]) # Error
## 
## # Update the model with training data
## update(bigglmMod, moredata = bigData2)
## 
## # AIC and BIC
## AIC(bigglmMod, k = 2)
## AIC(bigglmMod, k = log(n))
## 
## # Delete the files in disk
## file.remove(c("bigData1.csv", "bigData2.csv"))

## Note that this is also a perfectly **valid approach for linear models**, we just need to specify `family = gaussian()` in the call to `bigglm.ffdf`.

## ---- bigglm-2, eval = FALSE-----------------------------------------------------------
## # Model selection adapted to big data generalized linear models
## reg <- leaps::regsubsets(bigglmMod, nvmax = p + 1, method = "exhaustive")
## # This takes the QR decomposition, which encodes the linear model associated to
## # the last iteration of the IRLS algorithm. However, the reported BICs are *not*
## # the true BICs of the generalized linear models, but a sufficient
## # approximation to obtain a list of candidate models in a fast way
## 
## # Get the model with lowest BIC
## plot(reg)
## subs <- summary(reg)
## subs$which
## subs$bic
## subs$which[which.min(subs$bic), ]
## 
## # Let's compute the true BICs for the p models. This implies fitting p bigglm's
## bestModels <- list()
## for (i in 1:nrow(subs$which)) {
##   f <- formula(paste("resp ~", paste(names(which(subs$which[i, -1])),
##                                      collapse = " + ")))
##   bestModels[[i]] <- bigglm.ffdf(formula = f, data = bigData1ff,
##                                  family = binomial(), maxit = 20)
##   # Did not converge with the default iteration limit, maxit = 8
## 
## }
## 
## # The approximate BICs and the true BICs are very similar (in this example)
## exactBICs <- sapply(bestModels, AIC, k = log(n))
## plot(subs$bic, exactBICs, type = "o", xlab = "Exact", ylab = "Approximate")
## 
## # Pearson correlation
## cor(subs$bic, exactBICs, method = "pearson")
## 
## # Order correlation
## cor(subs$bic, exactBICs, method = "spearman")
## 
## # Both give the same model selection and same order
## subs$which[which.min(subs$bic), ] # Approximate
## subs$which[which.min(exactBICs), ] # Exact

