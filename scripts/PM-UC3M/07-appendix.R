
## ------------------------------------------------------------------------
## Name: 07-appendix.R
## Description: Script for Chapter 7 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/PM-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## Version: 5.9.5
## ------------------------------------------------------------------------

## ---- ht, fig.margin = FALSE, fig.fullwidth = TRUE, fig.asp = 1/2, fig.cap = '(ref:ht-title)'----
# Sample data from a N(0, 1)
set.seed(3245678)
n <- 50
x <- rnorm(n)

# Kolmogorov-Smirnov test for H_0 : F = N(0, 1). Does not reject.
ks.test(x, "pnorm")

# Simulation of p-values when H_0 is true
M <- 1e4
pValues_H0 <- sapply(1:M, function(i) {
  x <- rnorm(n) # N(0, 1)
  ks.test(x, "pnorm")$p.value
})

# Simulation of p-values when H_0 is false -- the data does not
# come from a N(0, 1) but from a N(0, 1.5)
pValues_H1 <- sapply(1:M, function(i) {
  x <- rnorm(n, mean = 0, sd = sqrt(1.5)) # N(0, 1.5)
  ks.test(x, "pnorm")$p.value
})

# Comparison of p-values
par(mfrow = 1:2)
hist(pValues_H0, breaks = seq(0, 1, l = 20), probability = TRUE,
     main = expression(H[0]), ylim = c(0, 2.5))
abline(h = 1, col = 2)
hist(pValues_H1, breaks = seq(0, 1, l = 20), probability = TRUE,
     main = expression(H[1]), ylim = c(0, 2.5))
abline(h = 1, col = 2)

## ---- multn-1----------------------------------------------------------------------------
# Data from the voting intentions in the 1988 Chilean national plebiscite
data(Chile, package = "carData")
summary(Chile)
# vote is a factor with levels A (abstention), N (against Pinochet),
# U (undecided), Y (for Pinochet)

# Fit of the model done by multinom: Response ~ Predictors
# It is an iterative procedure (maxit sets the maximum number of iterations)
# Read the documentation in ?multinom for more information
mod1 <- nnet::multinom(vote ~ age + education + statusquo, data = Chile,
                       maxit = 1e3)

# Each row of coefficients gives the coefficients of the logistic
# regression of a level versus the reference level (A)
summary(mod1)

# Set a different level as the reference (N) for easier interpretations
Chile$vote <- relevel(Chile$vote, ref = "N")
mod2 <- nnet::multinom(vote ~ age + education + statusquo, data = Chile,
                       maxit = 1e3)
summary(mod2)
exp(coef(mod2))
# Some highlights:
# - intercepts do not have too much interpretation (correspond to age = 0).
#   A possible solution is to center age by its mean (so age = 0 would
#   represent the mean of the ages)
# - both age and statusquo increase the probability of voting Y, A or U
#   with respect to voting N -> conservatism increases with ages
# - both age and statusquo increase more the probability of voting Y and U
#   than A -> elderly and status quo supporters are more decided to participate
# - a PS level of education increases the probability of voting N. Same for
#   a S level of education, but more prone to A

# Prediction of votes -- three profile of voters
newdata <- data.frame(age = c(23, 40, 50),
                      education = c("PS", "S", "P"),
                      statusquo = c(-1, 0, 2))

# Probabilities of belonging to each class
predict(mod2, newdata = newdata, type = "probs")

# Predicted class
predict(mod2, newdata = newdata, type = "class")

## ---- nas-1, error = TRUE----------------------------------------------------------------
# The airquality dataset contains NA's
data(airquality)
head(airquality)
summary(airquality)

# Let's add more NA's for the sake of illustration
set.seed(123456)
airquality$Solar.R[runif(nrow(airquality)) < 0.7] <- NA
airquality$Day[runif(nrow(airquality)) < 0.1] <- NA

# See what are the fully-observed cases
comp <- complete.cases(airquality)
mean(comp) # Only 15% of cases are fully observed

# Complete cases
head(airquality[comp, ])

# Linear model on all the variables
summary(lm(Ozone ~ ., data = airquality)) # 129 not included

# Caution! Even if the problematic variable is excluded, only
# the complete observations are employed
summary(lm(Ozone ~ . - Solar.R, data = airquality))

# Notice the difference with
summary(lm(Ozone ~ ., data = subset(airquality, select = -Solar.R)))

# Model selection can be problematic with missing data, since
# the number of complete cases changes with the addition or
# removing of predictors
mod <- lm(Ozone ~ ., data = airquality)

# stepAIC drops an error
modAIC <- MASS::stepAIC(mod)

# Also, this will be problematic (the number of complete
# cases changes with the predictors considered!)
modBIC <- MASS::stepAIC(mod, k = log(nrow(airquality)))

# Comparison of AICs or BICs is spurious: the scale of the
# likelihood changes with the sample size (the likelihood
# decreases with n), which increases AIC / BIC with n.
# Hence using BIC / AIC is not adequate for model selection
# with missing data.
AIC(lm(Ozone ~ ., data = airquality))
AIC(lm(Ozone ~ ., data = subset(airquality, select = -Solar.R)))

# Considers only complete cases including Solar.R
AIC(lm(Ozone ~ . - Solar.R, data = airquality))

## ---- nas-2, fig.asp = 1/2---------------------------------------------------------------
# The complete cases approach is the default in R
summary(lm(Ozone ~ ., data = airquality))

# However, since the complete cases that R is going to consider
# depends on which predictors are included, it is safer to
# exclude NA's explicitly before the fitting of a model.
airqualityNoNA <- na.exclude(airquality)
summary(airqualityNoNA)

# The package VIM has a function to visualize where the missing
# data is present. It gives the percentage of NA's for each
# variable and for the most important combinations of NA's.
VIM::aggr(airquality)
VIM::aggr(airqualityNoNA)

# Stepwise regression without NA's -- no problem
modBIC1 <- MASS::stepAIC(lm(Ozone ~ ., data = airqualityNoNA),
                         k = log(nrow(airqualityNoNA)), trace = 0)
summary(modBIC1)

# But we only take into account 16% of the original data
nrow(airqualityNoNA) / nrow(airquality)

# Removing the predictor with many NA's, as we did before
# We also exclude NA's from other predictors
airqualityNoSolar.R <- na.exclude(subset(airquality, select = -Solar.R))
modBIC2 <- MASS::stepAIC(lm(Ozone ~ ., data = airqualityNoSolar.R),
                         k = log(nrow(airqualityNoSolar.R)), trace = 0)
summary(modBIC2)
# In this example the approach works well because most of
# the NA's are associated to the variable Solar.R

# Input data using the sample mean
library(mice)
airqualityMean <- complete(mice(data = airquality, m = 1, method = "mean"))
head(airqualityMean)
# Explanation of the syntax:
# - mice::complete() serves to retrieve the completed dataset from
#   the mids object.
# - m = 1 specifies that we only want a reconstruction of the
#   dataset because the imputation method is deterministic
#   (it could be random).
# - method = "mean" says that we want the sample mean to be
#   used to fill NA's in all the columns. This only works
#   properly for quantitative variables.

# Impute using linear regression for the response (first column)
# and mean for the predictors (remaining five columns)
airqualityLm <- complete(mice(data = airquality, m = 1, 
                              method = c("norm.predict", rep("mean", 5))))
head(airqualityLm)

# Imputed data -- some extrapolation problems may happen
airqualityLm$Ozone[is.na(airquality$Ozone)]

# Notice that the imputed data is the same (except for a small
# truncation that is introduced by mice) as
predict(lm(airquality$Ozone ~ ., data = airqualityMean),
        newdata = airqualityMean[is.na(airquality$Ozone), -1])

# Removing the truncation with ridge = 0
complete(mice(data = airquality, m = 1, 
              method = c("norm.predict", rep("mean", 5)), 
              ridge = 0))[is.na(airquality$Ozone), 1]

# The default mice's method (predictive mean matching) works
# better in this case (in the sense that it does not yield
# negative Ozone values)
# Notice that there is randomness in the imputation!
airqualityMice <- complete(mice(data = airquality, m = 1, seed = 123))
head(airqualityMice)

## ---- notevarsel-------------------------------------------------------------------------
# Simulation setting
n <- 2e2
p <- 4
p0 <- p %/% 2
beta <- c(rep(1, p0), rep(0, p - p0))

# Generate two sets of independent data following the same linear model
# with coefficients beta and null intercept
x1 <- matrix(rnorm(n * p), nrow = n, ncol = p)
data1 <- data.frame("x" = x1)
xbeta1 <- x1 %*% beta
x2 <- matrix(rnorm(n * p), nrow = n, ncol = p)
data2 <- data.frame("x" = x2)
xbeta2 <- x2 %*% beta

# Objects for the simulation
M <- 1e4
pvalues1 <- pvalues2 <- pvalues3 <- matrix(NA, nrow = M, ncol = p)
set.seed(12345678)
data1$y <- xbeta1 + rnorm(n)
nam <- names(lm(y ~ 0 + ., data = data1)$coefficients)

# Simulation
# pb <- txtProgressBar(style = 3)
for (i in 1:M) {

  # Generate new data
  data1$y <- xbeta1 + rnorm(n)

  # Obtain the significances of the coefficients for the usual linear model
  mod1 <- lm(y ~ 0 + ., data = data1)
  s1 <- summary(mod1)
  pvalues1[i, ] <- s1$coefficients[, 4]

  # Obtain the significances of the coefficients for a data-driven selected
  # linear model (in this case, by stepwise regression using BIC)
  mod2 <- MASS::stepAIC(mod1, k = log(n), trace = 0)
  s2 <- summary(mod2)
  ind <- match(x = names(s2$coefficients[, 4]), table = nam)
  pvalues2[i, ind] <- s2$coefficients[, 4]

  # Generate independent data
  data2$y <- xbeta2 + rnorm(n)

  # Significances of the coefficients by the data-driven selected model
  s3 <- summary(lm(y ~ 0 + ., data = data2[, c(ind, p + 1)]))
  pvalues3[i, ind] <- s3$coefficients[, 4]

  # Progress
  # setTxtProgressBar(pb = pb, value = i / M)

}

# Percentage of NA's: NA = predictor excluded
apply(pvalues2, 2, function(x) mean(is.na(x)))

# Boxplots of significances
boxplot(pvalues1, names = expression(beta[1], beta[3], beta[3], beta[4]),
        main = "p-values in the full model", ylim = c(0, 1))
boxplot(pvalues2, names = expression(beta[1], beta[3], beta[3], beta[4]),
        main = "p-values in the stepwise model", ylim = c(0, 1))
boxplot(pvalues3, names = expression(beta[1], beta[3], beta[3], beta[4]),
        main = "p-values in the model with the predictors selected by
        stepwise regression, and fitted in an independent sample",
        ylim = c(0, 1))

# Test uniformity of the p-values associated to the coefficients that are 0
apply(pvalues1[, (p0 + 1):p], 2, function(x) ks.test(x, y = "punif")$p.value)
apply(pvalues2[, (p0 + 1):p], 2, function(x) ks.test(x, y = "punif")$p.value)
apply(pvalues3[, (p0 + 1):p], 2, function(x) ks.test(x, y = "punif")$p.value)

## ---- r-1, echo = FALSE, cache = FALSE---------------------------------------------------
rm(list = ls())

## ---- r-2, error = TRUE, cache = FALSE---------------------------------------------------
# The console can act as a simple calculator
1.0 + 1.1
2 * 2
3/2
2^3
1/0
0/0

# Use ";" for performing several operations in the same line
(1 + 3) * 2 - 1; 3 + 2

# Elemental mathematical functions
sqrt(2); 2^0.5
exp(1)
log(10) # Natural logarithm
log10(10); log2(10) # Logs in base 10 and 2
sin(pi); cos(0); asin(0)
tan(pi/3)
sqrt(-1)

# Remember to close the parenthesis -- errors below
1 +
(1 + 3

## ---- r-3, error = TRUE------------------------------------------------------------------
# Any operation that you perform in R can be stored in a variable
# (or object) with the assignment operator "<-"
x <- 1

# To see the value of a variable, simply type it
x

# A variable can be overwritten
x <- 1 + 1

# Now the value of x is 2 and not 1, as before
x

# Capitalization matters
X <- 3
x; X

# See what are the variables in the workspace
ls()

# Remove variables
rm(X)
X

## ---- r-4, error = TRUE------------------------------------------------------------------
# We combine numbers with the function "c"
c(1, 3)
c(1.5, 0, 5, -3.4)

# A handy way of creating integer sequences is the operator ":"
1:5

# Storing some vectors
myData <- c(1, 2)
myData2 <- c(-4.12, 0, 1.1, 1, 3, 4)
myData
myData2

# Entrywise operations
myData + 1
myData^2

# If you want to access a position of a vector, use [position]
myData[1]
myData2[6]

# You can also change elements
myData[1] <- 0
myData

# Think on what you want to access...
myData2[7]
myData2[0]

# If you want to access all the elements except a position,
# use [-position]
myData2[-1]
myData2[-2]

# Also with vectors as indexes
myData2[1:2]
myData2[myData]

# And also
myData2[-c(1, 2)]

# But do not mix positive and negative indexes!
myData2[c(-1, 2)]

# Remove the first element
myData2 <- myData2[-1]

## ---- r-5--------------------------------------------------------------------------------
# Functions take arguments between parenthesis and transform them
# into an output
sum(myData)
prod(myData)

# Summary of an object
summary(myData)

# Length of the vector
length(myData)

# Mean, standard deviation, variance, covariance, correlation
mean(myData)
var(myData)
cov(myData, myData^2)
cor(myData, myData * 2)
quantile(myData)

# Maximum and minimum of vectors
min(myData)
which.min(myData)

# Usually the functions have several arguments, which are set
# by "argument = value" In the next case, the second argument is
# a logical flag to indicate the kind of sorting
sort(myData) # If nothing is specified, decreasing = FALSE is
# assumed
sort(myData, decreasing = TRUE)

# Do not know what are the arguments of a function? Use args
# and help!
args(mean)
?mean

## ---- r-6, error = TRUE------------------------------------------------------------------
# A matrix is an array of vectors
A <- matrix(1:4, nrow = 2, ncol = 2)
A

# Another matrix
B <- matrix(1, nrow = 2, ncol = 2, byrow = TRUE)
B

# Matrix is a vector with dimension attributes
dim(A)

# Binding by rows or columns
rbind(1:3, 4:6)
cbind(1:3, 4:6)

# Entrywise operations
A + 1
A * B

# Accessing elements
A[2, 1] # Element (2, 1)
A[1, ] # First row -- this is a vector
A[, 2] # First column -- this is a vector

# Obtain rows and columns as matrices (and not as vectors)
A[1, , drop = FALSE]
A[, 2, drop = FALSE]

# Matrix transpose
t(A)

# Matrix multiplication
A %*% B
A %*% B[, 1]
A %*% B[1, ]

# Care is needed
A %*% B[1, , drop = FALSE] # Incompatible product

# Compute inverses with "solve"
solve(A) %*% A

# A data frame is a matrix with column names
# Useful when you have multiple variables
myDf <- data.frame(var1 = 1:2, var2 = 3:4)
myDf

# You can change names
names(myDf) <- c("newname1", "newname2")
myDf

# You can access variables by its name with the "$" operator
myDf$newname1

# And create new variables also (they have to be of the same
# length as the rest of variables)
myDf$myNewVariable <- c(0, 1)
myDf

# A list is a collection of arbitrary variables
myList <- list(myData = myData, A = A, myDf = myDf)

# Access elements by names
myList$myData
myList$A
myList$myDf

# Reveal the structure of an object
str(myList)
str(myDf)

# A less lengthy output
names(myList)

## ---- r-7--------------------------------------------------------------------------------
# The iris dataset is already imported in R
# (beware: locfit has also an iris dataset, with different names
# and shorter)

# The beginning of the data
head(iris)

# "names" gives you the variables in the data frame
names(iris)

# So we can access variables by "$" or as in a matrix
iris$Sepal.Length[1:10]
iris[1:10, 1]
iris[3, 1]

# Information on the dimension of the data frame
dim(iris)

# "str" gives the structure of any object in R
str(iris)

# Recall the species variable: it is a categorical variable
# (or factor), not a numeric variable
iris$Species[1:10]

# Factors can only take certain values
levels(iris$Species)

# If a file contains a variable with character strings as
# observations (either encapsulated by quotation marks or not),
# the variable will become a factor when imported into R

## ---- r-8--------------------------------------------------------------------------------
# The function "seq" creates sequences of numbers equally separated
seq(0, 1, by = 0.1)
seq(0, 1, length.out = 5)

# You can short the latter argument
seq(0, 1, l = 5)

# Repeat number
rep(0, 5)

# Reverse a vector
myVec <- c(1:5, -1:3)
rev(myVec)

# Another way
myVec[length(myVec):1]

# Count repetitions in your data
table(iris$Species)

## ---- r-9--------------------------------------------------------------------------------
# Relational operators: x < y, x > y, x <= y, x >= y, x == y, x!= y
# They return TRUE or FALSE

# Smaller than
0 < 1

# Greater than
1 > 1

# Greater or equal to
1 >= 1 # Remember: ">="" and not "=>"" !

# Smaller or equal to
2 <= 1 # Remember: "<="" and not "=<"" !

# Equal
1 == 1 # Tests equality. Remember: "=="" and not "="" !

# Unequal
1 != 0 # Tests inequality

# TRUE is encoded as 1 and FALSE as 0
TRUE + 1
FALSE + 1

# In a vector-like fashion
x <- 1:5
y <- c(0, 3, 1, 5, 2)
x < y
x == y
x != y

# Subsetting of vectors
x
x[x >= 2]
x[x < 3]

# Easy way of work with parts of the data
data <- data.frame(x = c(0, 1, 3, 3, 0), y = 1:5)
data

# Data such that x is zero
data0 <- data[data$x == 0, ]
data0

# Data such that x is larger than 2
data2 <- data[data$x > 2, ]
data2

# Problem -- what happened?
data[x > 2, ]

# AND operator "&"
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

# OR operator "|"
TRUE | TRUE
TRUE | FALSE
FALSE | FALSE

# Both operators are useful for checking for ranges of data
y
index1 <- (y <= 3) & (y > 0)
y[index1]
index2 <- (y < 2) | (y > 4)
y[index2]

## ---- r-10, out.width = '70%'------------------------------------------------------------
# "plot" is the main function for plotting in R
# It has a different behavior depending on the kind of object
# that it receives

# How to plot some data
plot(iris$Sepal.Length, iris$Sepal.Width,
     main = "Sepal.Length vs. Sepal.Width")

# Change the axis limits
plot(iris$Sepal.Length, iris$Sepal.Width, xlim = c(0, 10),
     ylim = c(0, 10))

# How to plot a curve (a parabola)
x <- seq(-1, 1, l = 50)
y <- x^2
plot(x, y, main = "A red thick parabola", type = "l",
     col = "red", lwd = 3)

# Plotting a more complicated curve between -pi and pi
x <- seq(-pi, pi, l = 50)
y <- (2 + sin(10 * x)) * x^2
plot(x, y, type = "l") # Kind of rough...

# Remember that we are joining points for creating a curve!
# More detailed plot
x <- seq(-pi, pi, l = 500)
y <- (2 + sin(10 * x)) * x^2
plot(x, y, type = "l")

# For more options in the plot customization see
?plot
?par

# "plot" is a first level plotting function. That means that
# whenever is called, it creates a new plot. If we want to
# add information to an existing plot, we have to use a second
# level plotting function such as "points", "lines" or "abline"

plot(x, y) # Create a plot
lines(x, x^2, col = "red") # Add lines
points(x, y + 10, col = "blue") # Add points
abline(a = 5, b = 1, col = "orange", lwd = 2) # Add a straight
# line y = a + b * x

## ---- r-11, out.width = '70%'------------------------------------------------------------
# R allows to sample [r], compute density/probability mass
# functions [d], compute distribution function [p], and compute
# quantiles [q] for several continuous and discrete distributions.
# The format employed is [rdpq]name, where name stands for:
# - norm -> Normal
# - unif -> Uniform
# - exp -> Exponential
# - t -> Student's t
# - f -> Snedecor's F
# - chisq -> Chi squared
# - pois -> Poisson
# - binom -> Binomial
# More distributions:
?Distributions

# Sampling from a normal -- 5 random points from a N(0, 1)
rnorm(n = 5, mean = 0, sd = 1)

# If you want to have always the same result, set the seed of
# the random number generator
set.seed(45678)
rnorm(n = 5, mean = 0, sd = 1)

# Plotting the density of a N(0, 1) -- the Gaussian bell
x <- seq(-4, 4, l = 100)
y <- dnorm(x = x, mean = 0, sd = 1)
plot(x, y, type = "l")

# Plotting the distribution function of a N(0, 1)
x <- seq(-4, 4, l = 100)
y <- pnorm(q = x, mean = 0, sd = 1)
plot(x, y, type = "l")

# Computing the 95% quantile for a N(0, 1)
qnorm(p = 0.95, mean = 0, sd = 1)

# All distributions have the same syntax: rname(n,...),
# dname(x,...), dname(p,...) and qname(p,...), but the
# parameters in ... change. Look them in ?Distributions
# For example, here is the same for the uniform distribution

# Sampling from a U(0, 1)
set.seed(45678)
runif(n = 10, min = 0, max = 1)

# Plotting the density of a U(0, 1)
x <- seq(-2, 2, l = 100)
y <- dunif(x = x, min = 0, max = 1)
plot(x, y, type = "l")

# Computing the 95% quantile for a U(0, 1)
qunif(p = 0.95, min = 0, max = 1)

# Sampling from a Bi(10, 0.5)
set.seed(45678)
samp <- rbinom(n = 200, size = 10, prob = 0.5)
table(samp) / 200

# Plotting the probability mass of a Bi(10, 0.5)
x <- 0:10
y <- dbinom(x = x, size = 10, prob = 0.5)
plot(x, y, type = "h") # Vertical bars

# Plotting the distribution function of a Bi(10, 0.5)
x <- 0:10
y <- pbinom(q = x, size = 10, prob = 0.5)
plot(x, y, type = "h")

## ---- r-12, out.width = '70%', error = TRUE----------------------------------------------
# A function is a way of encapsulating a block of code so it can
# be reused easily. They are useful for simplifying repetitive
# tasks and organize analyses

# This is a silly function that takes x and y and returns its sum
# Note the use of "return" to indicate what should be returned
add <- function(x, y) {
  z <- x + y
  return(z)
}

# Calling add -- you need to run the definition of the function
# first!
add(x = 1, y = 2)
add(1, 1) # Arguments names can be omitted

# A more complex function: computes a linear model and its
# posterior summary. Saves us a few keystrokes when computing a
# lm and a summary
lmSummary <- function(formula, data) {
  model <- lm(formula = formula, data = data)
  summary(model)
}
# If no return(), the function returns the value of the last
# expression

# Usage
lmSummary(Sepal.Length ~ Petal.Width, iris)

# Recall: there is no variable called model in the workspace.
# The function works on its own workspace!
model

# Add a line to a plot
addLine <- function(x, beta0, beta1) {
  lines(x, beta0 + beta1 * x, lwd = 2, col = 2)
}

# Usage
plot(x, y)
addLine(x, beta0 = 0.1, beta1 = 0)

# The function "sapply" allows to sequentially apply a function
sapply(1:10, sqrt)
sqrt(1:10) # The same

# The advantage of "sapply" is that you can use with any function
myFun <- function(x) c(x, x^2)
sapply(1:10, myFun) # Returns a 2 x 10 matrix

# "sapply" is useful for plotting non-vectorized functions
sumSeries <- function(n) sum(1:n)
plot(1:10, sapply(1:10, sumSeries), type = "l")

# "apply" applies iteratively a function to rows (1) or columns
# (2) of a matrix or data frame
A <- matrix(1:10, nrow = 5, ncol = 2)
A
apply(A, 1, sum) # Applies the function by rows
apply(A, 2, sum) # By columns

# With other functions
apply(A, 1, sqrt)
apply(A, 2, function(x) x^2)

## ---- r-13-------------------------------------------------------------------------------
# The "for" statement allows to create loops that run along a
# given vector
# Print 3 times a message (i varies in 1:3)
for (i in 1:3) {
  print(i)
}

# Another example
a <- 0
for (i in 1:3) {
  a <- i + a
}
a

# Nested loops are possible
A <- matrix(0, nrow = 2, ncol = 3)
for (i in 1:2) {
  for (j in 1:3) {
    A[i, j] <- i + j
  }
}

# The "if" statement allows to create conditional structures of
# the forms:
# if (condition) {
#  # Something
# } else {
#  # Something else
# }
# These structures are thought to be inside functions

# Is the number positive?
isPositive <- function(x) {
  if (x > 0) {
    print("Positive")
  } else {
    print("Not positive")
  }
}
isPositive(1)
isPositive(-1)

# A loop can be interrupted with the "break" statement
# Stop when x is above 100
x <- 1
for (i in 1:1000) {
  x <- (x + 0.01) * x
  print(x)
  if (x > 100) {
    break
  }
}

