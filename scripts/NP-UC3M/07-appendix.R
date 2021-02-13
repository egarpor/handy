
## ------------------------------------------------------------------------
## Name: 07-appendix.R
## Description: Script for Chapter 7 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- ci-1, fig.cap = '(ref:ci-1-title)', fig.margin = FALSE--------------------------------------------------------------
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
var_kde_hat <- kde$y * Rk / (n * h)

# True expectation and variance (because the density is a normal)
E_kde <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
var_kde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
              (2 * sqrt(pi) * h) - E_kde^2) / n

# CI with estimated variance
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
ci_low_1 <- kde$y - z_alpha * sqrt(var_kde_hat)
ci_up_1 <- kde$y + z_alpha * sqrt(var_kde_hat)

# CI with known variance
ci_low_2 <- kde$y - z_alpha * sqrt(var_kde)
ci_up_2 <- kde$y + z_alpha * sqrt(var_kde)

# Plot estimate, CIs and expectation
plot(kde, main = "Density and CIs", ylim = c(0, 1))
lines(kde$x, ci_low_1, col = "gray")
lines(kde$x, ci_up_1, col = "gray")
lines(kde$x, ci_low_2, col = "gray", lty = 2)
lines(kde$x, ci_up_2, col = "gray", lty = 2)
lines(kde$x, E_kde, col = "red")
legend("topright", legend = c("Estimate", "CI estimated var",
                              "CI known var", "Smoothed density"),
       col = c("black", "gray", "gray", "red"), lwd = 2, lty = c(1, 1, 2, 1))


## ---- ci-2, fig.cap = '(ref:ci-2-title)', fig.margin = FALSE--------------------------------------------------------------
# Simulation setting
n <- 200; h <- 0.15
mu <- 0; sigma <- 1 # Normal parameters
M <- 5e2 # Number of replications in the simulation
n_grid <- 512 # Number of x's for computing the kde
alpha <- 0.05; z_alpha <- qnorm(1 - alpha/2) # alpha for CI

# Compute expectation and variance
kde <- density(x = 0, bw = h, from = -4, to = 4, n = n_grid) # Just for kde$x
E_kde <- dnorm(x = kde$x, mean = mu, sd = sqrt(sigma^2 + h^2))
var_kde <- (dnorm(kde$x, mean = mu, sd = sqrt(h^2 / 2 + sigma^2)) /
              (2 * sqrt(pi) * h) - E_kde^2) / n

# For storing if the mean is inside the CI
inside_ci_1 <- inside_ci_2 <- matrix(nrow = M, ncol = n_grid)

# Simulation
set.seed(12345)
for (i in 1:M) {

  # Sample & kde
  x <- rnorm(n = n, mean = mu, sd = sigma)
  kde <- density(x = x, bw = h, from = -4, to = 4, n = n_grid)
  sd_kde_hat <- sqrt(kde$y * Rk / (n * h))

  # CI with estimated variance
  ci_low_1 <- kde$y - z_alpha * sd_kde_hat
  ci_up_1 <- kde$y + z_alpha * sd_kde_hat

  # CI with known variance
  ci_low_2 <- kde$y - z_alpha * sqrt(var_kde)
  ci_up_2 <- kde$y + z_alpha * sqrt(var_kde)

  # Check if for each x the mean is inside the CI
  inside_ci_1[i, ] <- E_kde > ci_low_1 & E_kde < ci_up_1
  inside_ci_2[i, ] <- E_kde > ci_low_2 & E_kde < ci_up_2

}

# Plot results
plot(kde$x, colMeans(inside_ci_1), ylim = c(0.25, 1), type = "l",
     main = "Empirical coverage of CIs", xlab = "x", ylab = "Coverage")
lines(kde$x, colMeans(inside_ci_2), col = 4)
abline(h = 1 - alpha, col = 2)
abline(h = 1 - alpha + c(-1, 1) * qnorm(0.975) *
         sqrt(alpha * (1 - alpha) / M), col = 2, lty = 2)
legend(x = "bottom", legend = c("CI estimated var", "CI known var",
                                "Nominal level", 
                                "95% CI for the nominal level"),
       col = c(1, 4, 2, 2), lwd = 2, lty = c(1, 1, 1, 2))




## ---- manipulate-1, eval = FALSE------------------------------------------------------------------------------------------
## # Sample
## x <- rnorm(100)
## 
## # Simple plot of kde for varying h
## manipulate::manipulate({
## 
##   kde <- density(x = x, from = -4, to = 4, bw = h)
##   plot(kde, ylim = c(0, 1), type = "l", main = "")
##   curve(dnorm(x), from = -4, to = 4, col = 2, add = TRUE)
##   rug(x)
## 
## }, h = manipulate::slider(min = 0.01, max = 2, initial = 0.5, step = 0.01))








## ---- lm------------------------------------------------------------------------------------------------------------------
# Create the data employed in Figure 3.1

# Generates 50 points from a N(0, 1): predictors and error
set.seed(34567)
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- x1 + rnorm(50, sd = 0.05) # Make variables dependent
eps <- rnorm(50)

# Responses
y_lin <- -0.5 + 0.5 * x1 + 0.5 * x2 + eps
y_qua <- -0.5 + x1^2 + 0.5 * x2 + eps
y_exp <- -0.5 + 0.5 * exp(x2) + x3 + eps

# Data
data_animation <- data.frame(x1 = x1, x2 = x2, y_lin = y_lin,
                             y_qua = y_qua, y_exp = y_exp)

# Call lm
# lm employs formula = response ~ predictor1 + predictor2 + ...
# (names according to the data frame names) for denoting the regression
# to be done
mod <- lm(y_lin ~ x1 + x2, data = data_animation)
summary(mod)

# mod is a list with a lot of information
# str(mod) # Long output

# Coefficients
mod$coefficients

# Application of formula (3.4)

# Matrix X
X <- cbind(1, x1, x2)

# Vector Y
Y <- y_lin

# Coefficients
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta
















## ---- glm-1---------------------------------------------------------------------------------------------------------------
# Create the data employed in Figure 3.4

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
data_mle <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3)


## ---- glm-2---------------------------------------------------------------------------------------------------------------
# Call glm
# glm employs formula = response ~ predictor1 + predictor2 + ...
# (names according to the data frame names) for denoting the regression
# to be done. We need to specify family = "binomial" to make a
# logistic regression
mod <- glm(y1 ~ x, family = "binomial", data = data_mle)
summary(mod)

# mod is a list with a lot of information
# str(mod) # Long output

# Coefficients
mod$coefficients

# Plot the fitted regression curve
x_grid <- seq(-5, 5, l = 200)
y_grid <- 1 / (1 + exp(-(mod$coefficients[1] + mod$coefficients[2] * x_grid)))
plot(x_grid, y_grid, type = "l", col = 2, xlab = "x", ylab = "y")
points(x, y1)

