
## ------------------------------------------------------------------------
## Name: 03-kde-ii.R
## Description: Script for Chapter 3 of "Notes for Predictive Modeling"
## Link: https://bookdown.org/egarpor/NP-UC3M/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Eduardo García-Portugués
## ------------------------------------------------------------------------

## ---- ks-2d-1------------------------------------------------------------
# DPI selectors
Hpi1 <- ks::Hpi(x = faithful)
Hpi1

# Compute kde (if H is missing, ks::Hpi is called)
kdeHpi1 <- ks::kde(x = faithful, H = Hpi1)

# Different representations
plot(kdeHpi1, display = "slice", cont = c(25, 50, 75))
# "cont" specifies the density contours, which are upper percentages of highest
# density regions. The default contours are at 25%, 50%, and 75%
plot(kdeHpi1, display = "filled.contour2", cont = c(25, 50, 75))
plot(kdeHpi1, display = "persp")

# Manual plotting using the kde object structure
image(kdeHpi1$eval.points[[1]], kdeHpi1$eval.points[[2]],
      kdeHpi1$estimate, col = viridis::viridis(20))
points(kdeHpi1$x)

# Diagonal vs. full
Hpi2 <- ks::Hpi.diag(x = faithful)
kdeHpi2 <- ks::kde(x = faithful, H = Hpi2)
plot(kdeHpi1, display = "filled.contour2", cont = c(25, 50, 75),
     main = "full")
plot(kdeHpi2, display = "filled.contour2", cont = c(25, 50, 75),
     main = "diagonal")

## ---- ks-2d-2, fig.margin = FALSE----------------------------------------
# Comparison of selectors along predefined contours
x <- faithful
Hlscv0 <- ks::Hlscv(x = x)
Hbcv0 <- ks::Hbcv(x = x)
Hpi0 <- ks::Hpi(x = x)
Hns0 <- ks::Hns(x = x)
par(mfrow = c(2, 2))
p <- lapply(list(Hlscv0, Hbcv0, Hpi0, Hns0), function(H) {
  # col.fun for custom colours
  plot(ks::kde(x = x, H = H), display = "filled.contour2",
       cont = seq(10, 90, by = 10), col.fun = viridis::viridis)
  points(x, cex = 0.5, pch = 16)
})

## ---- ks-3d, eval = knitr:::is_html_output()-----------------------------
## # Normal scale bandwidth
## Hns1 <- ks::Hns(iris[, 1:3])
## 
## # Show high nested contours of high density regions
## plot(ks::kde(x = iris[, 1:3], H = Hns1))
## points3d(x = iris[, 1:3])
## rgl::rglwidget()

## ---- level-set-1, fig.cap = '(ref:level-set-1-title)'-------------------
# Simulated sample
n <- 100
set.seed(12345)
samp <- rnorm(n = n)

# Kde as usual, but force to evaluate it at seq(-4, 4, length = 4096)
bw <- bw.nrd(x = samp)
kde <- density(x = samp, bw = bw, n = 4096, from = -4, to = 4)

# For a given c, what is the theoretical level set? Since we know that the real 
# density is symmetric and unimodal, then the level set is an inverval of the 
# form [-x_c, x_c]
c <- 0.2
x_c <- tryCatch(uniroot(function(x) dnorm(x) - c, lower = 0, upper = 4)$root,
                error = function(e) NA)

# Show theoretical level set
x <- seq(-4, 4, by = 0.01)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.5), ylab = "Density")
rug(samp)
polygon(x = c(-x_c, -x_c, x_c, x_c), y = c(0, c, c, 0), 
        col = rgb(0, 0, 0, alpha = 0.5), density = 10)

# Function to compute and plot a kde level set
kde_level_set <- function(kde, c, add_plot = FALSE, ...) {

  # Begin and end index for the potantially many intervals in the level sets 
  # of the kde
  kde_larger_c <- kde$y >= c
  run_length_kde <- rle(kde_larger_c) # Trick to compute the legth of the
  # sequence of TRUEs that indicates an interval for which kde$y >= c
  begin <- which(diff(kde_larger_c) > 0) # Trick to search for the begin of 
  # each of the intervals
  end <- begin + run_length_kde$lengths[run_length_kde$values] - 1 # Compute the 
  # end of the intervals from begin + length

  # Add polygons to a density plot? If so, ... are the additional parameters 
  # for polygon()
  if (add_plot) {
    
    apply(cbind(begin, end), 1, function(ind) {
      polygon(x = c(kde$x[ind[1]], kde$x[ind[1]], 
                    kde$x[ind[2]], kde$x[ind[2]]), 
              y = c(0, kde$y[ind[1]], 
                    kde$y[ind[2]], 0), ...)
      })
  
  }
  
  # Return the [a_i, b_i], i = 1, ..., K in the K rows
  return(cbind(kde$x[begin], kde$x[end]))
  
}

# Add kde and level set
lines(kde, col = 2)
kde_level_set(kde = kde, c = c, add_plot = TRUE, 
              col = rgb(1, 0, 0, alpha = 0.5))
abline(h = c, col = 4) # Level
legend("topright", legend = c("True density", "Kde", "True level set", 
                              "Kde level set", "Level c"), 
       lwd = 2, col = c(1, 2, rgb(0:1, 0, 0, alpha = 0.5), 4))

## ---- level-set-2, eval = FALSE------------------------------------------
## # Simulated sample
## n <- 100
## set.seed(12345)
## samp <- rnorm(n = n)
## 
## # Interactive visualization
## x <- seq(-4, 4, by = 0.01)
## manipulate::manipulate({
## 
##   # Show theoretical level set
##   plot(x, dnorm(x), type = "l", ylim = c(0, 0.5), ylab = "Density")
##   rug(samp)
##   x_c <- tryCatch(uniroot(function(x) dnorm(x) - c, lower = 0, upper = 4)$root,
##                   error = function(e) NA) # tryCatch() to bypass errors
##   polygon(x = c(-x_c, -x_c, x_c, x_c), y = c(0, c, c, 0),
##           col = rgb(0, 0, 0, alpha = 0.5), density = 10)
## 
##   # Add estimation
##   kde <- density(x = samp, bw = bw, n = 1e5, from = -4, to = 4)
##   lines(kde, col = 2)
##   kde_level_set(kde = kde, c = c, add_plot = TRUE,
##                 col = rgb(1, 0, 0, alpha = 0.5))
##   abline(h = c, col = 4) # Level
##   legend("topright", legend = c("True density", "Kde", "True level set",
##                                 "Kde level set", "Level c"),
##          lwd = 2, col = c(1, 2, rgb(0:1, 0, 0, alpha = 0.5), 4))
## 
## }, c = manipulate::slider(min = 0.01, max = 0.5, initial = 0.2, step = 0.01),
## bw = manipulate::slider(min = 0.01, max = 1, initial = 0.25, step = 0.01))

## ---- level-set-3, fig.cap = '(ref:level-set-3-title)'-------------------
# Simulate sample
n <- 200
set.seed(12345)
samp <- rnorm(n = n)

# We want to estimate the highest density region containing 0.75 probability
alpha <- 0.25

# For the N(0, 1), we know that this region is the interval [-x_c, x_c] with
x_c <- qnorm(1 - alpha / 2)
c_alpha <- dnorm(x_c)

# This corresponds to the c_alpha

# Theoretical level set
x <- seq(-4, 4, by = 0.01)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.5), ylab = "Density")
rug(samp)
polygon(x = c(-x_c, -x_c, x_c, x_c), y = c(0, c_alpha, c_alpha, 0), 
        col = rgb(0, 0, 0, alpha = 0.5), density = 10)
abline(h = c_alpha, col = 3, lty = 2) # Level

# Kde 
bw <- bw.nrd(x = samp)
c_alpha_hat <- quantile(ks::kde(x = samp, h = bw, eval.points = samp)$estimate, 
                        probs = alpha)
kde <- density(x = samp, bw = bw, n = 4096, from = -4, to = 4)
lines(kde, col = 2)
kde_level_set(kde = kde, c = c_alpha_hat, add_plot = TRUE, 
              col = rgb(1, 0, 0, alpha = 0.5))
abline(h = c_alpha_hat, col = 4, lty = 2) # Level
legend("topright", legend = expression("True density", "Kde", "True level set", 
                                       "Kde level set", "Level " * c[alpha], 
                                       "Level " * hat(c)[alpha]), 
       lwd = 2, col = c(1, 2, rgb(0:1, 0, 0, alpha = 0.5), 3:4), 
       lty = c(rep(1, 4), rep(2, 4)))

## ---- level-set-4--------------------------------------------------------
# N(0, 1) case
alpha <- 0.3
x_c <- qnorm(1 - alpha / 2)
c_alpha <- dnorm(x_c)
c_alpha

# Approximates c_alpha
quantile(dnorm(samp), probs = alpha)

# True integral: 1 - alpha (by construction)
1 - 2 * pnorm(-x_c)

# Monte Carlo integration, approximates 1 - alpha
mean(dnorm(samp) >= c_alpha)

## ---- level-set-5--------------------------------------------------------
# Simulated sample from a mixture of normals
n <- 200
set.seed(123456)
mu <- c(2, 2)
Sigma1 <- diag(c(2, 2))
Sigma2 <- diag(c(1, 1))
samp <- rbind(mvtnorm::rmvnorm(n = n / 2, mean = mu, sigma = Sigma1),
              mvtnorm::rmvnorm(n = n / 2, mean = -mu, sigma = Sigma2))

# Level set of the true density at levels c
c <- c(0.01, 0.03)
x <- seq(-5, 5, by = 0.1)
xx <- as.matrix(expand.grid(x, x))
contour(x, x, 0.5 * matrix(mvtnorm::dmvnorm(xx, mean = mu, sigma = Sigma1) + 
                             mvtnorm::dmvnorm(xx, mean = -mu, sigma = Sigma2), 
                           nrow = length(x), ncol = length(x)), 
        levels = c)

# Plot of the contour level
H <- ks::Hpi(x = samp)
kde <- ks::kde(x = samp, H = H)
plot(kde, display = "slice", abs.cont = c, add = TRUE, col = 4) # Argument 
# "abs.cont" for specifying c rather than (1 - alpha) * 100 in "cont"
legend("topleft", lwd = 2, col = c(1, 4),
       legend = expression(L * "(" * f * ";" * c * ")", 
                           L * "(" * hat(f) * "(" %.% ";" * H * "), " * c *")"))

# Computation of the probability cumulated in the level sets by numerical 
# integration
ks::contourSizes(kde, abs.cont = c)

## ---- level-set-6--------------------------------------------------------
# Simulate a sample from a mixture of normals
n <- 5e2
set.seed(123456)
mu <- c(2, 2, 2)
Sigma1 <- rbind(c(1, 0.5, 0.2), 
                c(0.5, 1, 0.5), 
                c(0.2, 0.5, 1))
Sigma2 <- rbind(c(1, 0.25, -0.5), 
                c(0.25, 1, 0.5), 
                c(-0.5, 0.5, 1))
samp <- rbind(mvtnorm::rmvnorm(n = n / 2, mean = mu, sigma = Sigma1),
              mvtnorm::rmvnorm(n = n / 2, mean = -mu, sigma = Sigma2))

# Plot of the contour level, changing the color palette
H <- ks::Hns(x = samp)
kde <- ks::kde(x = samp, H = H)
plot(kde, cont = 100 * c(0.99, 0.95, 0.5), col.fun = viridis::viridis, 
     drawpoints = TRUE, col.pt = 1)
rgl::rglwidget()

# Simulate a large sample from a single normal
n <- 5e4
set.seed(123456)
mu <- c(0, 0, 0)
Sigma <- rbind(c(1, 0.5, 0.2), 
                c(0.5, 1, 0.5), 
                c(0.2, 0.5, 1))
samp <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = Sigma)

# Plot of the contour level
H <- ks::Hns(x = samp)
kde <- ks::kde(x = samp, H = H)
plot(kde, cont = 100 * c(0.75, 0.5, 0.25), xlim = c(-2.5, 2.5), 
     ylim = c(-2.5, 2.5), zlim = c(-2.5, 2.5))
rgl::rglwidget()

## ---- level-set-7--------------------------------------------------------
# Compute kde of unicef dataset
data("unicef", package = "ks")
kde <- ks::kde(x = unicef)

# ks::ksupp evaluates whether the points in the grid spanned by ks::kde belong
# to the level set for alpha = 0.05 and then returns the points that belong to 
# the level set
sup <- ks::ksupp(fhat = kde, cont = 95) # Effective support up to a 5% of data
plot(sup)

## ---- level-set-8--------------------------------------------------------
# The convex hull boundary of the level set can be computed with chull()
# It returns the indexes of the points passed that form the corners of the 
# polygon of the convex hull
ch <- chull(sup)
plot(sup)
lines(sup[c(ch, ch[1]), ], col = 2, lwd = 2)
# One extra point for closing the polygon

## ---- level-set-9--------------------------------------------------------
# Compute the convex hull of sup via geometry::convhulln()
C <- geometry::convhulln(p = sup)
# The output of geometry::convhulln() is different from chull()

# The geometry::inhulln() allows to check if points are inside the convex hull
geometry::inhulln(ch = C, p = rbind(c(50, 50), c(150, 50)))

# The convex hull works as well in R^p. An example in which the level set is 
# evaluated by Monte Carlo and then the convex hull of the points in the level 
# set is computed

# Sample
set.seed(2134)
samp <- mvtnorm::rmvnorm(n = 1e2, mean = rep(0, 3))

# Evaluation sample: random data in [-3, 3]^3
M <- 1e3
eval_set <- matrix(runif(n = 3 * M, -3, 3), M, 3) 

# Kde of samp, evaluated at eval_set
H <- ks::Hns.diag(samp)
kde <- ks::kde(x = samp, H = H, eval.points = eval_set)

# Convex hull of points in the level set for a given c
c <- 0.01
C <- geometry::convhulln(p = eval_set[kde$estimate > c, ])

# We can test if a new point belongs to the level set by just checking if it 
# belongs to the convex hull, without the need of re-evaluating the kde, which
# is much more efficient
new_points <- rbind(c(1, 1, 1), c(2, 2, 2))
geometry::inhulln(ch = C, p = new_points)
ks::kde(x = samp, H = H, eval.points = new_points)$estimate > c

# # Performance evaluation
# microbenchmark::microbenchmark(
#   geometry::inhulln(ch = C, p = new_points),
#   ks::kde(x = samp, H = H, eval.points = new_points)$estimate > c)

## ---- ks-bug, error = TRUE-----------------------------------------------
# Sample test data
p <- 4
data <- mvtnorm::rmvnorm(n = 10, mean = rep(0, p))
ks::kde(x = data, H = diag(rep(1, p))) # Error related with the verbose argument

## ---- ks-bug-patch-------------------------------------------------------
# Create a copy of the funcion you want to modify
kde.points.fixed <- ks:::kde.points

# Modify the function (in this case, just replace the default arguments via the 
# formals() function)
f <- formals(fun = kde.points.fixed)
f$verbose <- FALSE
formals(fun = kde.points.fixed, envir = environment(kde.points.fixed)) <- f

# Overwrite original function (careful -- you will have to restart session to 
# come back to the previous object)
assignInNamespace(x = "kde.points", value = kde.points.fixed, ns = "ks", 
                  pos = 3) 
# ns = "ks" to indicate the package namespace, pos = 3 to indicate :::

# Check the result
ks:::kde.points

## ---- ref:level-set-11---------------------------------------------------
alpha <- 0.4
p <- 2
c_alpha <- exp(-0.5 * qchisq(p = 1 - alpha, df = p)) / 
  (sqrt(det(Sigma)) * (2 * pi)^(p / 2))

