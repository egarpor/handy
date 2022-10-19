
## ----------------------------------------------------------------------------
## Name: 01-preliminaries.R
## Description: Script for Chapter 1 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.4.5
## ----------------------------------------------------------------------------

## ---- toss-coin-freq-1, echo = FALSE-----------
set.seed(123456)
n <- c(10, 20, 30, 100, 1000)
toss <- sample(c("heads", "tails"), size = max(n), replace = TRUE)
p <- cumsum(toss == "heads") / (1:max(n))
knitr::kable(x = cbind(n, p[n], 1 - p[n]),
             col.names = c("$n$", "Heads", "Tails"), booktabs = TRUE,
             caption = "Relative frequencies of \"heads\" and \"tails\" for $n$ random experiments.",
             digits = 3, align = c("lcc"), escape = FALSE)

## ---- car-accidents-1, echo = FALSE, cache = TRUE----
set.seed(123456)
lambda <- 4
n <- c(10, 20, 30, 100, 1000, 10000)
accidents <- pmin(rpois(max(n), lambda), 6)
rel_freq_accidents <- sapply(0:6,
                            function(k) cumsum(k == accidents) / (1:max(n)))
knitr::kable(x = cbind(n, rel_freq_accidents[n, ]),
             col.names = c("$n$", paste0("$", 0:5, "$"), "$\\geq 6$"),
             caption = "Relative frequencies of car accidents in Spain for $n$ hours.",
             booktabs = TRUE, digits = 3, align = c("lccccccccc"),
             escape = FALSE)

## ---- pedestrian-1, echo = FALSE, cache = TRUE----
set.seed(123456)
n <- c(10, 20, 30, 100, 1000, 5000)
weights <- rnorm(max(n), mean = 49, sd = 5)
intervals <- cut(weights, breaks = c(0, 35, 45, 55, 65, Inf))
rel_freq_intervals <- sapply(levels(intervals),
                             function(w) cumsum(w == intervals) / (1:max(n)))
knitr::kable(x = cbind(n, rel_freq_intervals[n, ]),
             col.names = c("$n$", "$[0, 35)$", "$[35, 45)$", "$[45, 55)$",
                           "$[55, 65)$", "$[65, \\infty)$"),
             caption = "Relative frequencies of weight intervals for $n$ measured pedestrians.",
             booktabs = TRUE, digits = 3, align = c("lccccc"),
             escape = FALSE)

