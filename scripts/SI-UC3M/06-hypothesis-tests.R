
## ----------------------------------------------------------------------------
## Name: 06-hypothesis-tests.R
## Description: Script for Chapter 6 of "A First Course on Statistical Inference"
## Link: https://bookdown.org/egarpor/inference/
## License: https://creativecommons.org/licenses/by-nc-nd/4.0/
## Author: Isabel Molina Peralta and Eduardo García-Portugués
## Version: 1.3.0
## ----------------------------------------------------------------------------

## ---- qt-2-----------------------------------------------------------------------------------
qt(0.025, df = 7, lower.tail = FALSE)

## ---- pbiom----------------------------------------------------------------------------------
pbinom(3, size = 15, prob = 0.5)

## ---- pt-------------------------------------------------------------------------------------
pt(-1.992, df = 199)

## ---- chi-pval-------------------------------------------------------------------------------
pchisq(9.53, df = 1, lower.tail = FALSE)

