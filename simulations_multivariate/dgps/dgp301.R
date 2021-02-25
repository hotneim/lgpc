## File: WORKING_DIRECTORY/scripts/dgp/dgp301.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##        gaussian_d5_1           ##
## ______________________________ ##
##                                ##

name <- "gaussian-d5-1"
description <- "Five-variate standard Gaussian"
sample.data <- function(n) {
    rmvnorm(n, sigma = diag(5))
}
