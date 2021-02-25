## File: WORKING_DIRECTORY/scripts/dgp/gaussian_d3_1

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##        gaussian_d3_1           ##
## ______________________________ ##
##                                ##

name <- "gaussian-d3-1"
description <- "Three-variate standard Gaussian"
sample.data <- function(n) {
    rmvnorm(n, sigma = diag(3))
}
