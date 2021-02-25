## File: WORKING_DIRECTORY/scripts/dgp/data6.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data6               ##
## ______________________________ ##
##                                ##

name <- "data6"
description <- "Data 6 from Cheng and Huang (2012)"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 3)
    ret[1,] <- c(rmvnorm(1, sigma = diag(3)))
    for(i in 2:(burn.in + n)) {
        ret[i,2] <- 0.5*ret[i-1, 2] + rnorm(1)
        ret[i,1] <- 0.5*ret[i-1,1] + 0.5*ret[i, 2]^2 + rnorm(1)
        ret[i,3] <- ret[i-1,1]
    }
    return(ret[-(1:burn.in),])
}
