## File: WORKING_DIRECTORY/scripts/dgp/data3.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data3               ##
## ______________________________ ##
##                                ##

name <- "data3"
description <- "Data 3 from Cheng and Huang (2012)"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 3)
    ret[1,] <- c(rmvnorm(1, sigma = diag(3)))
    for(i in 2:(burn.in + n)) {
        ret[i,1] <- rnorm(1)*sqrt(0.01 + 0.5*ret[i-1,1]^2)
        ret[i,2] <- 0.5*ret[i-1,2] + rnorm(1)
        ret[i,3] <- ret[i-1,1]
    }
    return(ret[-(1:burn.in),])
}
