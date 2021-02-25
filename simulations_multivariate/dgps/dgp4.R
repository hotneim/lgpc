## File: WORKING_DIRECTORY/scripts/dgp/data4.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data4               ##
## ______________________________ ##
##                                ##

name <- "data4"
description <- "Data 4 from Cheng and Huang (2012)"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 3)
    h <- matrix(NA, nrow = burn.in + n, ncol = 2)
    ret[1,] <- c(rmvnorm(1, sigma = diag(3)))
    h[1,] <- abs(c(rmvnorm(1, sigma = diag(2))))
    for(i in 2:(burn.in + n)) {
        h[i,1] <- 0.01 + 0.9*h[i-1,1] + 0.05*ret[i-1,1]^2
        h[i,2] <- 0.01 + 0.9*h[i-1,2] + 0.05*ret[i-1,2]^2
        ret[i,1] <- rnorm(1)*sqrt(h[i,1])
        ret[i,2] <- rnorm(1)*sqrt(h[i,2])
        ret[i,3] <- ret[i-1,1]
    }
    return(ret[-(1:burn.in),])
}
