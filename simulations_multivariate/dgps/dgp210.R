## File: WORKING_DIRECTORY/scripts/dgp/dgp210.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data10'             ##
## ______________________________ ##
##                                ##

name <- "data10'"
description <- "Data 8' from Su & White (2014)"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 4)
    h <- matrix(NA, nrow = burn.in + n, ncol = 2)
    ret[1:2,] <- c(rmvnorm(2, sigma = diag(4)))
    h[1:2,] <- abs(c(rmvnorm(2, sigma = diag(2))))
    for(i in 3:(burn.in + n)) {
        h[i,2] <- 0.01 + 0.9*h[i-1,2] + 0.05*ret[i-1,1]^2
        ret[i,2] <- rnorm(1)*sqrt(h[i,2])
        h[i,1] <- 0.01 + 0.1*h[i-1,1] + 0.4*ret[i-1,1]^2 + 0.5*ret[i,2]^2
        ret[i,1] <- rnorm(1)*sqrt(h[i,1])
        ret[i,3] <- ret[i-1,1]
        ret[i,4] <- ret[i-2,1]
    }
    return(ret[-(1:burn.in),])
}
