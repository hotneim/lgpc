## File: WORKING_DIRECTORY/scripts/dgp/dgp309.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data9''             ##
## ______________________________ ##
##                                ##

name <- "data9''"
description <- "Data 7'' from Su & White (2014)"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 5)
    ret[1:3,] <- c(rmvnorm(3, sigma = diag(5)))
    for(i in 4:(burn.in + n)) {
        ret[i,2] <- 0.5*ret[i-1, 2] + rnorm(1)
        ret[i,1] <- rnorm(1)*sqrt(0.01 + 0.5*ret[i-1,1]^2 + 0.25*ret[i-2,1]^2 + 0.125*ret[i-3,1]^2 + 0.25*ret[i,2]^2)
        ret[i,3] <- ret[i-1,1]
        ret[i,4] <- ret[i-2,1]
        ret[i,5] <- ret[i-3,1]
    }
    return(ret[-(1:burn.in),])
}
