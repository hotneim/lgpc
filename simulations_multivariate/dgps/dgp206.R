## File: WORKING_DIRECTORY/scripts/dgp/dgp206.R

## ______________________________ ##
##                                ##
##    DATA GENERATING PROCESS     ##
##            data6'              ##
## ______________________________ ##
##                                ##

name <- "data6'"
description <- "Data 4' from Su & White 2014"
sample.data <- function(n) {
    burn.in <- 500
    ret <- matrix(NA, nrow = burn.in + n, ncol = 4)
    ret[1:2,] <- c(rmvnorm(2, sigma = diag(4)))
    for(i in 3:(burn.in + n)) {
        ret[i,2] <- 0.5*ret[i-1, 2] + rnorm(1)
        ret[i,1] <- 0.5*ret[i-1,1] + 0.25*ret[i-2,1] + 0.5*ret[i, 2]^2 + rnorm(1)
        ret[i,3] <- ret[i-1,1]
        ret[i,4] <- ret[i-2,1]
    }
    return(ret[-(1:burn.in),])
}
