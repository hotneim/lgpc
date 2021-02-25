
## ________________________ ##
##|                        |##
##|   CONFIGURATION FILE   |##
##|________________________|##
##                          ##

#' Location of this file: config.R
#'
#' This file contains the basic running parameters for the entire simulation experiment, and
#' this particular running session.

#' The size of the experiment
#' --------------------------
#'
#' Here we set the total number of experiments we want to run for each combination of data
#' generating process and test variation. We set this only once, and generate the seeds, so
#' that the different tests run with the same seed. If this is going to be changed at a later
#' time, the seed file must be deleted.

TOTAL_NUMBER <- 1000

#' What to do in this particular run
#' ---------------------------------
#'
#' Specify the specific tasks of this particular iteration of the run. For simplicity, we can only
#' specify one combination of test variation and dgp at the same time.

TEST_NUMBER <- 2           # Which test variation
DGP_NUMBER  <- 9         # The data generating process
N_TESTS     <- 1000        # How many tests to perform in this particular run

#' Naming conventions for the files and other definitions
#' ------------------------------------------------------
#'
#' Some definitions of file names and so on that is used in this experiment

SEED_FILE <- "seeds.txt"                #| The file where the seeds are stored
SEED_MIN  <- -2147483646                #| If we need to generate the seeds, then
SEED_MAX  <- 2147483646                 #| these are the max and min values.

TEST_FILE <- "test_specifications.txt"  #| The file where we specify the test variations

RESULTS_DIR <- "results"
DGP_DIR <- "dgps"

LOG_FILE <- "log.txt"

#' The number of cores to use
#' --------------------------

CORES <- 11
