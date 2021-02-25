
## __________________________ ##
##|                          |##
##|   BASIC INITIALIZATION   |##
##|__________________________|##
##                            ##

#' Location of this file: rscripts/init.R
#'
#' This code will take care of the very basic initialization of the power experiment
#' in the conditional independence project. First we load the configuation file, that
#' contains all the parameters that we need in order to perform this experiment.

source("config.R")

#' Load the lg-package
#' -------------------
#'
#' This is the lg-package, which contains the conditional independence test. We also need
#' some parallel stuff and so on.

# devtools::install_github("hotneim/lg")
library(lg)
library(doMC)
registerDoMC()
options(cores = CORES)
library(mvtnorm)
library(magrittr)

#' Take care of the seeds
#' ----------------------
#'
#' First, we check whether the seed-file exists. If it does, then the experiment is under
#' way, and we can go on to load what we need in order to start the simulations.

if(!file.exists(SEED_FILE)) {

    # Generate the seeds to be used in the simulation experiment, one for each realization
    # of the dgps. That means that the corresponding generations from each data set shares
    # seed.
    seeds <- round(runif(n = TOTAL_NUMBER, min = SEED_MIN, max = SEED_MAX))

    # Save the seeds in a text file that we can refer to later
    write.table(x = data.frame(ID = 1:TOTAL_NUMBER, seed = seeds),
                file = SEED_FILE,
                row.names = FALSE)
}

#' Load the seeds
#' --------------
#' 
#' Now, the seed file exists, and we can load it into memory for use when generating test data.

seeds <- read.table(SEED_FILE, header = TRUE)

#' Load the test specifications
#' ----------------------------
#'
#' The file for test specifications is readable and configurable by the user. This is where we
#' specify the parameters for each test type, and the TEST_NUMBER variable refers to lines in
#' this file.

tests <- read.table(TEST_FILE, header = TRUE, stringsAsFactors = FALSE)

#' Figure out what has already been done
#' -------------------------------------
#'
#' Now we have the seeds, and we have loaded the test file. Step into the results-folder (or
#' create it if needed) and figure out what needs to be done. This will simply result in a
#' vector to_do, which contains the IDs of the test runs that will be performed this time.

# Create the results directory if needed
if(!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR)

# Create the filename for this test configuration
results_file <- paste(RESULTS_DIR, "/test", as.character(TEST_NUMBER),
                      "_dgp", as.character(DGP_NUMBER),
                      ".txt", sep = "")

# Check if the results file for this test configuration already exists. If not, create it,
# and just start working. If it does exist, then load the results, and find work to do.
if(!file.exists(results_file)) {
    cat("ID p_value duration seed start_date start_time end_date end_time lg_version doMC_version mvtnorm_version magrittr_version R_version \n", file = results_file)
    to_do <- 1:min(N_TESTS, TOTAL_NUMBER)
} else {
    done <- read.table(results_file, header = TRUE, stringsAsFactors = FALSE)
    missing <- (1:TOTAL_NUMBER)[!(1:TOTAL_NUMBER %in% done$ID)]
    to_do <- missing[1:min(N_TESTS, length(missing))]   
}

