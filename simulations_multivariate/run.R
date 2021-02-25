
## __________________________________________________________ ##
##|                                                          |##
##|  POWER EXPERIMENT FOR TESTING CONDITIONAL INDEPENDENCE   |##
##|                                                          |##
##|        ------ THIS IS THE MAIN SCRIPT ------             |##
##|__________________________________________________________|##
##                                                            ##

#' Introduction
#' ------------
#'
#' This is an attempt to make a much more lightweight simulation experiment than
#' what I have done before. The advantages of generating a ton of information along
#' the way is of course that it is transparent, but it takes up a lot of space and
#' becomes very heavy to work with. I will therefore try a new strategy now, which
#' is more minimalistic, and where we just save the p-value and the duration of the
#' test in readable files. We have the seed, and we save the session info once, after
#' we are done with the simulations.
#'
#' The user must set the running parameters in the configuration file "config.R". 

#' The initial setup
#' -----------------
#'
#' This is the very basic stuff. We really need to set a working directory, but it 
#' will only be done here, so that everything else will be relative. We then load the
#' initialization file ("rscripts/init.R"), which takes care of all the basic initialization,
#' generating seeds if they have not been generated already, load the configurations and test
#' variations into memory and so on. 
 
WORKING_DIRECTORY <- "/home/hakon/Dropbox/nhh/prosjekt/2018/09-lgpc/simulations_multivariate"
setwd(WORKING_DIRECTORY)
source("rscripts/init.R")

#' Perform the tests
#' -----------------
#'
#' Now that all the administration has been done, the tasks is sumarized in the variables
#' TEST_NUMBER and DGP_NUMBER, and the vector do_do specifies the IDs that we have to do.
a <- foreach(i = to_do) %dopar% {

     start_date <- format(Sys.time(), "%Y-%m-%d")
     start_time <- format(Sys.time(), "%H:%M")
     
    # Set the seed
    set.seed(seeds$seed[seeds$ID == i])

    # Load the DGP
    dgp_file <- paste(DGP_DIR, "/dgp", as.character(DGP_NUMBER), ".R", sep = "")
    source(dgp_file)

    # Generate the data
    n <- tests$n[tests$test_number == TEST_NUMBER]
    x <- sample.data(n)
    
    # Some processes have extreme outliers. For those, we use the logarithms
        
    # Create the lg-object
    lg_object <- lg_main(x,
                         bw_method = tests$bw_method[tests$test_number == TEST_NUMBER],
                         est_method = tests$estimation_method[tests$test_number == TEST_NUMBER],
                         plugin_constant_joint = tests$joint_c[tests$test_number == TEST_NUMBER],
                         transform_to_marginal_normality =
                             as.logical(tests$transform[tests$test_number == TEST_NUMBER]))

    # Perform the test
    test_result <- ci_test(lg_object, n_rep = tests$bootstrap_rep[tests$test_number == TEST_NUMBER])

    end_date <- format(Sys.time(), "%Y-%m-%d")
    end_time <- format(Sys.time(), "%H:%M")

    # Save the result
    result_string <- paste(as.character(i), 
                           as.character(test_result$p_value),
                           as.character(test_result$duration), 
                           as.character(seeds$seed[seeds$ID == i]),
                           start_date, start_time,
                           end_date, end_time, 
                           packageVersion("lg"),
                           packageVersion("doMC"),
                           packageVersion("mvtnorm"),
                           packageVersion("magrittr"),
                           paste(version$major, version$minor, sep = "."),                      
                           "\n")
    cat(result_string, file = results_file, append = TRUE)
    
}

#' Write the summary tables
#' ------------------------
#'
#' Go through all the results and write summaries that can be picked up by R Markdown
source("rscripts/summarize.R")
summarize_all(WORKING_DIRECTORY)


