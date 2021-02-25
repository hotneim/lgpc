
## _________________________ ##
##|                         |## 
##|   Summarize functions   |##
##|_________________________|##
##                           ##

#' Location of this file: rscripts/summarize.R
#'
#' First and foremost this file contains a function function that will summarize the results
#' from all the simulation runs into easily digestible files that can be further formatted by
#' RMarkdown and the like.
#'
#' The summarize_all-function
#' --------------------------
#'
#' Takes the working directory of the simulation experiment as argument, works out all the test
#' configurations and data sets present, and creates som text files with summary. Further arguments:
#'
#'   levels: The level for power, default 5%, but can be changed.
#'   ret_n_completed_tests: create summary of the number of tests completed.
#'   ret_duartion: create summary of mean run time

summarize_all <- function(wd, level = .05, ret_n_completed_tests = TRUE, ret_duration = TRUE) {

    # Create a summary folder, if it does not aleady exist
    if(!("summaries" %in% dir(wd))) {
        dir.create(paste(wd, "summaries", sep = "/"))
    }
    
    # Load the test variations
    tests <- read.table(paste(wd, TEST_FILE, sep = "/"), header = TRUE, stringsAsFactors = FALSE)

    # Naming convention for the dgp files is dgpX.R, and that we start with X = 1 and add sequantially.
    dgp <- list.files(path = paste(wd, DGP_DIR, sep = "/"), pattern = glob2rx("dgp*.R")) %>% 
        gsub("dgp", "", .) %>% 
        gsub(".R", "", .) %>% 
        as.numeric %>% 
        sort

    # We make one set of files for each *test*
    for(i in 1:nrow(tests)) {
        power <- rep(NA, length(dgp))
        if(ret_n_completed_tests) completed <- power
        if(ret_duration) duration <- power

        # Load the results from each results file
        for(j in 1:length(dgp)) {

            # Read file if it exists
            result <- suppressWarnings(try(read.table(paste(wd, "/", RESULTS_DIR, "/test",
                                                            as.character(tests$test_number[i]), "_dgp",
                                                            as.character(dgp[j]), ".txt", sep = ""),
                                                      header = TRUE, stringsAsFactors = FALSE),
                                           silent = TRUE))

            if(class(result) != "try-error") {
                power[j] <- mean(result$p_value < level)
                
                if(ret_n_completed_tests) {
                    completed[j] <- nrow(result)
                }

                if(ret_duration) {
                    duration[j] <- mean(result$duration)
                }                
            }       
        }

        # Write the sumnmary files
        write.table(data.frame(dgp = paste("dgp", as.character(dgp), sep = ""),
                               power = power),
                    file = paste(wd, "/summaries/test", as.character(tests$test_number[i]), "_power.txt", sep = ""),
                    row.names = FALSE,
                    quote = FALSE)
        if(ret_n_completed_tests) {

            write.table(data.frame(dgp = paste("dgp", as.character(dgp), sep = ""),
                                   completed = completed),
                        file = paste(wd, "/summaries/test", as.character(tests$test_number[i]),
                                     "_completed.txt", sep = ""),
                        row.names = FALSE,
                        quote = FALSE)
                   
        }

        if(ret_duration) {

            write.table(data.frame(dgp = paste("dgp", as.character(dgp), sep = ""),
                                   duration = duration),
                        file = paste(wd, "/summaries/test", as.character(tests$test_number[i]),
                                     "_duration.txt", sep = ""),
                        row.names = FALSE,
                        quote = FALSE)
                   
        }  
        
    }
        
        
    
    
}
