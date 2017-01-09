########################################################################################################################################
# run_tests.R
# Source this file to run lmito unit tests and display results to the console
########################################################################################################################################

# Use testthat package
if (!require(testthat)) install.packages("testthat")

# Source all functions for unit testing
for(file in list.files("R")) {
 source(paste0("R\\", file)) 
}

# Run the tests and print results
test_results <- test_dir("tests", reporter="summary")
print(test_results)