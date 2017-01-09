########################################################################################################################################
# Test zeroOutProbabilitiesFromHigherSubterms (1)
# zeroOutProbabilitiesFromHigherSubterms should set to zero any terms already present by way of a term at a higher term level
########################################################################################################################################

test_that("Test zeroOutProbabilitiesFromHigherSubterms (1)", {
  
  # Sample terms
  termOptions <- list()
  termOptions[[1]] <- matrix(c("a","b","c","d","e"), nrow = 5)
  termOptions[[2]] <- matrix(c("a","b","a","c","a","d","a","e","b","c","b","d","b","e","c","d","c","e","d","e"), nrow = 10, byrow = TRUE)
  termOptions[[3]] <- matrix(c("a","b","c","a","b","d","a","b","e","a","c","d","a","c","e","a","d","e","b","c","d",
                               "b","c","e","b","d","e","c","d","e"), nrow = 10, byrow = TRUE)
  termOptions[[4]] <- matrix(c("a","b","c","d","a","b","c","e","a","b","d","e","a","c","d","e","b","c","d","e"), nrow = 5, byrow = TRUE)
  
  # Assemple test term sets
  termSet <- list()
  termSet[[1]] <- matrix(c("a","b"), nrow = 2)
  termSet[[2]] <- matrix(c("a","b","b","c","c","e"), nrow = 3)
  termSet[[3]] <- matrix(c("a","b","d"), nrow = 1)
  termSet[[4]] <- matrix(c("a","b","d","e"), nrow = 1)
     
  termProbabilities <- list()
  termProbabilities[[4]] <- c(0,0,0.5,0,0)
  termProbabilities[[3]] <- c(0,0.5,0,0,0,0,0,0,0,0)
  termProbabilities[[2]] <- c(0.5,0,0,0,0.5,0,0,0,0.5,0)
  termProbabilities[[1]] <- c(.5,.5,0,0,0)
  
  highestTermLevel <- 3  
   
  # Expected results
  expectedTermProbabilities <- list()
  expectedTermProbabilities[[4]] <- c(0,0,0.5,0,0)
  expectedTermProbabilities[[3]] <- c(0,0.5,0,0,0,0,0,0,0,0)
  expectedTermProbabilities[[2]] <- c(0,0,0,0,0.5,0,0,0,0.5,0)
  expectedTermProbabilities[[1]] <- c(0,0,0,0,0)
  
  # Perform tests
  for(i in 1:4) {  
    expect_equal(zeroOutProbabilitiesFromHigherSubterms(termProbabilities, termOptions, termSet, i, highestTermLevel), 
                 expectedTermProbabilities[[i]])  	
  }
  
})