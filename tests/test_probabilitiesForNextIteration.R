########################################################################################################################################
# Test bestTermProbabilities (1)
# 
# bestTermProbabilities should:
#  (a) maintain probabilities for any terms at levels above highestTermLevel
#  (b) Set probabilities to 1 for any terms used in the current particle's regression
#  (c) Set probabilities to 1 for any terms at lower levels that were not explicitly used
#      but which were present by way of the higher-level term
#  (Note that probabilities are not really "1," as they are renormalized before given 
#  to base::sample)
#
########################################################################################################################################

test_that("Test bestTermProbabilities (1)", {  
  
  termOptions <- list()
  termOptions[[1]] <- matrix(c("a","b","c","d"), nrow = 4)
  termOptions[[2]] <- matrix(c("a","b","a","c","a","d","b","c","b","d","c","d"), nrow = 6, byrow = TRUE)
  termOptions[[3]] <- matrix(c("a","b","c","a","b","d","a","c","d","b","c","d"), nrow = 4, byrow = TRUE)
  termOptions[[4]] <- matrix(c("a","b","c","d"), nrow = 1, byrow = TRUE)
  
  termSet <- list()
  termSet[[1]] <- matrix(c("a","b"), nrow = 2)
  termSet[[2]] <- matrix(c("a","b","b","c"), nrow = 2)
  termSet[[3]] <- matrix(c("a","b","d"), nrow = 1)
  
  highestTermLevel <- 3
  maxHighestTermLevel <- 4
   
  termProbabilities <- list()
  termProbabilities[[1]] <- c(.5,.5,.5,.5)
  termProbabilities[[2]] <- c(.5,.5,.5,.5,.5,.5)
  termProbabilities[[3]] <- c(.5,.5,.5,.5)
  termProbabilities[[4]] <- c(.5)
  
  expectedTermProbabilities <- list()
  expectedTermProbabilities[[1]] <- c(1,1,1,1)
  expectedTermProbabilities[[2]] <- c(1,0,1,1,1,0)
  expectedTermProbabilities[[3]] <- c(0,1,0,0)
  expectedTermProbabilities[[4]] <- c(0.5)
  
  expect_equal(probabilitiesForNextIteration(termProbabilities, termSet, termOptions, highestTermLevel, maxHighestTermLevel), expectedTermProbabilities)  
  
})