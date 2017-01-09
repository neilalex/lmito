########################################################################################################################################
# Test bestTermProbabilities (1)
# bestTermProbabilities should set to zero any probabilities for terms at levels above highestTermLevel
########################################################################################################################################

test_that("Test bestTermProbabilities (1)", {
  termOptions <- list()
  termOptions[[1]] <- matrix(c("a","b","c"), nrow = 3)
  termOptions[[2]] <- matrix(c("a","b","a","c","b","c"), nrow = 3, byrow = TRUE)
  termOptions[[3]] <- matrix(c("a","b","c"),nrow = 1, byrow = TRUE)
  
  termProbabilities <- list()
  termProbabilities[[1]] <- c(1,1,1)
  termProbabilities[[2]] <- c(1,1,1)
  termProbabilities[[3]] <- c(1)
  
  highestTermLevel <- 1
  maxHighestTermLevel <- 3
  
  expectedResult <- list()
  expectedResult[[1]] <- c(1,1,1)
  expectedResult[[2]] <- c(0,0,0)
  expectedResult[[3]] <- c(0)    
  
  expect_equal(bestTermProbabilities(termProbabilities, termOptions, highestTermLevel, maxHighestTermLevel), expectedResult)  
})