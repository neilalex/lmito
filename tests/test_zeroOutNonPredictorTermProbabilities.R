########################################################################################################################################
# Test zeroOutNonPredictorTermProbabilities (1)
# zeroOutNonPredictorTermProbabilities should set to zero any terms built using variables outside the current variable set
########################################################################################################################################

test_that("Test zeroOutNonPredictorTermProbabilities (1)", {

  predictorVars <- c("a","b")
  
  # Set term options
  termOptions <- list()
  termOptions[[1]] <- matrix(c("a","b","c"), nrow = 3)
  termOptions[[2]] <- matrix(c("a","b","a","c","b","c"), nrow = 3, byrow = TRUE)
  termOptions[[3]] <- matrix(c("a","b","c"), nrow = 1, byrow = TRUE)
  
  # Set term probabilities 
  termProbabilities <- list()
  termProbabilities[[1]] <- c(1,1,1)
  termProbabilities[[2]] <- c(1,1,1)
  termProbabilities[[3]] <- c(1)
  
  highestTermLevel <- 3
  
  # Expected rsult
  expectedResult <- list()
  expectedResult[[1]] <- c(1,1,0)
  expectedResult[[2]] <- c(1,0,0)
  expectedResult[[3]] <- c(0)
  
  # Perform test
  expect_equal(zeroOutNonPredictorTermProbabilities(termProbabilities, termOptions, predictorVars, highestTermLevel), expectedResult)
  
})