########################################################################################################################################
# Test bestNTerms (1)
# bestNTerms should set to zero the number of terms at any levels above highestTermLevel
########################################################################################################################################

test_that("Test bestNTerms (1)", {
  #bestNTerms should set to zero the number of terms at any levels above highestTermLevel
	  
  nTerms <- list()
  nTerms[[1]] <- 3
  nTerms[[2]] <- 6
  nTerms[[3]] <- 3
  nTerms[[4]] <- 10
  
  highestTermLevel <- 2
  maxHighestTermLevel <- 4
  
  expectedResult <- list()
  expectedResult[[1]] <- 3
  expectedResult[[2]] <- 6
  expectedResult[[3]] <- 0
  expectedResult[[4]] <- 0  
  
  expect_equal(bestNTerms(nTerms, highestTermLevel, maxHighestTermLevel), expectedResult)  
})