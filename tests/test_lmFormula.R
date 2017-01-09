########################################################################################################################################
# Test lmFormula (1)
# Provide lmFormula with a list of variables and interactions from which
# to build a regression formula, and confirm the formula it returns is as expected
########################################################################################################################################

test_that("Test lmFormula (1)", {
  
  yName = "outcome"
  
  terms <- list()
  terms[[1]] <- matrix(c("a","b","c"), nrow = 3)
  terms[[2]] <- matrix(c("a","b","a","c","b","c"), nrow = 3, byrow = TRUE)
  terms[[3]] <- matrix(c("a","b","c"), nrow = 1, byrow = TRUE)
  
  highestTermLevel <- 2
  
  expectedFormula <- as.formula("outcome ~ a:b + a:c + b:c + a + b + c")  
  
  expect_equal(lmFormula(yName, terms, highestTermLevel), expectedFormula)
  
})