########################################################################################################################################
# Test lmFitAndCrossValidate (1)
# Provide a sample set of X and Y values to lmFitAndCrossValidate and test if the resulting MSE is within the expected range 
#
# TODO: create a deterministic test that checks for an exact MSE
#
########################################################################################################################################

test_that("Test lmFitAndCrossValidate (1)", {  

  library(randomForest)
  
  data <- data.frame(1:20)
  
  for(i in 1:100) {
  
    # Sample X and Y values
    data$xval <- sample(5:15, 20, replace = TRUE)
    data$yval <- 2 + 3*data$xval + sample(-3:3, 20, replace = TRUE)
	  
	  # Creat x-val partitiosn
    partitions <- sample(1:10, 20, replace = TRUE)

    formula <- as.formula("yval ~ xval")

		# Impute as needed		
    imputationFunction <- randomForest::na.roughfix
    imputationEnvironment <- environment(randomForest::na.roughfix)

		# MSE should be ~9 given above samples
    expectedMaxSE <- 9
		
    expect_true(lmFitAndCrossValidate(data, 10, partitions, "yval", formula, imputationFunction, imputationEnvironment) < expectedMaxSE)
    expect_true(lmFitAndCrossValidate(data, 10, partitions, "yval", formula, imputationFunction, imputationEnvironment) > 0)		
		
  }
})