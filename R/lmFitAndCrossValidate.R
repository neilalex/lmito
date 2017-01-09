########################################################################################################################################
# Function lmFitAndCrossValidate
# Fits and cross-validates a linear regression model
#
# Arguments:
#   - A data.frame to build the model from
#   - A number of partitions over which to cross-validate the fitted model
#   - A vector of length equal to that of the number of rows in the above data.frame, containing partition indices for these rows
#   - A string containing the name of the Y outcome variable (which should match one of the columns in the above data.frame)
#   - A formula to use in the regression modeling
#   - A function with which to perform imputation on any potential missing data values in the above data.frame
#   - The appropriate environment for this imputation function (important in case of masking)
#
# Returns:
#   - The out-of-sample mean squared error (MSE) of the predictions of the resulting linear model	
#
########################################################################################################################################

lmFitAndCrossValidate <- function(data, numPartitions, partitions, yName, formula, imputationFunction, imputationFunctionEnvironment) {
  seCV <- 0
  for (i in 1:numPartitions) {
  
  	# Isolate training data and impute if needed
    trainingData <- data[partitions != i, ]
    trainingDataImputed <- do.call(imputationFunction, list(trainingData), envir=imputationFunctionEnvironment)
    
    # Isolate test data and mipute if needed
    testData <- data[partitions == i, ]
    testDataImputed <- do.call(imputationFunction, list(testData), envir=imputationFunctionEnvironment)
    testDataY <- testData[[yName]]
    
    # Regression model
    lin1 <- lm(formula, data=trainingDataImputed)
    scoresCV <- suppressWarnings(stats::predict(lin1, testDataImputed)) # Suppress warnings about poorly-fitting models
    seCV <- seCV + sum((testDataY - scoresCV)^2)
  }
  
  # Mean Squared Error (MSE)
  seCV <- seCV / nrow(data)
}