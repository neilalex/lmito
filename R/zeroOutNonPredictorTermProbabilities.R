########################################################################################################################################
# Function zeroOutNonPredictorTermProbabilities
# Zeros out the probabilities for any terms that make use variables outside the currently allowable set
#
# Arguments:
#   - A list containing the current set of term probabilities
#   - A list containing the overall set of possible terms
#   - A vector of currently allowed predictor variables
#   - The highest level at which variable interactions are currently allowed  
#
# Returns:
#   - An updated list of term probabilities
#
########################################################################################################################################

zeroOutNonPredictorTermProbabilities <- function(currentTermProbabilities, termOptions, predictorVars, highestTermLevel) {
  if (highestTermLevel >= 1) {
    for(i in 1:highestTermLevel) {
      currentTermProbabilities[[i]][
        apply(
          termOptions[[i]], 
          1, 
          function(x) {
            any(!(x %in% predictorVars))
          }
        )
        ] <- 0    
    }
  }
  currentTermProbabilities
}