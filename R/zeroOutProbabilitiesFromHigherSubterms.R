########################################################################################################################################
# Function zeroOutProbabilitiesFromHigherSubterms
# Zeros out the probabilities for any terms that will have already been included by way of higher-level term interactions
#
# Arguments:
#   - A list containing the current set of term probabilities
#   - A list containing the overall set of possible terms
#   - A list containing the current set of terms for the particle
#   - An integer representing the current term level for which to zero out probabilities
#   - An integer representing the highest currently allowable interaction term level 
#
# Returns:
#   - An updated list of term probabilities	
#
########################################################################################################################################

zeroOutProbabilitiesFromHigherSubterms <- function(currentTermProbabilities, termOptions, existingTermSet, termLevel, highestTermLevel) {
  if (termLevel < highestTermLevel) {
    for (k in (termLevel+1):highestTermLevel) {
      nTermsK <- nrow(existingTermSet[[k]])
      if (nTermsK >= 1) {
        for (j in 1:nTermsK) {
          currentTermProbabilities[[termLevel]][
            apply(
              termOptions[[termLevel]], 
              1, 
              function(x) {
                all(x %in% existingTermSet[[k]][j, ])
              }
            )
          ] <- 0
        }
      }
    }
  }
  currentTermProbabilities[[termLevel]]
}