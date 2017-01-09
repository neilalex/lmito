########################################################################################################################################
# Function setProbabilitiesAndTerms
# Applies rules adjusting term probabilities, and then chooses terms for the regression
#
# Arguments:
#   - The current set of term probabilities
#   - The allowable set of term options from which to choose for the regression
#   - The allowable set of predictor variables from which to choose for the regression
#   - An integer representing the highest interaction term level allowable in the current particle
#   - An integer representing the overall maximum highest term level ever allowed in the swarm
#
# Returns:
#   - A two-element vector containing the following:
#       - Adjusted term probabilities
#       - Output set of terms for the regression
#
#   - Probabilities should be zero for any terms built using variables outside the current variable set
#
########################################################################################################################################

setProbabilitiesAndTerms <- function(currentTermProbabilities, termOptions, predictorVars, highestTermLevel, nTermsAtLevel) {
  currentTermProbabilities <- zeroOutNonPredictorTermProbabilities(currentTermProbabilities, termOptions, predictorVars, highestTermLevel)
  terms <- list()  
  for (i in highestTermLevel:1) {    
  
    # Probabilities should be zero for any terms already present by way of a term at a higher term level
    currentTermProbabilities[[i]] <- zeroOutProbabilitiesFromHigherSubterms(currentTermProbabilities, termOptions, terms, i, highestTermLevel)
    
    # Reduce the number of terms in case expansions of higher-level terms did not leave enough terms to choose from at the current level
    nTermsAtLevel[i] <- min(nTermsAtLevel[i], length(currentTermProbabilities[[i]][currentTermProbabilities[[i]]>0]))    
    
    # Choose terms for the current term level
    if (nTermsAtLevel[i] >= 1) {
      terms[[i]] <- termOptions[[i]][base::sample(nrow(termOptions[[i]]), size=nTermsAtLevel[i], prob=currentTermProbabilities[[i]], replace=FALSE), , drop=FALSE]
    } else {
      terms[[i]] <- matrix(numeric(0))
    }    
    
  }  
  list(currentTermProbabilities, terms)
}