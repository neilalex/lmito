########################################################################################################################################
# Function bestTermProbabilities
# Zeros out a list of term probabilities at any term levels above the highest currently allowed term level
#
# Arguments:
#   - A list of current term probabilities
#   - The current highest allowed term level
#   - The overall maximum highest term level ever allowed in the swarm
#
# Return:
#   - A new list of term probabilities with zeros at any term level above that which is currently allowed  
#
########################################################################################################################################

bestTermProbabilities <- function(currentTermProbabilities, termOptions, highestTermLevel, maxHighestTermLevel) {  
  results <- list()
  results <- currentTermProbabilities
  if (highestTermLevel < maxHighestTermLevel) {
    for (i in (highestTermLevel + 1):maxHighestTermLevel) {
      results[[i]] <- rep(0, nrow(termOptions[[i]]))
    }
  }
  results
}