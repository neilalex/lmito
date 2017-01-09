########################################################################################################################################
# Function bestNTerms
# Zeros out a vector of terms counts at any term levels above the term level that is the currently highest allowed 
#
# Arguments:
#   - A vector of current term counts
#   - The current highest allowed term level
#   - The overall maximum highest term level ever allowed in the swarm
#
# Return:
#   - A new vector of term counts with zeros at any term level above that which is currently allowed
#
########################################################################################################################################

bestNTerms <- function(currentNTerms, highestTermLevel, maxHighestTermLevel) {  
  results <- currentNTerms
  if (highestTermLevel < maxHighestTermLevel) {
    for (i in (highestTermLevel + 1):maxHighestTermLevel) {
      results[[i]] <- 0
    }
  }
  results
}