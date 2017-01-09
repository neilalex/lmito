########################################################################################################################################
# Function probabilitiesForNextIteration
# Adjusts the current set of term probabilities for use in the next particle swarm iteration
#
# Arguments:
#   - A list representing the current set of term probabilities in the particle
#   - A list representing the current set of terms in the particle
#   - A list representing the overall allowable set of terms in the particle
#   - An integer representing the highest interaction term level allowable in the current particle
#   - An integer representing the overall maximum highest term level ever allowed in the swarm
#
# Returns:
#   - An adjusted set of probabilities for passing to the next iteration
#
########################################################################################################################################

probabilitiesForNextIteration <- function(currentTermProbabilities, termSet, termOptions, highestTermLevel, maxHighestTermLevel) {  
  resultingProbabilities <- list()
  for (i in maxHighestTermLevel:1) {    
  
    # Keep probabilities consistent into the next iteration for any terms at levels above those used in the current particle
    if (i > highestTermLevel) {      
      resultingProbabilities[[i]] <- currentTermProbabilities[[i]]      
    } else {      
    
      # Make sure that all sub-terms of any terms from higher levels are also counted at the current term level
      if (i < highestTermLevel) {
        nTermsIplus1 <- nrow(termSet[[i+1]])        
        
        # Loop across terms at the next-highest term level and add any discovered sub-terms
        if (nTermsIplus1 >= 1) {
          for (k in 1:nTermsIplus1) { 
            if (nrow(termSet[[i]]) == 0) { 
              termSet[[i]] <- t(combn(termSet[[i+1]][k, ], i))
            } else {
              termSet[[i]] <- rbind(termSet[[i]], t(combn(termSet[[i+1]][k, ], i))) 
            }
          }
        }
        
      } 
           
      # The "current solution" term probabilities to pass along to the next iteration should be zero for anything
      # other than the terms that were ultimately used in the current particle
      resultingProbabilities[[i]] <- ifelse(apply(termOptions[[i]], 1, function(x){
        counter <- FALSE
        nTermsI <- nrow(termSet[[i]])
        if (nTermsI >= 1) {
          for (j in 1:nTermsI) {
            counter <- counter | (length(intersect(x, termSet[[i]][j, ])) == i)
          }
        }
        counter
      }), 1, 0)
      
    }
    
  }
  resultingProbabilities
}