########################################################################################################################################
# Function lmFormula
# Creates a linear regression formula from a list of terms
#
# Arguments:
#   - String representing the name of the Y outcome variable for the formula
#   - A list of terms to includce in the formula, with each index of the list
#     corresponding to the number of variables allowed to interact in the given terms
#   - An integer corresponding to the highest interaction term level
#
# Returns:
#   - A formula that can be passed to an R linear regression routine
#
########################################################################################################################################

lmFormula <- function(yName, termSet, highestTermLevel) {
  formula <- paste0(yName, " ~ ")
  hasTermsFlag <- 0
  if (highestTermLevel >= 1) {
  
  	# Loop across the various term levels
    for (i in highestTermLevel:1) { 
      nTermsI <- nrow(termSet[[i]])
      if (nTermsI > 0) {
      
      	# Loop across the various terms at the current level
        for (j in 1:nTermsI) { 
        
        	# Loop across the different variables included in current term
          for (k in 1:i) { 
            if (k > 1) {
              formula <- paste0(formula, ":", termSet[[i]][j, k])
            } else {
              if (hasTermsFlag == 0) {
                hasTermsFlag <- 1
                formula <- paste0(formula, termSet[[i]][j, k])
              } else {
                formula <- paste0(formula, " + ", termSet[[i]][j, k])
              }
            }
          }
          
        }
        
      }      
    }
    
  }
  formula <- as.formula(formula)
}