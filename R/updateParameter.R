########################################################################################################################################
# Function updateParameter
# Updates a particle swarm parameter accounting for updates to the current local and global best known values for this parameter
#
# Arguments:
#   - The current value for the parameter
#   - The best known local value for the parameter from across all completed iterations so far
#   - The best known global value for the parameter from across all particles across all iterations so far
#   - Learning factors for incorportating knwoledge about the best local and global parameters into the current parameter value
#   - An allowable range of noise to apply to the parameter
#
# Returns:
#   - An updated parameter
#
########################################################################################################################################

updateParameter <- function(current, localBest, localLearningFactor, globalBest, globalLearningFactor, noiseRange) {
  updated <- current + 
    runif(n=1, min=0, max=1) * localLearningFactor * (localBest - current) +
    runif(n=1, min=0, max=1) * globalLearningFactor * (globalBest - current) +
    noiseRange
}