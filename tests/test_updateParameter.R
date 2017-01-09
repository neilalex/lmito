########################################################################################################################################
# Test updateParameter (1)
# Test that updateParameter returns results within the expected range given local and global learning factors
########################################################################################################################################

test_that("Test updateParameter (1)", {
  
  # Test values
  current <- 1
  localBest <- 2
  localLearningFactor <- 0.3
  globalBest <- 3
  globalLearningFactor <- 0.7
  noiseRange <- 0.6
  
  # Expected values
  expectedMinimum <- current + noiseRange
  expectedMaximum <- current + localLearningFactor * (localBest - current) + globalLearningFactor * (globalBest - current) + noiseRange
 
 # Perform test repeatedly to ensure correct results over the course of the algorithm's learning
  for(i in 1:100) {
    expect_true(updateParameter(current, localBest, localLearningFactor, globalBest, globalLearningFactor, noiseRange) >= expectedMinimum)
    expect_true(updateParameter(current, localBest, localLearningFactor, globalBest, globalLearningFactor, noiseRange) <= expectedMaximum)
  }
 
})