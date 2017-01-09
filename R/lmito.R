########################################################################################################################################
# Function lmito
# Performs particle swarm optimization on the set of variables and interaction terms to include in an OLS regression    
#
# Arguments:
#  - data: data.frame containing data from which to derive the regression model; all columns will serve as X variables
#      with the exception of the outcome column specified in yName
#  - yName: string name of the outcome Y variable column in data
#  - imputationFunction: must take a data.frame, with potentially missing values, as its first argument and return 
#      a fully-imputed data.frame. (Because lmito generates test samples when fitting parameters, it may be important not to
#      impute missing data in advance, in the event the imputation creates a correlation between the training and test 
#      samples)
#  - enableSpark: whether to distribute processing on an Apache Spark 2.0 (or higher) instance
#      Note: if TRUE, a Spark 2.0 sparkR.session will already need to have been opened before calling lmito
#  - nParticles: the number of particles to include in the swarm (in parallel if using Spark)
#  - nIterations: the number of optimization iterations to execute across the swarm 
#  - maxHighestTermLevel: the greatest number of varibles to include in a single regression interaction (e.g. A:B:C has 3 terms)
#  - maxNTermsByLevel: the maximum number of terms to allow at each term level; 
#      set this to save computation if the swarm need not explore the full term space
#  - localLearningFactor, globalLearningFactor: rates at which each particle should move toward the local and global
#      best solutions across iterations
#  - nPredictorNoiseRange, highestTermNoiseRange, termsByLevelNoiseRange, 
#      predictorProbabilityNoiseRange, termProbabilityNoiseRange: factors controlling the degree to which each particle
#      should randomize its parameters every iteramtion
#
# Returns:
#    The regression formula specifying the best discovered set of variables and interactions, plus the history
#    of these variables and interactions over the course of lmito processing 
#
########################################################################################################################################   


source("R/zeroOutNonPredictorTermProbabilities.R")
source("R/zeroOutProbabilitiesFromHigherSubterms.R")
source("R/lmFormula.R")
source("R/lmFitAndCrossValidate.R")
source("R/probabilitiesForNextIteration.R")
source("R/setProbabilitiesAndTerms.R")
source("R/updateParameter.R")
source("R/bestTermProbabilities.R")
source("R/bestNTerms.R")

lmito <- function(data, yName, imputationFunction, imputationFunctionEnvironment, enableSpark = FALSE, nParticles = 4, 
                  nIterations = 20, maxHighestTermLevel = 3, maxNTermsByLevel = c(NA, 20, 20), localLearningFactor = 0.5, 
                  globalLearningFactor = 0.2, nPredictorNoiseRange = 2, highestTermNoiseRange = 0.6, termsByLevelNoiseRange = 0.4,
                  predictorProbabilityNoiseRange = 0.1, termProbabilityNoiseRange = 0.1) {  
                  
  # Constants to help with random number generation 
  kAlmostOne <- 0.999999999
  kAlmostHalf <- 0.499999999
  kNPartitions <- 10
  
  
  ########################################################################################################################################    
  # Function RunModel  
  # Lapply (below) calls this to build the regression model for each particle
  ########################################################################################################################################    
  
  RunModel <- function(particle) {
    print(paste0("   Particle ", particle))
    # As described below, because of an apparent limitation in Spark 2.0 (as of this commit),
    # we are not able to use a Spark dataframe for storing the particle's parameters.
    # Instead we store the parameters across a set of R global data.frames.
    # To avoid update anomalies if distributing processing across multiple Spark workers, we create 
    # local copies here so that workers can operate on them individually
    # TODO: convert to Spark data frame once it supports nesting a list inside a matrix/vector cell
    ciPredictorIncludeProbabilities <- unlist(ciPredictorIncludeProbabilitiesMaster[particle, ]$data)
    ciNPredictors <- ciNPredictorsMaster[particle, ]$data
    ciPredictorVars <- vector(length=ciNPredictors)
    ciHighestTermLevel <- ciHighestTermLevelMaster[particle, ]$data
    ciHighestTermLevelInt <- 0
    ciNTermsAtLevel <- unlist(ciNTermsAtLevelMaster[particle, ]$data)
    ciNTermsAtLevelInt <- vector(length=maxHighestTermLevel)
    ciTermProbabilities <- ciTermProbabilitiesMaster[particle, ]$data[[1]]
    ciTerms <- list() #Can't allocate space for this in advance because its size will vary
    ciSE <- ciSEMaster[particle, ]$data
    lBestPredictorIncludeProbabilities <- unlist(lBestPredictorIncludeProbabilitiesMaster[particle, ]$data)
    lBestNPredictors <- lBestNPredictorsMaster[particle, ]$data
    lBestHighestTermLevel <- lBestHighestTermLevelMaster[particle, ]$data
    lBestNTermsAtLevel <- unlist(lBestNTermsAtLevelMaster[particle, ]$data)
    lBestTermProbabilities <- lBestTermProbabilitiesMaster[particle, ]$data[[1]]
    lBestSE <- lBestSEMaster[particle, ]$data
    lBestIteration <- lBestBestIterationMaster[particle, ]$data
    gBestPredictorIncludeProbabilities <- unlist(gBestPredictorIncludeProbabilitiesMaster)
    gBestNPredictors <- gBestNPredictorsMaster
    gBestHighestTermLevel <- gBestHighestTermLevelMaster
    gBestNTermsAtLevel <- unlist(gBestNTermsAtLevelMaster)
    gBestTermProbabilities <- gBestTermProbabilitiesMaster[[1]]
    gBestSE <- gBestSEMaster
    gBestIteration <- gBestBestIterationMaster
    
    
    ########################################################################################################################################    
    # During iteration 1, establish initial parameters for the particle
    ########################################################################################################################################
    
    if (currentIteration == 1) {
    
      # A unique seed for each particle
      set.seed(particle*20) 
      
      # Overall variable set from which to choose predictors
      ciPredictorIncludeProbabilities <- rep(1, totalNPredictors)
      ciNPredictors <- runif(n = 1, min = 1 - kAlmostHalf, max = totalNPredictors + kAlmostHalf)
      ciPredictorVars <- base::sample(allPredictorVars, size = as.integer(round(ciNPredictors)), prob = ciPredictorIncludeProbabilities, replace = FALSE)
      
      # Highest term level
      ciHighestTermLevel <- runif(n = 1, min  = 1 - kAlmostHalf, max = maxHighestTermLevel + kAlmostHalf)
      ciHighestTermLevelInt <- as.integer(round(ciHighestTermLevel))
      for (i in maxHighestTermLevel:1) {
      
        # Number of terms at each term level
        # At term level 1 (independent / non-interaction terms), always include the full number of predictors determined above        
        if (i == 1) ciNTermsAtLevel[[i]] <- ciNPredictors
        
        # Randomly choose a number of terms for the interaction term levels
        # Make sure this number doesn't exceed the total possible number of terms given  number of vars available (i.e. choose(n,k))
        else ciNTermsAtLevel[[i]] <- min(runif(1, min = -kAlmostHalf, maxNTermsByLevel[i] + kAlmostHalf), choose(n = ciNPredictors, k = i))
        
        # The particle's parameters are stored as continuous variables as the optimization assumes smooth gradients
        # However, we create integer versions in places needed
        ciNTermsAtLevelInt[[i]] <- as.integer(round(ciNTermsAtLevel[[i]]))
        
        # Term Probabilities: start with equal probability for every term
        ciTermProbabilities[[i]] <- rep(1, choose(n = totalNPredictors, k = i))
      }
      
      
    ########################################################################################################################################
    # During iterations 2 and beyond, update the particle with the best discovered local and global parameters
    ########################################################################################################################################
    
    } else {
    
      # Overall variable set from which to choose predictors
      # Ensure variable is within bounds, and then choose the predictors using sample(...)
      ciPredictorIncludeProbabilities <- updateParameter(ciPredictorIncludeProbabilities, lBestPredictorIncludeProbabilities, localLearningFactor, gBestPredictorIncludeProbabilities, globalLearningFactor, predictorProbabilityNoiseRange)
      ciNPredictors <- updateParameter(ciNPredictors, lBestNPredictors, localLearningFactor, gBestNPredictors, globalLearningFactor, nPredictorNoiseRange)
      ciNPredictors <- max(1-kAlmostHalf, min(totalNPredictors + kAlmostHalf, ciNPredictors))  
      ciPredictorVars <- base::sample(allPredictorVars, size = as.integer(round(ciNPredictors)), prob = ciPredictorIncludeProbabilities, replace=FALSE)
      
      # Highest term level
      ciHighestTermLevel <- updateParameter(ciHighestTermLevel, lBestHighestTermLevel, localLearningFactor, gBestHighestTermLevel, globalLearningFactor, highestTermNoiseRange)
      ciHighestTermLevel <- max(1 - kAlmostHalf, min(maxHighestTermLevel + kAlmostHalf, ciHighestTermLevel))
      ciHighestTermLevelInt <- as.integer(round(ciHighestTermLevel))
      
      # Number of terms at each term level
      for (i in ciHighestTermLevelInt:1) {
        if (i == 1) {
        
          # At term level 1 (i.e. for independent / non-interaction terms), always include the full number of predictors determined above
          ciNTermsAtLevel[i] <- ciNPredictors
        } else {
        
          # Randomly choose a number of terms for each interaction term level
          ciNTermsAtLevel[i] <- updateParameter(ciNTermsAtLevel[i], lBestNTermsAtLevel[i], localLearningFactor, gBestNTermsAtLevel[i], globalLearningFactor, termsByLevelNoiseRange)
          
          #Ensure the number of terms is within bounds despite adding noise
          ciNTermsAtLevel[i] <- max(-kAlmostHalf, min(choose(n=ciNPredictors, k=i) + kAlmostHalf, ciNTermsAtLevel[i] ))
        }
        ciNTermsAtLevelInt[i] <- as.integer(round(ciNTermsAtLevel[i]))
        
        # Probabilities for terms at each term level
        ciTermProbabilities[[i]] <- updateParameter(ciTermProbabilities[[i]], lBestTermProbabilities[[i]], localLearningFactor, gBestTermProbabilities[[i]], globalLearningFactor, termProbabilityNoiseRange)
      }
    }
    
    
    ########################################################################################################################################
    # Build the particle's regression model
    ########################################################################################################################################
        
    # Pick terms for the current regression
    finalProbabilitiesAndTerms <- setProbabilitiesAndTerms(ciTermProbabilities, termOptions, ciPredictorVars, ciHighestTermLevelInt, ciNTermsAtLevelInt)
    
    # Prepare a linear regression formula incorporating all of the selected terms
    formula <- lmFormula(yName, finalProbabilitiesAndTerms[[2]], ciHighestTermLevelInt)
    
    # Fit and ten-fold cross-validate a linear model
    seCV <- lmFitAndCrossValidate(data, kNPartitions, partitions, yName, formula, imputationFunction, imputationFunctionEnvironment)
   
    # Gather together all the results for the particle 
    results <- list(NA)
    results$ciSE <- seCV
    results$ciPredictorIncludeProbabilities <- ifelse(allPredictorVars %in% ciPredictorVars, 1, 0)
    results$ciNPredictors <- ciNPredictors
    results$ciHighestTermLevel <- ciHighestTermLevel
    results$ciNTermsAtLevel <- ciNTermsAtLevel
    results$ciTermProbabilities <- probabilitiesForNextIteration(finalProbabilitiesAndTerms[[1]], finalProbabilitiesAndTerms[[2]], termOptions, ciHighestTermLevelInt, maxHighestTermLevel)   
    
    # Update the best known local solution if a new one is discovered
    if (seCV < lBestSE) {
      results$lBestFormula <- as.character(formula)[3]
      results$lBestSE <- seCV
      results$lBestIteration <- currentIteration
      results$lBestPredictorIncludeProbabilities <- results$ciPredictorIncludeProbabilities
      results$lBestNPredictors <- ciNPredictors
      results$lBestHighestTermLevel <- ciHighestTermLevel
      results$lBestNTermsAtLevel <- bestNTerms(ciNTermsAtLevel, ciHighestTermLevelInt, maxHighestTermLevel)
      results$lBestTermProbabilities <- bestTermProbabilities(results$ciTermProbabilities, termOptions, ciHighestTermLevelInt, maxHighestTermLevel)
      
    # Maintain the existing local best solution if a new one was not discovered this iteration
    } else {
      # In an effort to limit data transfer between Spark nodes, don't store the formula if
      # a new local best solution is not discovered
      results$lBestFormula <- NA
      results$lBestSE <- lBestSE
      results$lBestIteration <- lBestIteration
      results$lBestPredictorIncludeProbabilities <- lBestPredictorIncludeProbabilities
      results$lBestNPredictors <- lBestNPredictors
      results$lBestHighestTermLevel <- lBestHighestTermLevel
      results$lBestNTermsAtLevel <- lBestNTermsAtLevel
      results$lBestTermProbabilities <- lBestTermProbabilities
    }
    
    # Return results
    results
  }
  
  
  ########################################################################################################################################
  # lmito function execution begins here
  ########################################################################################################################################    
  
  # Split the data into cross-validation partitions; this is done outside of the particle swarm algorithm 
  # to ensure that all particles across all iterations use a consistent set of partitions
  partitions <- base::sample(x=1:kNPartitions, size=nrow(data), replace=TRUE)
  
  # Create a data structure for storing solutions from the current iteration
  outcomes <- list()
  
  # Establish an allowed set of variables and terms that each particle's model can choose from
  allPredictorVars <- names(data[, !names(data) %in% yName])
  totalNPredictors <- length(allPredictorVars)
  termOptions <- vector(mode="list", length=maxHighestTermLevel)
  for (i in maxHighestTermLevel:1) termOptions[[i]] <- t(combn(allPredictorVars, i)) 
  
  # Prepare a set of particles to give to lapply (which may be run on Spark depending on current mode)
  seeds <- 1:nParticles 
  currentIteration <- 1
  
  
  ########################################################################################################################################
  # Perform the particle swarm optimization
  ########################################################################################################################################    
    
  # The following allocates space for the various parameters that the particle swarm will attempt to optimize over. 
  # Note that ideally we'd use a single Spark dataframe for containing all of the relevant information, 
  # however, as of this version, processing a Spark 2.0 dataframe generates an error if the data frame contains 
  # nested lists, which are important for lmito. As a next-best alternative, we also cannot use a single 
  # R data frame, because, depending on the number of variables and term levels we must optimize over, it could 
  # exceed the R address-space limit. Instead we store parameters across a -set- of R data.frames.
  ciPredictorIncludeProbabilitiesMaster <- data.frame(seeds)
  ciNPredictorsMaster <- data.frame(seeds)
  ciHighestTermLevelMaster <- data.frame(seeds)
  ciNTermsAtLevelMaster <- data.frame(seeds)
  ciTermProbabilitiesMaster <- data.frame(seeds)
  ciSEMaster <- data.frame(seeds)
  ciPredictorIncludeProbabilitiesMaster$data <- list(rep(0, totalNPredictors))
  ciNPredictorsMaster$data <- 0
  ciHighestTermLevelMaster$data <- 0
  ciNTermsAtLevelMaster$data <- list(rep(NA, maxHighestTermLevel))
  ciTermProbabilitiesMaster$data <- list(vector(mode="list", length=maxHighestTermLevel))
  ciSEMaster$data <- Inf
  lBestPredictorIncludeProbabilitiesMaster <- data.frame(seeds)
  lBestNPredictorsMaster <- data.frame(seeds)
  lBestHighestTermLevelMaster <- data.frame(seeds)
  lBestNTermsAtLevelMaster <- data.frame(seeds)
  lBestTermProbabilitiesMaster <- data.frame(seeds)
  lBestFormulaMaster <- data.frame(seeds)
  lBestSEMaster <- data.frame(seeds)
  lBestBestIterationMaster <- data.frame(seeds)
  lBestPredictorIncludeProbabilitiesMaster$data <- list(rep(0, totalNPredictors))
  lBestNPredictorsMaster$data  <- 0
  lBestHighestTermLevelMaster$data  <- 0
  lBestNTermsAtLevelMaster$data  <- list(rep(NA, maxHighestTermLevel))
  lBestTermProbabilitiesMaster$data <- list(vector(mode="list", length=maxHighestTermLevel))
  lBestFormulaMaster$data  <- list("")
  lBestSEMaster$data <- Inf
  lBestBestIterationMaster$data <- 0
  gBestPredictorIncludeProbabilitiesMaster <- lBestPredictorIncludeProbabilitiesMaster[1, ]$data
  gBestNPredictorsMaster <- lBestNPredictorsMaster[1, ]$data
  gBestHighestTermLevelMaster  <- lBestHighestTermLevelMaster[1, ]$data
  gBestNTermsAtLevelMaster  <- lBestNTermsAtLevelMaster[1, ]$data
  gBestTermProbabilitiesMaster <- lBestTermProbabilitiesMaster[1, ]$data
  gBestFormulaMaster  <- lBestFormulaMaster[1, ]$data
  gBestSEMaster <- lBestSEMaster[1, ]$data
  gBestBestIterationMaster <- lBestBestIterationMaster[1, ]$data
  
  # Variables for storing the history of the particles' parameters
  ciNPredictorsHistory <- vector(mode="list", length=nIterations)
  ciHighestTermLevelHistory <- vector(mode="list", length=nIterations)
  ciNTermsAtLevelHistory <- vector(mode="list", length=nIterations)
  ciSEHistory <- vector(mode="list", length=nIterations)
  lBestNPredictorsHistory <- vector(mode="list", length=nIterations)
  lBestHighestTermLevelHistory <- vector(mode="list", length=nIterations)
  lBestNTermsAtLevelHistory <- vector(mode="list", length=nIterations)
  lBestSEHistory <- vector(mode="list", length=nIterations)
  gBestNPredictorsHistory <- vector(mode="list", length=nIterations)
  gBestHighestTermLevelHistory <- vector(mode="list", length=nIterations)
  gBestNTermsAtLevelHistory <- vector(mode="list", length=nIterations)
  gBestFormulaHistory <- vector(mode="list", length=nIterations)
  gBestSEHistory <- vector(mode="list", length=nIterations)
  
  # Particle swarm iteration loop
  for (iteration in 1:nIterations) {
    currentIteration <- iteration
    print(paste0("Starting iteration ", currentIteration, "..."))
    
    # Run lapply on the particles
    # Use Spark if enabled
    if (enableSpark) {
      tryCatch({
        outcomes <- spark.lapply(1:nParticles, RunModel)
      }, warning = function(w) {
      
      # Occasionally we'll temporarily lose a Spark worker; if this occurs, skip the iteration and move on
      }, error = function(e) {
        next  
      }, finally = {})
      
    } else {
      outcomes <- lapply(1:nParticles, RunModel)  
    }
    print("Compiling results from particles...")
    
    # Extract results from each particle and save to the global variables set up above
    for (i in 1:nParticles) {
      lBestPredictorIncludeProbabilitiesMaster[i, ]$data <- list(outcomes[[i]]$lBestPredictorIncludeProbabilities)
      lBestNPredictorsMaster[i, ]$data <- outcomes[[i]]$lBestNPredictors
      lBestHighestTermLevelMaster[i, ]$data <- outcomes[[i]]$lBestHighestTermLevel
      lBestNTermsAtLevelMaster[i, ]$data <- list(outcomes[[i]]$lBestNTermsAtLevel)
      lBestTermProbabilitiesMaster[i, ]$data <- list(outcomes[[i]]$lBestTermProbabilities)
      lBestFormulaMaster[i, ]$data <- list(outcomes[[i]]$lBestFormula)
      lBestSEMaster[i, ]$data <- outcomes[[i]]$lBestSE
      lBestBestIterationMaster[i, ]$data <- outcomes[[i]]$lBestIteration
      ciPredictorIncludeProbabilitiesMaster[i, ]$data <- list(outcomes[[i]]$ciPredictorIncludeProbabilities)
      ciNPredictorsMaster[i, ]$data <- outcomes[[i]]$ciNPredictors
      ciHighestTermLevelMaster[i, ]$data <- outcomes[[i]]$ciHighestTermLevel
      ciNTermsAtLevelMaster[i, ]$data <- list(outcomes[[i]]$ciNTermsAtLevel)
      ciTermProbabilitiesMaster[i, ]$data <- list(outcomes[[i]]$ciTermProbabilities)
      ciSEMaster[i, ]$data <- outcomes[[i]]$ciSE
    }
    
    # Find the best solution from across all particles during the most recent iteration
    currentGbestSE <- min(lBestSEMaster$data)
    if (currentGbestSE < gBestSEMaster) {
      bestParticle <- which(lBestSEMaster$data == currentGbestSE)
      gBestPredictorIncludeProbabilitiesMaster <- lBestPredictorIncludeProbabilitiesMaster[bestParticle, ]$data
      gBestNPredictorsMaster  <- lBestNPredictorsMaster[bestParticle, ]$data
      gBestHighestTermLevelMaster  <- lBestHighestTermLevelMaster[bestParticle, ]$data
      gBestNTermsAtLevelMaster  <- lBestNTermsAtLevelMaster[bestParticle, ]$data
      gBestTermProbabilitiesMaster <- lBestTermProbabilitiesMaster[bestParticle, ]$data
      gBestFormulaMaster  <- lBestFormulaMaster[bestParticle, ]$data
      gBestSEMaster <- lBestSEMaster[bestParticle, ]$data
      gBestBestIterationMaster <- lBestBestIterationMaster[bestParticle, ]$data
    }
    
    # Store the history of particle parameters and outputs across iterations
    ciNPredictorsHistory[[iteration]] <- ciNPredictorsMaster$data
    ciHighestTermLevelHistory[[iteration]] <- ciHighestTermLevelMaster$data
    ciNTermsAtLevelHistory[[iteration]] <- ciNTermsAtLevelMaster$data
    ciSEHistory[[iteration]] <- ciSEMaster$data
    lBestNPredictorsHistory[[iteration]] <- lBestNPredictorsMaster$data
    lBestHighestTermLevelHistory[[iteration]] <- lBestHighestTermLevelMaster$data
    lBestNTermsAtLevelHistory[[iteration]] <- lBestNTermsAtLevelMaster$data
    lBestSEHistory[[iteration]] <- lBestSEMaster$data
    gBestNPredictorsHistory[[iteration]] <- gBestNPredictorsMaster
    gBestHighestTermLevelHistory[[iteration]] <- gBestHighestTermLevelMaster
    gBestNTermsAtLevelHistory[[iteration]] <- gBestNTermsAtLevelMaster
    gBestFormulaHistory[[iteration]] <- gBestFormulaMaster
    gBestSEHistory[[iteration]] <- gBestSEMaster
    
  }
  
  # Return results
  c("Particle Number of Predcitors History" = list(ciNPredictorsHistory), 
    "Particle Highest Term Level History" = list(ciHighestTermLevelHistory), 
    "Particle Number of Terms by Level History" = list(ciNTermsAtLevelHistory), 
    "Particle Cross-Validation Error History" = list(ciSEHistory),
    "Particle Local Best Number of Predictors History" = list(lBestNPredictorsHistory), 
    "Particle Local Best Highest Term Level History" = list(lBestHighestTermLevelHistory), 
    "Particle Local Best Number of Terms by Level History" = list(lBestNTermsAtLevelHistory), 
    "Particle Local Best Cross-Validation Error History" = list(lBestSEHistory),
    "Global Best Number of Predictors History" = list(gBestNPredictorsHistory), 
    "Global Best Highest Term Level History" = list(gBestHighestTermLevelHistory), 
    "Global Best Number of Terms by Level History" = list(gBestNTermsAtLevelHistory), 
    "Global Best Cross-Validation Error History"= list(gBestSEHistory),
    "Global Best Formula History" = list(gBestFormulaHistory)
  )
}