########################################################################################################################################
# Linear Model Interaction Terms Optimizer (lmito)
# Example Driver Routine
#
# For discussion, please see 
# http://www.neilalex.com/discovering-variable-interactions-using-particle-swarm
#
# Please contact me directly with questions or comments
# Neil Alexander McQuarrie
# neil@neilalex.com
########################################################################################################################################


########################################################################################################################################
# Instructions for Use
########################################################################################################################################
# First set the working directory uncommenting setwd as noted below
#
# If you would like to run the routine in parallel mode using Apache Spark 2.0 
# or higher, also uncomment the Spark section below and update the parameters 
# to match your installation
#
# To run in serial mode without Spark (with a smaller number of particles and
# iterations), simply source this file
#
# For a nice guide on setting up SparkR with Spark 2.0 on Amazon Web Services, 
# see https://edgarsdatalab.com/2016/08/25/setup-a-spark-2-0-cluster-r-on-aws/
#
########################################################################################################################################


########################################################################################################################################
# Uncomment and set working directory here
########################################################################################################################################
# setwd("~/lmito")

# This example uses the the randomForest package's roughfix imputation method
if (!require(randomForest)) install.packages("randomForest")
source("R/lmito.R")
enableSpark <- FALSE


########################################################################################################################################
# Uncomment and set parameters here to run using Spark 2.0 or higher
########################################################################################################################################
# # Set up SparkR and Spark session
# enableSpark <- TRUE
# library(SparkR, lib.loc="/home/ubuntu/spark-2.0.0/R/lib")
# sparkMaster <- "spark://ip-172-31-14-87:7077"
# sparkHome <- "/home/ubuntu/spark-2.0.0"
# enableHiveSupport <- FALSE
# sparkConfigList <- list(spark.rpc.numRetries="10", spark.rpc.lookupTimeout="500s", spark.network.timeout="500s", 
#                         spark.rpc.askTimeout="500s", spark.rpc.message.maxSize="2047", spark.driver.maxResultSize="250gb",
#                         spark.rpc.message.maxSize="2047", spark.driver.maxResultSize="250gb", spark.driver.memory="100g",
#                         spark.executor.memory="58g")
# sparkR.session(master=sparkMaster, sparkHome=sparkHome, enableHiveSupport=enableHiveSupport, sparkConfig=sparkConfigList)

# Load example data
load("example_data.rda")
yName <- "logEarnings2012"

# Establish number of particles and number of iterations
if (enableSpark) {
  nParticles <- 344
  nIterations <- 500    
} else {
  nParticles <- 4
  nIterations <- 4  
}

# Run lmito
results <- lmito(exampleData, yName, randomForest::na.roughfix, environment(randomForest::na.roughfix), enableSpark=enableSpark, nParticles=nParticles, nIterations=nIterations)
if (enableSpark) sparkR.stop()
	
# Save the lmito output
if (!file.exists("output")) dir.create("output")
save(list="results", file="output/results.rda")

# Plot local best MSE versus iteration number for every particle
matplot(y=matrix(unlist(results$`Particle Local Best Cross-Validation Error History`),
                 ncol=nParticles, byrow=TRUE), x=1:nIterations, type=c("l"), ylim=c(1.5,2.5),
        xlab="Iteration Number", ylab="Local Best Cross-Validated MSE", font.lab=2, col.axis="#5d5d5d", font.axis=1)
box(lwd=3, fg="#ffffff")

# Print best discovered formula
bestDiscoveredFormula <- paste0(yName," ~ ", unlist(results$`Global Best Formula History`[[nIterations]]))
cat("\nBest discovered formula...\n\n")
cat(bestDiscoveredFormula)

# Save best discovered formula to a file
fileWriter <- file("output/best_discovered_formula.txt")
writeLines(bestDiscoveredFormula, fileWriter)
close(fileWriter)