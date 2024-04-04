# lmito
> Linear Model Interaction Terms Optimizer

Applies particle swarm optimization to the set of interaction terms to include in an ordinary least squares regression (OLS). 

For a discussion and example results, please see [https://neilalex.com/discovering-variable-interactions-using-particle-swarm-and-apache-spark/](https://neilalex.com/discovering-variable-interactions-using-particle-swarm-and-apache-spark/).

### Running / Getting Started
The algorithm itself is in R/lmito.R. An example driver using data from the Panel Study of Income Dynamics (PSID) is in example.R. 

To run the core algorithm in serial mode using 4 particles and 4 iterations, just source example.R.

To run the algorithm on Apache Spark (with a larger / potentially more-robust number of particles and iterations), follow instructions inside example.R to enable the correct parameters. This mode will require Apache Spark 2.0 or higher. [Here](https://www.r-bloggers.com/2015/11/launch-apache-spark-on-aws-ec2-and-initialize-sparkr-using-rstudio-2/) and [here](https://www.youtube.com/watch?v=ISsnKm2mAx4) are some instructions for setting up a SparkR on Amazon Web Services.

Please contact neil@neilalex.com with questions or comments.
