# lmito
> Linear Model Interaction Terms Optimizer

Applies particle swarm optimization to the set of interaction terms to include in an ordinary least squares regression (OLS). 

For a discussion and example results, please see [http://www.neilalex.com/discovering-variable-interactions-using-particle-swarm](http://www.neilalex.com/discovering-variable-interactions-using-particle-swarm).

### Running / Getting Started
The algorithm itself is in R/lmito.R. An example driver using data from the Panel Study of Income Dynamics (PSID) is in example.R. 

To run the core algorithm in serial mode using 4 particles and 4 iterations, just source example.R.

To run the algorithm on Apache Spark (with a larger / potentially more-robust number of particles and iterations), follow instructions inside example.R to enable the correct parameters. This mode will require Apache Spark 2.0 or higher -- good instructions for setting up a Spark 2.0 installation on Amazon Web Services (along with SparkR) are at
[https://edgarsdatalab.com/2016/08/25/setup-a-spark-2-0-cluster-r-on-aws/](https://edgarsdatalab.com/2016/08/25/setup-a-spark-2-0-cluster-r-on-aws/).

Please contact neil@neilalex.com with questions or comments.