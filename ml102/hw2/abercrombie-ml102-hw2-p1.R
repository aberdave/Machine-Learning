# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #2, Basic clustering 2
# Dave Abercrombie, Noveember 1 2011
#
# http://machinelearning102.pbworks.com/w/file/47470778/Homework2.txt
#
# Problem 1 of 1
#

#####################################################################
# Step 1. Generate synthetic dataset

# We want 25 cluster centers in a five-by-five grid with both X and Y
# taking on the values 1 through 5. Around each center, we will generate
# Gaussian distribution of random points, with a variety of standard deviations

ls()
setwd("/Users/dabercrombie/Documents/aberdave-repos/Machine-Learning/ml102/hw2")

# Step 1a - generate centers
# This handy function does what we in the database
# field might call a Cartesian product, i.e., all 
# combinations of the supplied vectors.
#
cluster.centers.actual.df <- expand.grid(
  1:5,
  1:5
)
# Interactive peek at these centers
#
plot(cluster.centers.actual.df, main="Actual centers")


# Step 1b Create a function that generates random points
# around a center cluster. 
#
point.generator.f <- function(
  center.vector,
  standard.deviation,
  point.count
) {
  cbind(
    rnorm(point.count, sd=standard.deviation) + center.vector[,1],
    rnorm(point.count, sd=standard.deviation) + center.vector[,2]
  )
}

# sanity check function using one center
cluster.centers.actual.df[22,]
point.generator.f(
  center.vector=cluster.centers.actual.df[22,], 
  standard.deviation=0.2, 
  point.count=10
)


# Loop through set of cluster centers, appending
# each center's points to a data frame with rbind()
# (must first initialize data frame to be empty)
synthetic.points.df <- data.frame()
for( i in 1:nrow(cluster.centers.actual.df) ) {
  synthetic.points.df <- rbind(
    synthetic.points.df,
    point.generator.f(
      center.vector=cluster.centers.actual.df[i,],   # the i-th row
      standard.deviation=0.2,                        # will vary later
      point.count=100
    )
  )
}

# Sanity check generated points
#
dim(synthetic.points.df)
plot(synthetic.points.df, main="Standard deviation = 0.2")


