# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #1, Basic clustering
# Dave Abercrombie, October 24 2011
#
# http://machinelearning102.pbworks.com/w/file/37958115/ML102Homework02.pdf
# http://archive.ics.uci.edu/ml/datasets/Synthetic+Control+Chart+Time+Series
#
# Problem 2 of 4
#

#####################################################################
# Step 1. Import data

setwd("/Users/dabercrombie/Documents/aberdave-repos/Machine-Learning/ml102/hw1")

control.chart.raw.df <- read.table(
  file="synthetic_control.data",
  header=FALSE,
)

# Interactive look at structure
str(control.chart.raw.df)
# tail(control.chart.raw.df)
dim(control.chart.raw.df)


#####################################################################
# Step 2. Do diana on raw dataset

require(cluster)


control.chart.raw.km <- kmeans(
  x=control.chart.raw.df,
  centers=6,
  iter.max=20,
  nstart=10
)

# Use diss=FALSE since these observations
# do not standardize, yet (that comes later)
control.chart.raw.diana <- diana(
  x=control.chart.raw.df, 
  diss=FALSE,
  metric = "euclidean"
)

str(control.chart.raw.diana)
# Interactive look at return value structure
#
summary(control.chart.raw.diana)

plot(
  x=control.chart.raw.diana,
  ask=FALSE,
  which.plots=2
)





#####################################################################
# Step 3. Exract and prepare cluster assignments for review

# Intercative look at structure of cluster element
# It is a vector of 600 integers
#
str(control.chart.raw.km$cluster)

# We know that the data had six types of control charts, with 100 examples 
# of each, in contiguous chunks of rows
# See http://archive.ics.uci.edu/ml/databases/synthetic_control/synthetic_control.data.html
# So lets create some factors and attach them to cluster assingment as a data frame
#
control.chart.type.factors <- as.factor(
  c(
    rep('1. Normal',100),
    rep('2. Cyclicl',100),
    rep('3. Increasing trend',100),
    rep('4. Decreasing trend',100),
    rep('5. Upward shift',100),
    rep('6. Downward shift',100)
  )
)
str(control.chart.type.factors)

control.chart.raw.clusters.df <- data.frame(
  actual.type=control.chart.type.factors,
  modeled.cluster=control.chart.raw.km$cluster
)
str(control.chart.raw.clusters.df)


#####################################################################
# Step 4. Use a lattice of histograms to review

# Lattice graphics includes methods to put many histograms on 
# the same chart. It is installed by default, but not loaded
# by default.
#
require('lattice')


# The first argument is a "model formula" 
# in "Wilkinon-Rogers (1973)" notation (see Venables-Ripley chap. 6)
# Think of it as saying that the modeled.cluster values depend on the
# control.chart.raw.clusters.
# The third argument. layout, and is poorly documented. I learned this
# trick from "R in a Nutshell" Figure 15-12.
#
histogram(
  ~modeled.cluster|actual.type, 
  data=control.chart.raw.clusters.df,
  layout=c(1,6)
)

# Reviewing the chart, I am amazed at how well the clustering did work.
# The normal charts are all in a cluste of their own. The clyclic charts 
# are off by themselves, but split into two sub groups. Cyclics are clearly
# distinguished from decreasing trend, but confused with upward shift.
# Interesting, far from arbitrary, but not compelling.
#

#####################################################################
# Step 5. Since data are from time series, see if deltas work better

# 5A: convert time series data frame to matrix for simpler 
#     math and indexing notation 
#
control.chart.raw.m <- as.matrix(control.chart.raw.df)

# Create a data frame that has the deltas betwen one observation
# and the next:
#
# 5B. Initialize the first column of data frame by subtracting the 
# the matrix first column from it's second. I use drop=FALSE to
# preserve the column-like nature of the subtraction.
#
# Inspired by these recepies from the R Cookbook:
#    5.17 Selecting One Row or Column from a Matrix
#    5.29 Combining Two Data Frames
#
control.chart.deltas.df <- data.frame(
  control.chart.raw.m[,2,drop=FALSE] - control.chart.raw.m[,1,drop=FALSE]
)


# 5C. Iterate through the remaining matrix columns,
# appending fresh delta column to existing data frame
#
for ( i in 3:ncol(control.chart.raw.m) ) {
  # calculate the next column of deltas
  # by subtracting column 1-2 from column i
  new.column <- data.frame(
    control.chart.raw.m[,i,drop=FALSE] - control.chart.raw.m[,i-1,drop=FALSE]
  )
  # combine the new column with the existing
  # data frame, column-wise
  control.chart.deltas.df <-cbind(
    control.chart.deltas.df,
    new.column
  )
}
# expecting 600 59
dim(control.chart.deltas.df)

#####################################################################
# Step 6. Do k-means on delta dataset

control.chart.deltas.km <- kmeans(
  x=control.chart.deltas.df,
  centers=6,
  iter.max=20,
  nstart=10
)

# Interactive look at return value structure
#
summary(control.chart.deltas.km)

#####################################################################
# Step 7. Use a lattice of histograms to review delta clusters


control.chart.deltas.clusters.df <- data.frame(
  actual.type=control.chart.type.factors,
  modeled.cluster=control.chart.deltas.km$cluster
)
str(control.chart.deltas.clusters.df)


# Lattice graphics includes methods to put many histograms on 
# the same chart. It is installed by default, but not loaded
# by default.
#
require('lattice')


# The first argument is a "model formula" 
# in "Wilkinon-Rogers (1973)" notation (see Venables-Ripley chap. 6)
# Think of it as saying that the modeled.cluster values depend on the
# control.chart.raw.clusters.
# The third argument. layout, and is poorly documented. I learned this
# trick from "R in a Nutshell" Figure 15-12.
#
histogram(
  ~modeled.cluster|actual.type, 
  data=control.chart.deltas.clusters.df,
  layout=c(1,6)
)

# This did a horrible job!!!! Really bad!!!


