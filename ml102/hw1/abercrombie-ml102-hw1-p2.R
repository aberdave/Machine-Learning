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
  stand=FALSE,
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
# Step 3. Do diana on scaled dataset

# Use diss=FALSE since these observations
# now we standardize
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



# I have no idea how to interpret these charts

