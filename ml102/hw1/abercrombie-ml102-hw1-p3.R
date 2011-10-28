# Machine Learning 102 - Unsupervised - Hacker Dojo
# http://machinelearning102.pbworks.com/w/page/32890352/FrontPage
#
# Homework #1, Basic clustering
# Dave Abercrombie, October 24 2011
#
# http://machinelearning102.pbworks.com/w/file/37958115/ML102Homework02.pdf
# http://archive.ics.uci.edu/ml/datasets/Synthetic+Control+Chart+Time+Series
#
# Problem 3 of 4
#

#####################################################################
# Step 1. Import data

# see if stuff is lying around
ls()


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
# Step 2. Do agnes on raw dataset

require(cluster)


# do not standardize, yet (that comes later)
control.chart.raw.agnes <- agnes(
  x=control.chart.raw.df, 
  stand=FALSE
)

str(control.chart.raw.agnes)
# Interactive look at return value structure
#
summary(control.chart.raw.agnes)

plot(
  x=control.chart.raw.agnes,
  ask=FALSE,
  which.plots=2
)





#####################################################################
# Step 3. Do agnes on scaled dataset

# now we standardize
control.chart.stand.agnes <- agnes(
  x=control.chart.raw.df, 
  stand=TRUE
)

summary(control.chart.stand.agnes)

plot(
  x=control.chart.stand.agnes,
  ask=FALSE,
  which.plots=2
)



# I have no idea how to interpret these charts

