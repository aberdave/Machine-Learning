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
# summary(control.chart.raw.agnes)

plot(
  x=control.chart.raw.agnes,
  ask=FALSE,
  which.plots=2
)


#####################################################################
# Step 4. Extract and review agnes clusters of unscaled dataset
#
# see also Cookbook 13.6 Finding Clusters in Your Data
# a) cast to hclust with as.hclust()
# b) extract clusters with cutree()
# c) create factors for use with lattice histogram()

# 4a) convert agnes object to an hclust object
#
control.chart.raw.agnes.hc <- as.hclust(control.chart.raw.agnes)

# 4) We know from the dataset description that we hope for six clusters,
# so I use k=6 here.
#
control.chart.raw.agnes.cuts <- cutree(
  tree=control.chart.raw.agnes.hc,
  k=6
)

# cutree() returns a vector of cluster assignments, index by obsersvation
#
str(control.chart.raw.agnes.cuts)

 
# 4C) create factors for use with lattice histogram()
# We know that the data had six types of control charts, with 100 examples 
# of each, in contiguous chunks of rows
# See http://archive.ics.uci.edu/ml/databases/synthetic_control/synthetic_control.data.html
# So lets create some factors and attach them to cluster assingment as a data frame
#
control.chart.type.factors <- as.factor(
  c(
    rep('1. Normal',100),
    rep('2. Cyclic',100),
    rep('3. Increasing trend',100),
    rep('4. Decreasing trend',100),
    rep('5. Upward shift',100),
    rep('6. Downward shift',100)
  )
)
str(control.chart.type.factors)

control.chart.raw.agnes.clusters.df <- data.frame(
  actual.type=control.chart.type.factors,
  modeled.cluster=control.chart.raw.agnes.cuts
)
str(control.chart.raw.agnes.clusters.df)


# 4d) do lattice histogram. 
# 
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
  main="agnes clusters on raw data",
  ~modeled.cluster|actual.type, 
  data=control.chart.raw.agnes.clusters.df,
  layout=c(1,6)
)

# Use student Jason's simple visualization, made possible since input dataset
# was organized in groups of 100.
plot(
  control.chart.raw.agnes.cuts,
  main="agnes clusters on raw data",
  xlab="Obesrvation index: 1-100=normal, 101-200=cyclic, etc.",
  ylab="modeled cluster assigmment"
)








#####################################################################
# Step 5. Do agnes on scaled dataset using same steps

# now we standardize
control.chart.stand.agnes <- agnes(
  x=control.chart.raw.df, 
  stand=TRUE
)

# summary(control.chart.stand.agnes)

plot(
  x=control.chart.stand.agnes,
  ask=FALSE,
  which.plots=2
)

control.chart.stand.agnes.hc <- as.hclust(control.chart.stand.agnes)

# 4) We know from the dataset description that we hope for six clusters,
# so I use k=6 here.
#
control.chart.stand.agnes.cuts <- cutree(
  tree=control.chart.stand.agnes.hc,
  k=6
)

control.chart.stand.agnes.clusters.df <- data.frame(
  actual.type=control.chart.type.factors,
  modeled.cluster=control.chart.stand.agnes.cuts
)
str(control.chart.stand.agnes.clusters.df)


# 4d) do lattice histogram. 
# 
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
  main="agnes clusters on standardized data",
  ~modeled.cluster|actual.type, 
  data=control.chart.stand.agnes.clusters.df,
  layout=c(1,6)
)

# Use student Jason's simple visualization, made possible since input dataset
# was organized in groups of 100.
plot(
  control.chart.stand.agnes.cuts,
  main="agnes clusters on standardized data",
  xlab="Obesrvation index: 1-100=normal, 101-200=cyclic, etc.",
  ylab="modeled cluster assigmment"
)



