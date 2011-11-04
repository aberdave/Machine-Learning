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

# Step 1. Generate synthetic datasets
# Step 2. Sanity check use of kmeans()
# Step 3, Write a function to return SSE for a dataset and value of K
# Step 4. Write single-arg functions to return SSE for a value of K
#         One function for each sd dataframe: this allows the use
#         of sapply() to generate a plot of SSE function of only K
# Step 5. Plot SSE as function of K, look for inflection point
# Step 6. use AIC method to find optimal K

# Short answer: Both methods did a good job at finding a good K
# for the datasets that had real clusters (sd=0.1 or 0.2) but failed
# for the essentially unclustered data. 

#####################################################################
# Step 1. Generate synthetic datasets

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


# The problem calls for exploring three satnard
# deviations: sd = 0.1, sd = 0.2 and sd = 0.5.
# So I generate three synthetic datasets, one for
# each value of standard deviation. I copy code
# three times, should prably be another function!

# Loop through set of cluster centers, appending
# each center's points to a data frame with rbind()
# (must first initialize data frame to be empty)

# sd = 0.1
synthetic.points.0.1.sd.df <- data.frame()
for( i in 1:nrow(cluster.centers.actual.df) ) {
  synthetic.points.0.1.sd.df <- rbind(
    synthetic.points.0.1.sd.df,
    point.generator.f(
      center.vector=cluster.centers.actual.df[i,],   # the i-th row
      standard.deviation=0.1,                        # will vary later
      point.count=100
    )
  )
}
# sd = 0.2 
synthetic.points.0.2.sd.df <- data.frame()
for( i in 1:nrow(cluster.centers.actual.df) ) {
  synthetic.points.0.2.sd.df <- rbind(
    synthetic.points.0.2.sd.df,
    point.generator.f(
      center.vector=cluster.centers.actual.df[i,],   # the i-th row
      standard.deviation=0.2,                        # will vary later
      point.count=100
    )
  )
}
# sd = 0.5
synthetic.points.0.5.sd.df <- data.frame()
for( i in 1:nrow(cluster.centers.actual.df) ) {
  synthetic.points.0.5.sd.df <- rbind(
    synthetic.points.0.5.sd.df,
    point.generator.f(
      center.vector=cluster.centers.actual.df[i,],   # the i-th row
      standard.deviation=0.5,                        # varied from above
      point.count=100
    )
  )
}


# Sanity check generated points
#
dim(synthetic.points.0.1.sd.df)
dim(synthetic.points.0.2.sd.df)
dim(synthetic.points.0.5.sd.df)

plot(synthetic.points.0.1.sd.df, main="Standard deviation = 0.1")
plot(synthetic.points.0.2.sd.df, main="Standard deviation = 0.2")
plot(synthetic.points.0.5.sd.df, main="Standard deviation = 0.5")


#####################################################################
# Step 2. Sanity check use of kmeans()

synthetic.points.0.2.sd.km <- kmeans(
  x=synthetic.points.0.2.sd.df,
  centers=25,
  iter.max=20,
  nstart=10
)

# Interactive look at return value structure
#
summary(synthetic.points.0.2.sd.km)
str(synthetic.points.0.2.sd.km)
plot(synthetic.points.0.2.sd.km$centers, main="synthetic.points.0.2.sd.km$centers, K=25")
plot(synthetic.points.0.2.sd.km$cluster, main="plot(synthetic.points.0.2.sd.km$cluster)")


#####################################################################
# Step 3, Write a function to return SSE for a dataset and value of K

km.sse.f <- function(
  df.arg,
  centers.arg
) {
  tmp.km <- kmeans(
    x=df.arg,
    centers=centers.arg,
    iter.max=20,
    nstart=10
  )
  # Pretty sure that I should return tot.withinss rather
  # than totss. They have the same definition in the docs!
  #
  # "total within-cluster sum of squares" and
  #
  # yet thay have different values. It appears that
  # 
  #    totss = tot.withinss + betweenss
  #
  # and that totss does NOT change much with K.
  # 
  # The text calls for just "the SSE" on page 546
  # and it does not seem as if between cluster errors
  # are anywhere near as important as within cluster.
  # So I conclude that we want to return tot.withinss
  #
  tmp.km$tot.withinss
}

# sanity check function
km.sse.f(synthetic.points.0.2.sd.df, 20)

#####################################################################
# Step 4. Write single-arg functions to return SSE for a value of K
#         One function for each sd dataframe: this allows the use
#         of sapply() to generate a plot of SSE function of only K


km.sse.0.1.sd.f <- function(k) {
  # returns tot.withinss for a
  # hardcoded data frame and an
  # single argument of K. For use
  # with sapply()
  km.sse.f(
    df.arg=synthetic.points.0.1.sd.df, 
    centers.arg=k
  )
}
km.sse.0.2.sd.f <- function(k) {
  km.sse.f(
    df.arg=synthetic.points.0.2.sd.df, 
    centers.arg=k
  )
}
km.sse.0.5.sd.f <- function(k) {
  km.sse.f(
    df.arg=synthetic.points.0.5.sd.df, 
    centers.arg=k
  )
}


#####################################################################
# Step 5. Plot SSE as function of K, look for inflection point


# This method does a great job of finding a good K, near 25 as expected,
# for both the 0.1 and 0.2 standard deviation synthetic datasets. It fails
# to find a minimum for the 0.5 standard deviation dataset, but those data
# are not really clustered (just look at them!)
#
# I was initially confused since I made my plots start at K=1, which
# exagerated the peak on the left, making it hard to see the curve minimums.

plot(
  data.frame(
      k=10:50,
      sse=sapply(10:50, km.sse.0.1.sd.f)
  ), 
  main="SSE as function of K, sd = 0.1",
  sub="look for inflection point"
)
plot(
  data.frame(
      k=10:50,
      sse=sapply(10:50, km.sse.0.2.sd.f)
  ), 
  main="SSE as function of K, sd = 0.2",
  sub="look for inflection point"
)
plot(
  data.frame(
      k=10:50,
      sse=sapply(10:50, km.sse.0.5.sd.f)
  ), 
  main="SSE as function of K, sd = 0.5",
  sub="look for inflection point"
)


#####################################################################
# Step 6. use AIC method to find optimal K

# Akaike Information Criterion or AIC
# Equation 197
# Cluster cardinality in K-means
# Christopher D. Manning
# Introduction to Information Retrieval, Cambridge University Press. 2008.
# http://nlp.stanford.edu/IR-book/html/htmledition/cluster-cardinality-in-k-means-1.html

# This method does a great job of finding a good K, near 25 as expected,
# for both the 0.1 and 0.2 standard deviation synthetic datasets. It fails
# to find a minimum for the 0.5 standard deviation dataset, but those data
# are not really clustered (just look at them!)
#
# I was initially confused since I made my plots start at K=1, which
# exagerated the peak on the left, making it hard to see the curve minimums.

M = 2 # the dimensionality of the dataset

# Dataset 1: sd = 0.1
km.aic.0.1.sd.f <- function(k) {
  # returns Equation 197 for a
  # hardcoded data frame and an
  # single argument of K. For use
  # with sapply()
  km.sse.0.1.sd.f(k) + (2 * M * k)
}
plot(
  data.frame(
    k=10:50,
    eq.197=sapply(10:50, km.aic.0.1.sd.f)
  ),
  main=paste("Equation 197 as function of K, sd = 0.1, M = ", M),
  sub="look for minimum"
)


# Dataset 2: sd = 0.2
km.aic.0.2.sd.f <- function(k) {
  km.sse.0.2.sd.f(k) + (2 * M * k)
}
plot(
  data.frame(
    k=10:50,
    eq.197=sapply(10:50, km.aic.0.2.sd.f)
  ),
  main=paste("Equation 197 as function of K, sd = 0.2, M = ", M),
  sub="look for minimum"
)


# Dataset 3: sd = 0.5
km.aic.0.5.sd.f <- function(k) {
  km.sse.0.5.sd.f(k) + (2 * M * k)
}
plot(
  data.frame(
    k=10:50,
    eq.197=sapply(10:50, km.aic.0.5.sd.f)
  ),
  main=paste("Equation 197 as function of K, sd = 0.5, M = ", M),
  sub="look for minimum"
)

